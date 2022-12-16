library(tidyverse)
library(dplyr)
library(lubridate)
library(vegan)

set.seed(12)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('SSBcode/00_LoadData.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ PHYTOPLANKTON ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ANOSIM for different parameters and groupings
anosim.pp <- function(parameter, usegroup, by = 'year', name1, name2) {
  SSB2 <- pp %>% 
    group_by(sampledate) |> 
    mutate(totalCells = sum(nu_per_ml)) |> 
    ungroup() |> 
    mutate(relNu = nu_per_ml/totalCells) |> 
    select(any_of(c('sampledate', usegroup, parameter))) %>%
    group_by(sampledate,  .data[[usegroup]]) %>%
    summarize_at(vars(parameter), funs(tot = sum)) %>% 
    pivot_wider(names_from = usegroup, values_from = tot, values_fill = 0) %>%
    ungroup()
  
  SSB3 = SSB2 |> 
    select(-sampledate)
  #dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
  SSB3.group = SSB2 |> 
    select(sampledate) |> 
    mutate(year = year(sampledate)) |> 
    mutate(light = case_when(year <= 2020 ~ 'low',
                             year == 2021 ~ 'high'))
  
  #ANOSIM
  p.dist2 <- vegdist(SSB3)
  p.ano2 <- with(SSB3.group, anosim(p.dist2, get(by)))
  # print(summary(p.ano2))
  print(paste0(name1, ', ', name2, ', p = ',p.ano2$signif, ', r2 = ', round(p.ano2$statistic,2)))
  # return(SSB2)
}

pp1 = anosim.pp(parameter = 'biovolume_conc', usegroup = 'division', by = 'year',
                name1 = 'Phytoplankton Biovolume', name2 = 'Division')
pp2 = anosim.pp(parameter = 'biovolume_conc', usegroup = 'grouping', by = 'year',
                name1 = 'Phytoplankton Biovolume', name2 = 'Morpho-Functional Groupings')

pp3 = anosim.pp(parameter = 'relAbd', usegroup = 'division', by = 'year',
                name1 = 'Phytoplankton Relative Biovolume', name2 = 'Division')
pp4 = anosim.pp(parameter = 'relAbd', usegroup = 'grouping', by = 'year',
                name1 = 'Phytoplankton Relative Biovolume', name2 = 'Morpho-Functional Groupings')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ ZOOPLANKTON ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ANOSIM for different parameters and groupings
anosim.zoops <- function(parameter, usegroup, by = 'year', name1, name2) {
  SSB2 <- zoops %>% 
    select(any_of(c('sample_date', usegroup, parameter))) %>%
    group_by(sample_date,  .data[[usegroup]]) %>%
    summarize_at(vars(parameter), funs(tot = sum)) %>% 
    pivot_wider(names_from = usegroup, values_from = tot, values_fill = 0) %>%
    ungroup()
  
  SSB3 = SSB2 |> 
    select(-sample_date)
  #dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
  SSB3.group = SSB2 |> 
    select(sample_date) |> 
    mutate(year = year(sample_date)) |> 
    mutate(light = case_when(year <= 2020 ~ 'low',
                             year == 2021 ~ 'high'))
  
  #ANOSIM
  p.dist2 <- vegdist(SSB3, method = 'bray')
  p.ano2 <- with(SSB3.group, anosim(p.dist2, get(by)))
  
  # print(summary(p.ano2))
  print(paste0(name1, ', ', name2,
    ', p = ',p.ano2$signif, ', r2 = ', round(p.ano2$statistic,2)))
  # return(SSB2)
}


z1 = anosim.zoops(parameter = 'density', usegroup = 'genus', name1 = 'Zooplankton Total Density', name2 = 'Genus')
z2 = anosim.zoops(parameter = 'density', usegroup = 'trophi_grouping', name1 = 'Zooplankton Total Density', name2 = 'Trophi Groupings')

z3 = anosim.zoops(parameter = 'relAbd', usegroup = 'genus', name1 = 'Zooplankton Relative Abundance', name2 = 'Trophi Genus')
z4 = anosim.zoops(parameter = 'relAbd', usegroup = 'trophi_grouping', name1 = 'Zooplankton Relative Abundance', name2 = 'Trophi Groupings')


out = rbind(pp1, pp2, pp3, pp4, z1, z2, z3, z4)
out.table = data.table::fread(text = paste0(out, collapse = "\n"), header = FALSE)

print(xtable(out.table), include.rownames=FALSE)


