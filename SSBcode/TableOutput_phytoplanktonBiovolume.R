library(tidyverse)
library(vegan)
library(xtable)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('SSBcode/00_LoadData.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Numbers in results section 
# Mean biovolume by year
pp |> group_by(year4, sampledate) |> 
  summarise(biovolume_conc = sum(biovolume_conc)) |> 
  group_by(year4) |> 
  summarise(mean(biovolume_conc), sd(biovolume_conc))


# Relative abundance by year and division 
output.table = pp |> group_by(year4, sampledate, division) |> 
  summarise(relAbd = sum(relAbd)) |> 
  group_by(year4, division) |> 
  summarise(relAbd = mean(relAbd)) |> 
  pivot_wider(names_from = year4, values_from = relAbd, values_fill = 0)

print(xtable(output.table, digits = 3), include.rownames=FALSE)
