library(tidyverse)
library(lubridate)
library(dplyr)
library(vegan)

# Zooplankton data from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/414/1/63bfa1566eaf39762e3036b8a7906359" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
zoops.raw = read_csv(infile1)

#Only dates under ice and combining and combining duplicate "Unknown rotifer" counts (from 2020-01-31 and 2021-03-15) to make matrix
zoops.winter <- zoops.raw %>%
  filter(month(zoops.raw$sample_date)  < 4  | month(zoops.raw$sample_date) >= 12 ) %>%
  group_by(sample_date, species_name) %>% summarise(individuals_measured = sum(individuals_measured))

#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df <- zoops.raw %>%
  filter(month(zoops.raw$sample_date)  < 4  | month(zoops.raw$sample_date) >= 12 ) %>%
  select(year, sample_date) %>%
  distinct(year, sample_date, .keep_all = TRUE)

#pivoting table to match abundance matrix 
zoop.pivoted <- zoops.winter %>%
  select(sample_date, individuals_measured, species_name) %>%
  group_by(sample_date, species_name) %>%
  pivot_wider(names_from = species_name, values_from = individuals_measured, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#converting zoop.pivoted df into matrix
zoop.matrix = as.matrix(zoop.pivoted)

#ANOSIM
ano = anosim(zoop.matrix, group.df$year, distance = "bray", permutations = 9999)
ano

#~~~~~~~~~~~~~~~
#Different anosim using a dissimilarity matrix
data(zoop.pivoted)
data(group.df)
zoop.dist <- vegdist(zoop.pivoted)
zoop.ano <- with(group.df, anosim(zoop.dist, year))
summary(zoop.ano)
plot(zoop.ano)

