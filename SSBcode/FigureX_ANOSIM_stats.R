library(tidyverse)
library(lubridate)


# Zooplankton data from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/414/1/63bfa1566eaf39762e3036b8a7906359" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
zoops.raw = read_csv(infile1)

#Only dates under ice
zoops.winter <- zoops.raw %>%
  filter(month(zoops.raw$sample_date)  < 4  | month(zoops.raw$sample_date) >= 12 )
#pivoting table to match abundance matrix 
zoop.matrix <- zoops.winter %>%
  select(sample_date, individuals_measured, species_name) %>%
  group_by(sample_date, species_name) %>%
  pivot_wider(names_from = species_name, values_from = individuals_measured, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

zoops.winter %>% group_by(sample_date, species_name) %>% tally() %>% filter(n>1)
phytos %>% group_by(sample_date, taxa_id, relative_concentration_percent) %>% tally() %>% filter(n>1)
