### Data load ####
library(tidyverse)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data for ice, snow, secchi, and chlorophyll
# Subset to under-ice measurements
ice = read_csv('SSBdata/SSB_ice_snow_secchi.csv') |> 
  filter(month(sample_date) < 4 | month(sample_date) >= 12)
chlorophyll = read_csv('SSBdata/SSB_chlorophyll.csv')

# Rearrange for plotting
icesnow <- ice %>%
  mutate(Snow = -abs(avsnow)) %>%
  select(-avsnow, -secchi) %>%
  pivot_longer(cols = whiteice:Snow, names_to = 'icetype', values_to = 'thickness') |> 
  mutate_at("icetype", str_replace, "whiteice", "White ice") %>%
  mutate_at("icetype", str_replace, "blackice", "Black ice") |> 
  mutate(icetype = factor(icetype, levels = c('Black ice','White ice', 'Snow')))  #('Snow','White ice','Black ice')))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Buoy
lightSurf = read_csv('SSBdata//SSB_HoboClean.csv') |> 
  mutate(sample_date = as.Date(dateTime), hour = hour(dateTime)) |> 
  filter(hour >= 10 & hour <=14) |> 
  group_by(sample_date, Sensor, Depth_m) |> 
  summarise(Light_lumm2 = mean(Light_lumft2, na.rm = T) / 0.092903, Temp_C = mean(Temp_C)) |> 
  filter(Sensor == 1) |> 
  filter(sample_date %in% ice$sample_date)

# Join data
env.vars = ice |> left_join(chlorophyll |> filter(depth == 0)) |> left_join(lightSurf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Phytoplankton data 
pp.raw = read_csv('SSBdata/SSB_phytoplankton.csv') |> 
  filter(sampledate != as.Date('2019-03-06')) 

# Crosswalk for grouping data
phyto.cross = read_csv('SSBdata/SSB_phytoplankton_morphogroup_crosswalk.csv') |> 
  select(taxa_id, morpho_func_group) |> 
  group_by(taxa_id) |> 
  summarise(morpho_func_group = first(morpho_func_group))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Removing sample dates during open water
pp = pp.raw |>
  filter(month(sampledate) < 4 | month(sampledate) >= 12) |> 
  group_by(sampledate) |> 
  mutate(totalbiovol = sum(biovolume_conc)) |> 
  ungroup() |> 
  mutate(relAbd = biovolume_conc/totalbiovol) |> 
  left_join(phyto.cross) |> 
  mutate(grouping = case_when(morpho_func_group == 1 ~ 'small\nmixotrophs',
                              morpho_func_group == 2 ~ 'large\nmixotrophs',
                              morpho_func_group == 4 ~ 'unicellular\ncyanobacteria',
                              morpho_func_group == 5 ~ 'colonial\ncyanobacteria',
                              morpho_func_group == 8 ~ 'chlorophytes\n/other')) %>%
  mutate(grouping = factor(grouping, levels = c('small\nmixotrophs','large\nmixotrophs', 
                                                'unicellular\ncyanobacteria', 'colonial\ncyanobacteria', 
                                                'chlorophytes\n/other' )))  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zooplankton data from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/414/1/63bfa1566eaf39762e3036b8a7906359" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
zoops.raw = read_csv(infile1)

# Crosswalk for grouping data
zoop.cross = read_csv('SSBdata/SSB_zooplankton_group_crosswalk.csv')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Removing sample dates during open water
zoops = zoops.raw |>
  filter(month(sample_date) < 4 | month(sample_date) >= 12) |> 
  group_by(sample_date) |> 
  mutate(totalind = sum(individuals_measured)) |> 
  ungroup() |> 
  mutate(relAbd = individuals_measured/totalind) |> 
  mutate(genus = case_when(str_detect(species_name, 'Unknown') ~ species_name, # extract genus name
                           TRUE ~ word(species_name, 1))) |> 
  mutate(genus = if_else(genus == 'Keratell', 'Keratella', genus)) |> 
  left_join(zoop.cross) |> 
  mutate(sample_date = if_else(sample_date == as.Date('2019-01-18'), as.Date('2019-01-08'), sample_date))

