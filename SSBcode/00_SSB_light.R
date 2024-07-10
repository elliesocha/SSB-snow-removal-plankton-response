library(tidyverse)
library(xtable)
library(broom)

# PAR sensor (LiCor 192SA)
# micromolesPerMeterSquaredPerSec
# µmol/m2/sec
# Logger is an LI1400.  The surface light sensor is an LI190 sensor

# Load licor data
ssb.PAR = read_csv('SSBdata/SSB_licor.csv') |> 
  filter(lakeid == 'SSB') |> 
  rename(sample_date = sampledate) |>
  group_by(sample_date) |> 
  mutate(deck = mean(deck)) %>% 
  mutate(frlight = light/deck)

# Determine light extinction coefficient add to data.frame 
ssb.k = ssb.PAR %>% 
  group_by(sample_date) %>% 
  group_modify(~ broom::tidy(lm(log(frlight) ~ Depth_m, data = .x))) %>% 
  filter(term == 'Depth_m') %>% 
  mutate(extcoef = -estimate) |> 
  select(sample_date, extcoef)

ssb.PAR = ssb.PAR |> left_join(ssb.k)

# Load light data
hobo.light = read_csv('SSBdata//SSB_HoboClean.csv') |> 
  mutate(sample_date = as.Date(dateTime), hour = hour(dateTime)) |> 
  filter(hour >= 10 & hour <=14) |> 
  group_by(sample_date, Sensor, Depth_m) |> 
  summarise(Light_lumm2 = mean(Light_lumft2, na.rm = T) / 0.092903, Temp_C = mean(Temp_C)) |> 
  filter(sample_date %in% ssb.PAR$sample_date) |> 
  mutate(Depth_m = round(Depth_m/25, 2) * 25) #round to nearest 0.25

ssb.par.hobo = ssb.PAR |>
  left_join(hobo.light) |> 
  # mutate(par.eq = A1 * exp(-Light_lumm2/t1) + y0) |> 
  filter(!is.na(Light_lumm2))

## PAR equation 
ssb.light.filter = ssb.par.hobo |> 
  filter(Depth_m < 2) |> 
  filter(sample_date != as.Date('2021-02-17'))

modelPARFit = lm(log10(light) ~ log10(Light_lumm2), data = ssb.light.filter)

### HOBO.PAR conversion
hobo.light.PAR = read_csv('SSBdata//SSB_HoboClean.csv') |> 
  mutate(sample_date = as.Date(dateTime), hour = hour(dateTime)) |> 
  mutate(Light_lumm2 = Light_lumft2 / 0.092903) |> 
  mutate(PAR.est = summary(modelPARFit)$coefficients[2,1] * log10(Light_lumm2) + summary(modelPARFit)$coefficients[1,1]) |> 
  mutate(PAR.est = 10^PAR.est)
  
# PAR stats 
hobo.light.PAR |> 
  group_by(date = as.Date(dateTime)) |> 
  summarise(PAR = max(PAR.est), light = mean(Light_lumm2)) |> 
  mutate(month = month(date)) |> 
  filter(month %in% c(1:3)) |> 
  mutate(month = if_else(month == 1, 2, month)) |>
  group_by(year(date), month) |> 
  summarise(mean(PAR), mean(light)) |> 
  arrange(month)

