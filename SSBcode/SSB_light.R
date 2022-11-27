library(tidyverse)
library(xtable)
library(broom)

# PAR sensor (LiCor 192SA)
# micromolesPerMeterSquaredPerSec
# µmol/m2/sec
# Logger is an LI1400.  The surface light sensor is an LI190 sensor

# Load licor data
ssb.light = read_csv('SSBdata/SSB_licor.csv') |> 
  filter(lakeid == 'SSB') |> 
  rename(sample_date = sampledate) |>
  group_by(sample_date) |> 
  mutate(deck = mean(deck)) %>% 
  mutate(frlight = light/deck)

# Determine light extinction coefficient 
ssb.k = ssb.light %>% 
  group_by(sample_date) %>% 
  group_modify(~ broom::tidy(lm(log(frlight) ~ Depth_m, data = .x))) %>% 
  filter(term == 'Depth_m') %>% 
  mutate(extcoef = -estimate) |> 
  select(sample_date, extcoef)

ssb.light = ssb.light |> left_join(ssb.k)

# Load light data
hobo.light = read_csv('SSBdata//SSB_HoboClean.csv') |> 
  mutate(sample_date = as.Date(dateTime), hour = hour(dateTime)) |> 
  filter(hour >= 10 & hour <=14) |> 
  group_by(sample_date, Sensor, Depth_m) |> 
  summarise(Light_lumm2 = mean(Light_lumft2, na.rm = T) / 0.092903, Temp_C = mean(Temp_C)) |> 
  filter(sample_date %in% ssb.light$sample_date) |> 
  mutate(Depth_m = round(Depth_m/25, 2) * 25) #round to nearest 0.25

## Fits from Long et al. https://aslopubs.onlinelibrary.wiley.com/doi/epdf/10.4319/lom.2012.10.416
## HOBOs had average constants of
# A1 = -8165.9
# t1 = 1776.4
# y0 = 8398.2
# # par.eq = A1 * exp(-hobo/t1) + y0

ssb.light = ssb.light |>
  left_join(hobo.light) |> 
  # mutate(par.eq = A1 * exp(-Light_lumm2/t1) + y0) |> 
  filter(!is.na(Light_lumm2))

  
#### Load ice and snow data ##############
ssb.join =  read_csv('SSBdata/SSB_licor.csv')  |>  
  filter(lakeid == 'SSB') |> 
  rename(sample_date = sampledate) |> 
  left_join(read_csv('SSBdata/SSB_ice_snow_secchi.csv')) |> 
  group_by(sample_date) |> 
  filter(Depth_m == min(Depth_m)) |> 
  mutate(frlight = light/deck) |> 
  mutate(totice = totice/100, avsnow = avsnow/100,
         whiteice = whiteice/100, blackice = blackice/100) |> 
  filter(sample_date != as.Date('2021-02-17')) 

# Non-Linear Minimization to find k values
loss.2 <- function(X, df) {
  k.ice = X[1]
  # k.blackice = X[1]
  # k.whiteice = X[2]
  # k.snow = X[3]
  
  # loss = sum((exp(-k.blackice * df$blackice) * 
  #               exp(-k.whiteice * df$whiteice) - df$frlight) ^ 2, na.rm = T)
  loss = sum((exp(-k.ice * df$totice) - df$frlight) ^ 2, na.rm = T)
  return(loss)
}

# Non-Linear Minimization
ice.k = nlm(loss.2, c(0), ssb.join)

output.table = ssb.join |> select(sample_date, totice, deck, light, frlight) |> 
  mutate(k.blackice = ice.k$estimate) |> 
  left_join(ssb.k)

print(xtable(output.table), include.rownames=FALSE)
