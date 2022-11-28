## Table 1. Light transmittance and attenuation coefficients (KD) of ice and the water column on
# South Sparkling Bog in 2021. A single ice KD was calculated for all dates

#### Load ice and snow data ##############
ssb.join =  ssb.light |> 
  left_join(read_csv('SSBdata/SSB_ice_snow_secchi.csv')) |> 
  group_by(sample_date) |> 
  filter(Depth_m == min(Depth_m)) |> 
  mutate(totice = totice/100, avsnow = avsnow/100,
         whiteice = whiteice/100, blackice = blackice/100) |> 
  filter(sample_date != as.Date('2021-02-17')) 

# Non-Linear Minimization to find k values
loss.2 <- function(X, df) {
  k.ice = X[1]
  loss = sum((exp(-k.ice * df$totice) - df$frlight) ^ 2, na.rm = T)
  return(loss)
}

# Non-Linear Minimization
ice.k = nlm(loss.2, c(0), ssb.join)

output.table = ssb.join |> select(sample_date, totice, deck, light, frlight) |> 
  mutate(k.blackice = ice.k$estimate) |> 
  left_join(ssb.k)

print(xtable(output.table), include.rownames=FALSE)
