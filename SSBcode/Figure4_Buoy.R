#Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

# Ice on and off dates
# Ice off/on 2018-11-09 2019-04-13
# Ice off/on 2019-11-06 2020-04-24
# Ice off/on 2020-11-16 2021-04-05

##### HOBO vs Licor plot ######
ssb.light.filter = ssb.par.hobo |> 
  filter(Depth_m < 2) |> 
  filter(sample_date != as.Date('2021-02-17'))

p.light.compare = ggplot(ssb.light.filter) + 
  geom_smooth(aes(x = Light_lumm2, y = light), method = 'lm', color = 'lightblue4', size = 0.5, alpha = 0.3) +
  geom_point(aes(x = Light_lumm2, y = light, group = Depth_m), shape = 21, stroke = 0.2, fill = 'lightblue4') +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Light Intensity"~(lum~m^-2)) +
  ylab("PAR"~(µmol~m^-2~s^-1)) +
  theme_bw(base_size = 9) +
  theme(panel.grid = element_line(size = rel(0.3)))

# Load temperature/light data
light_all = read_csv('SSBdata//SSB_HoboClean.csv') |> 
  mutate(Light_lumm2 = Light_lumft2 / 0.092903) |> 
  mutate(group = case_when(dateTime >= as.Date('2018-11-09') & dateTime <= as.Date('2019-04-13') ~ '2019: Reference Yr',
                          dateTime >= as.Date('2019-11-06') & dateTime <= as.Date('2020-04-24') ~ '2020: Manipulation Yr1',
                          dateTime >= as.Date('2020-11-16') & dateTime <= as.Date('2021-04-05') ~ '2021: Manipulation Yr2')) |> 
  filter(!is.na(group)) |> 
  mutate(date = as.Date(dateTime)) |> 
  mutate(PAR.est = summary(modelPARFit)$coefficients[2,1] * log10(Light_lumm2) + summary(modelPARFit)$coefficients[1,1]) |> 
  mutate(PAR.est = 10^PAR.est)

ltw.day = light_all |> 
  group_by(date, Sensor, group, Depth_m) |> 
  summarise(Temp_C_mean = mean(Temp_C), Temp_C_max = max(Temp_C), Temp_C_min = min(Temp_C), 
            light_max = max(Light_lumm2),  light_mean = mean(Light_lumm2), 
            PAR.est_max = max(PAR.est), PAR.est_mean = mean(PAR.est)) |> 
  mutate(useDate = if_else(month(date) >= 10, `year<-`(date, 1999), `year<-`(date, 2000)))

# Load oxygen data
ssb.do.raw = read_csv('SSBdata/SSB_DObuoy.csv') |> 
  mutate(date = as.Date(DateTime_CST)) |> 
  mutate(group = case_when(date >= as.Date('2018-11-09') & date <= as.Date('2019-04-13') ~ '2019: Reference Yr',
                           date >= as.Date('2019-11-06') & date <= as.Date('2020-04-24') ~ '2020: Manipulation Yr1',
                           date >= as.Date('2020-11-16') & date <= as.Date('2021-04-05') ~ '2021: Manipulation Yr2')) 

ssb.do.day = ssb.do.raw |> 
  filter(!is.na(group), !is.na(DO_mgL)) |> 
  group_by(date, group) |> 
  summarise(DO_mgL_mean = max(DO_mgL, na.rm = T),
            DO_sat_mean = max(DO_sat, na.rm = T)) |> 
  mutate(useDate = if_else(month(date) >= 10, `year<-`(date, 1999), `year<-`(date, 2000)))

##### Light Intensity Plots ######
s = 1
p.light = ggplot(ltw.day) +
  geom_hline(aes(yintercept = 7.6), linetype = 2, linewidth = 0.2) +
  geom_hline(aes(yintercept = 20), linetype = 2, linewidth = 0.2) +
  geom_col(data = ltw.day |> filter(group == "2021: Manipulation Yr2", Sensor == s), aes(x = useDate, y = PAR.est_max, fill = '2021: Manipulation Yr2')) +
  geom_path(data = ltw.day |> filter(group == "2021: Manipulation Yr2", Sensor == s), aes(x = useDate, y = PAR.est_mean, col = '2021: Manipulation Yr2')) +
  geom_col(data = ltw.day |> filter(group == "2020: Manipulation Yr1", Sensor == s), aes(x = useDate, y = PAR.est_max, fill = '2020: Manipulation Yr1')) +
  geom_path(data = ltw.day |> filter(group == "2020: Manipulation Yr1", Sensor == s), aes(x = useDate, y = PAR.est_mean, col = '2020: Manipulation Yr1')) +
  geom_col(data = ltw.day |> filter(group == "2019: Reference Yr", Sensor == s), aes(x = useDate, y = PAR.est_max, fill = '2019: Reference Yr')) +
  # ylab("Light Intensity"~(lum~m^-2)) +
  ylab("PAR"~(µmol~m^-2~s^-1)) +
  scale_fill_manual(values = c("2019: Reference Yr" = "black",
                               "2020: Manipulation Yr1" = "#f5562a",
                               "2021: Manipulation Yr2" = "#f5d02a"),
                    name = "Max. Daily PAR") +
  scale_color_manual(values = c("2019: Reference Yr" = "black",
                               "2020: Manipulation Yr1" = "#b33c1b",
                               "2021: Manipulation Yr2" = "#c4a61f"),
                    name = "Mean Daily PAR") +
  scale_x_date(labels = date_format("%b")) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_line(size = rel(0.3)),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = c(0.3,0.75),
        legend.title = element_blank(),
        ) +
  guides(color = 'none'); p.light
  
##### Dissolved oxygen Plots ######
p.do = ggplot(ssb.do.day) +
  geom_line(data = ssb.do.day |> filter(group == "2021: Manipulation Yr2"), 
              aes(x = useDate, y = DO_sat_mean, col = '2021: Manipulation Yr2'), alpha = 0.9) +
  geom_line(data = ssb.do.day |> filter(group == "2020: Manipulation Yr1"), 
              aes(x = useDate, y = DO_sat_mean, col = '2020: Manipulation Yr1'), alpha = 0.9) +
  geom_line(data = ssb.do.day |> filter(group == "2019: Reference Yr"), 
              aes(x = useDate, y = DO_sat_mean, col = '2019: Reference Yr'), alpha = 0.9) +
  ylab("Dissolved Oxygen (%)") +
  scale_color_manual(values = c("2019: Reference Yr" = "black",
                               "2020: Manipulation Yr1" = "#f5562a",
                               "2021: Manipulation Yr2" = "#f5d02a"),
                    name = "") +
  scale_x_date(labels = date_format("%b")) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_line(size = rel(0.3)),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = c(0.3,0.75),
        legend.title = element_blank())


#### Combine plots #####
p.light.compare / p.light / p.do +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size  = 8))

ggsave('SSBfigures/Figure4_Buoy.png', width = 3.25, height = 5.5, dpi = 500)  


