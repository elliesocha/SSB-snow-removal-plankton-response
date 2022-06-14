library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

# Ice on and off dates
# Ice off/on 2018-11-09 2019-04-13
# Ice off/on 2019-11-06 2020-04-24
# Ice off/on 2020-11-16 2021-04-05

# Load temperature/light data
light_all = read_csv('SSBdata//SSB_HoboClean.csv') |> 
  mutate(Light_lumm2 = Light_lumft2*0.092903) |> 
  mutate(group = case_when(dateTime >= as.Date('2018-11-09') & dateTime <= as.Date('2019-04-13') ~ '2019: Reference Yr',
                          dateTime >= as.Date('2019-11-06') & dateTime <= as.Date('2020-04-24') ~ '2020: Manipulation Yr1',
                          dateTime >= as.Date('2020-11-16') & dateTime <= as.Date('2021-04-05') ~ '2021: Manipulation Yr2')) |> 
  filter(!is.na(group)) |> 
  mutate(date = as.Date(dateTime))

ltw.day = light_all |> 
  group_by(date, Sensor, group, Depth_m) |> 
  summarise(Temp_C_mean = mean(Temp_C), Temp_C_max = max(Temp_C), 
            Temp_C_min = min(Temp_C), light_max = max(Light_lumm2)) |> 
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
  geom_col(data = ltw.day |> filter(group == "2021: Manipulation Yr2", Sensor == s), aes(x = useDate, y = light_max, fill = '2021: Manipulation Yr2')) +
  geom_col(data = ltw.day |> filter(group == "2020: Manipulation Yr1", Sensor == s), aes(x = useDate, y = light_max, fill = '2020: Manipulation Yr1')) +
  geom_col(data = ltw.day |> filter(group == "2019: Reference Yr", Sensor == s), aes(x = useDate, y = light_max, fill = '2019: Reference Yr')) +
  ylab("Light Intensity"~(lum~m^-2)) +
  scale_fill_manual(values = c("2019: Reference Yr" = "black",
                               "2020: Manipulation Yr1" = "#f5562a",
                               "2021: Manipulation Yr2" = "#f5d02a"),
                    name = "") +
  scale_x_date(labels = date_format("%b")) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_line(size = rel(0.3)),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = c(0.3,0.8),
        legend.title = element_blank()) 
  
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
        legend.position = c(0.3,0.8),
        legend.title = element_blank())

#### Combine plots #####
p.light / p.do +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size  = 8))
ggsave('SSBfigures/Figure4_Buoy.png', width = 3.25, height = 4, dpi = 500)  


