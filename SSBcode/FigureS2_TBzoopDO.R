library(lubridate)
library(tidyverse)
library(MetBrewer)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Download zooplankton data from EDI ####
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/36/c4b652eea76cd431ac5fd3562b1837ee" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
dt1 <-read_csv(infile1)

#### Download dissolved oxygen data from EDI ####
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/30/03e232a1b362900e0f059859abe8eb97" 
infile2 <- tempfile()
download.file(inUrl2,infile2,method="curl")
dt2 <-read_csv(infile2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               
# Zooplankton ID codes
codes = dt1 |> 
  group_by(species_code) |> 
  summarise(first(species_name))

zoops = dt1 |> 
  mutate(code = floor(species_code/10000)) |>
  group_by(lakeid, sample_date, code) |> 
  summarize(density = sum(density, na.rm = T)) |> 
  mutate(month = month(sample_date)) |> 
  mutate(zoopGroup = case_when(code == 1 ~ 'copepod nauplii',
                               code == 2 ~ 'copepod',
                               code == 3 ~ 'calanoid',
                               code == 4 ~ 'harpacticoid',
                               code == 5 ~ 'cladocera',
                               code == 6 ~ 'rotifer',
                               code == 7 ~ 'unknown',
                               code == 8 ~ 'unknown',
                               code == 9 ~ 'unknown'))

p1 = ggplot(zoops |> filter(lakeid == 'TB', month <= 3)) +
  geom_col(aes(x = sample_date, y = density, fill = zoopGroup), width = 100) + 
  scale_fill_manual(values = met.brewer("Archambault", 7), name = '') +
  xlim(as.Date('1981-01-01'),as.Date('2021-01-01')) +
  ylab("Zooplankton Density"~(ind~L^-1)) +
  labs(title = 'Trout Bog, Under-Ice') +
  theme_minimal(base_size = 9) +
  theme(axis.title.x = element_blank(),
        legend.margin = margin(c(0,0,0,0), unit = "cm"),
        legend.key.width = unit(0.3,"cm"),
        legend.key.height = unit(0.3,"cm"))

#### Dissolved oxygen ####
do = dt2 |> 
  filter(lakeid == 'TB') |> 
  mutate(month = month(sampledate), year = year(sampledate)) |> 
  filter(month <= 3) |> 
  group_by(year, lakeid, depth) |> 
  summarise(o2sat = min(o2sat, na.rm = T), wtemp = min(wtemp, na.rm = T))

p2 = ggplot(do |> filter(lakeid == 'TB', depth == 0)) +
  geom_col(aes(x = year, y = o2sat), width = 0.2) + 
  # scale_fill_viridis_d() +
  xlim(1981,2021) +
  ylab("Min. DO at 0 m (%)") +
  labs(title = 'Trout Bog, Under-Ice') +
  theme_minimal(base_size = 9) +
  theme(axis.title.x = element_blank())

p1 / p2 + plot_layout(widths = c(0.6,0.4)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size  = 8))

ggsave('SSBfigures/FigureSX_TBzoopDO.png', width = 6.5, height = 4, dpi = 500)  



