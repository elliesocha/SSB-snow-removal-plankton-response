library(tidyverse)
library(lubridate)
library(patchwork)
library(ggpattern)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('SSBcode/00_LoadData.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Ice Thickness ####
p.ice = ggplot() +
  geom_hline(aes(yintercept = 0), size = 0.3) +
  geom_bar(data = icesnow, aes(x = factor(sample_date), y = thickness, fill = icetype), 
           stat = 'identity', width=0.5, color = 'black', size = 0.2) + 
  scale_y_reverse(labels = abs) +
  scale_fill_manual(values = c('#404040','#E0E0E0','lightblue3'), name = '') +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(y = "Thickness (cm)") +
  annotate(geom = 'rect',xmin = 0.4, xmax = 3.5, ymin = -19, ymax = -Inf, fill = 'lightblue1', alpha = 1) +
  annotate(geom = 'rect', xmin = 3.5, xmax = Inf, ymin = -19, ymax = -Inf, fill = 'lightblue2', alpha = 1) +
  geom_vline(aes(xintercept = 3.5), linetype = 2) +
  geom_vline(aes(xintercept = 7.5), linetype = 2) +
  annotate("text", x = 2, y = -23, label = "Reference Year", size = 9/.pt, hjust = 0.5) +
  annotate("text", x = 5.5, y = -23, label = "Manipulation Year 1", size = 9/.pt) +
  annotate("text", x = 10, y = -23, label = "Manipulation Year 2", size = 9/.pt) +
  theme_bw(base_size = 9) +
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1),
        # panel.grid.minor = element_line(size = rel(0.2)),
        panel.grid = element_line(size = rel(0.3)),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin = margin(c(0,0,0,0), unit = "cm")); p.ice

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Secchi ####
p.secchi = ggplot() +
  # geom_col(data = ice, aes(x = factor(sample_date), 
  #                          y = secchi), width=0.3, fill = '#542d1d') + 
  geom_col_pattern(data = ice, aes(x = factor(sample_date), y = secchi), width=0.3, color = 'black', size = 0.2,
                   fill = '#f0ee8b', pattern_colour = 'black', pattern = 'stripe', pattern_fill = 'grey80',
                   pattern_size = 0.1, pattern_spacing = 0.1) +
  
  theme_bw(base_size = 9) +
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_reverse(labels = abs, expand = c(0,0), limits = c(105,0)) +
  labs(y = "Secchi (cm)") +
  geom_vline(aes(xintercept = 3.5), linetype = 2) +
  geom_vline(aes(xintercept = 7.5), linetype = 2) +
  theme(axis.text.x = element_blank(),
        panel.grid = element_line(size = rel(0.3)),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin = margin(c(0,0,0,0), unit = "cm")); p.secchi

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### chlorophyll ####
winterchlorophyll = chlorophyll |> filter(month(sample_date) < 4 | month(sample_date) >= 12)

chla <- winterchlorophyll |> 
  filter(depth == 0) |> 
  mutate(group = 'Surface') |> 
  select(sample_date, chlorophyll_ug_L, group)

chla.mean = winterchlorophyll |> 
  group_by(sample_date) |> 
  summarise(chlorophyll_ug_L = mean(chlorophyll_ug_L, na.rm = T)) |> 
  mutate(group = 'Water\nColumn') |> 
  bind_rows(chla)

p.chl = ggplot(chla.mean) +
  geom_point(data = winterchlorophyll, 
             aes(x = factor(sample_date), y= chlorophyll_ug_L), size = 2, shape = '—', alpha = 0.7) +
  geom_point(aes(x=factor(sample_date), y= chlorophyll_ug_L, fill = group), 
             shape = 21, size = 2) +
  scale_y_continuous(limits = c(0,270)) +
  scale_fill_manual(values = c('#8ed1a1', '#1f4d2c'), name = "") +
  ylab("Chl-a"~(µg~L^-1)) +
  geom_vline(aes(xintercept = 3.5), linetype = 2) +
  geom_vline(aes(xintercept = 7.5), linetype = 2) +
  theme_bw(base_size = 9) +
  theme(#axis.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_line(size = rel(0.3)),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin = margin(c(0,0,0,0), unit = "cm"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combo plot
p.ice / p.secchi / p.chl + 
  plot_layout(heights = c(2,1,2)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size  = 8))

ggsave(filename ="SSBfigures/Figure2_Ice.png", plot = last_plot(), 
       height = 5.5, width = 6.5, units = "in", dpi = 500)

