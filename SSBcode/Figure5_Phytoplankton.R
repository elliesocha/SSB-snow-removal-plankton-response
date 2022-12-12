library(vegan)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('SSBcode/00_LoadData.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Colors and common layers ###
my.colors.2 <- c("#94C971", "#226231",
                 "#B5D6E2", "#1D3E68",
                 "#FFA17F", "#BF4904",
                 "#EBADBE", "#702539")

gglayers = list(
  theme_classic(base_size = 9),
  geom_vline(aes(xintercept = 3.5), linetype = 2),
  geom_vline(aes(xintercept = 7.5), linetype = 2),
  guides(fill = guide_legend(byrow = TRUE)),
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.height = unit(0.3, 'cm')))
    
ggplot(pp, aes(x=factor(sampledate), y=relative_total_biovolume, fill=grouping)) +
  geom_bar(position = "stack" , stat = "identity", width = 0.5) +
  scale_fill_manual(name= "Morpho-Functional\nGroupings", values = my.colors.6) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Relative total biovolume") +
  gglayers

#### Phytoplankton by genus ####
p.pp.rel <- ggplot(pp, aes(x=factor(sampledate), y=relAbd, fill=division)) +
  geom_bar(position = "stack" , stat = "identity", width = 0.5) +
  scale_fill_manual(name= "Division", values = my.colors.2) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Relative total biovolume") +
  gglayers

p.pp.tot <- ggplot(pp, aes(x=factor(sampledate), y=biovolume_conc, fill=division)) +
  geom_bar(position = "stack" , stat = "identity", width = 0.5) +
  scale_fill_manual(name= "Division", values = my.colors.2) +
  scale_y_continuous(limits = c(0, 11.5e6), expand = c(0,0)) +
  ylab(expression(paste("Biovolume ", "(",µm^3," ",mL^-1,")"))) +
  gglayers

#### Phytoplankton groupings ####
my.colors.6 <- c("#ECD698", "#4E4019",
                 "#26B7AE", "#0D5C57",
                 "#7FDF8F")

p.pg.tot = ggplot(pp, aes(x=factor(sampledate), y=biovolume_conc, fill=grouping)) +
  geom_bar(position = "stack" , stat = "identity", width = 0.5) +
  scale_fill_manual(name= "Morpho-Functional\nGroupings", values = my.colors.6) +
  scale_y_continuous(limits = c(0, 11.5e6), expand = c(0,0)) +
  ylab(expression(paste("Biovolume ", "(",µm^3," ",mL^-1,")"))) +
  gglayers
p.pg.tot

p.pg.rel = ggplot(pp, aes(x=factor(sampledate), y=relative_total_biovolume, fill=grouping)) +
  geom_bar(position = "stack" , stat = "identity", width = 0.5) +
  scale_fill_manual(name= "Morpho-Functional\nGroupings", values = my.colors.6) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Relative total biovolume") +
  gglayers
p.pg.rel

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Combo Plot ###
layout <- 
  'AB
CD'

p.pp.tot + p.pp.rel + p.pg.tot + p.pg.rel +
  plot_layout(guides = "collect", design = layout) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size  = 8))

ggsave('SSBfigures/Figure5_Phytoplankton.png', width = 6.5, height = 5, dpi = 500) 

