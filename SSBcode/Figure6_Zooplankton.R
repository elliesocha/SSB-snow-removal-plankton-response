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
my.colors <- c("#94C971", "#226231", "#B5D6E2", "#1D3E68", "#FFA17F", "#BF4904", "#EBADBE", "#702539", "#C4AED0", "#8A629E", "#D23131", "#EFD556", "#B99A00")

gglayers = list(
  theme_classic(base_size = 9),
  geom_vline(aes(xintercept = 3.5), linetype = 2),
  geom_vline(aes(xintercept = 6.5), linetype = 2),
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1),
        legend.key.height = unit(0.3, 'cm')))

#### Zooplankton by genus ####
p.zoops.rel <- 
  ggplot(zoops, aes(x=factor(sample_date), y=relAbd, fill=genus)) +
  geom_bar(position = "stack" , stat = "identity", width = 0.5) +
  scale_fill_manual(name= "Genus", values = my.colors) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Relative abundance") +
  gglayers

p.zoops.tot <- 
  ggplot(zoops, aes(x=factor(sample_date), y=density, fill=genus)) +
  geom_bar(position = "stack" , stat = "identity", width = 0.5) +
  scale_fill_manual(name= "Genus", values = my.colors) +
  scale_y_continuous(limits = c(0, 72), expand = c(0,0)) +
  ylab(expression(paste("Density ", "(",'Individuals ', L^-1,")"))) +
  gglayers

#### Zooplankton groupings ####
my.colors.2 <- c("#94C971", "#226231",
                 "#FFA17F",
                 "#B5D6E2", "#1D3E68",
                 "#FFA17F", "#BF4904")

p.zg.tot = ggplot(zoops, aes(x=factor(sample_date), y=density, fill=trophi_grouping)) +
  geom_bar(position = "stack" , stat = "identity", width = 0.5) +
  scale_fill_manual(name= "Trophi Groupings", values = my.colors.2) +
  scale_y_continuous(limits = c(0, 72), expand = c(0,0)) +
  ylab(expression(paste("Density ", "(",'Individuals ', L^-1,")"))) +
  gglayers

p.zg.rel = ggplot(zoops, aes(x=factor(sample_date), y=relAbd, fill=trophi_grouping)) +
  geom_bar(position = "stack" , stat = "identity", width = 0.5) +
  scale_fill_manual(name= "Trophi Groupings", values = my.colors.2) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Relative abundance") +
  gglayers

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Combo Plot ###
layout <- 
'AB
CD'

p.zoops.tot + p.zoops.rel + p.zg.tot + p.zg.rel +
  plot_layout(guides = "collect", design = layout) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size  = 8))

ggsave('SSBfigures/Figure6_Zooplankton.png', width = 6.5, height = 5, dpi = 500) 
 
