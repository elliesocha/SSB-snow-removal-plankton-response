# Load packages
library(vegan)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)

set.seed(12)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('SSBcode/00_LoadData.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

useVars = env.vars |> 
  mutate(PAR.est = log10(PAR.est + 0.001)) |> 
  select(avsnow, totice, whiteice, blackice, secchi, chlorophyll_ug_L, PAR.est) |> 
  setNames(c('Snow','Total Ice','White Ice',
             'Black Ice','Secchi','Chl-a 0 m','PAR 0.7 m'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Group phytoplankton division ####
df.division = pp |> arrange(sampledate) |> 
  group_by(lakeid, sampledate, division) |> 
  summarise(total = sum(cells_per_ml, na.rm = T)) 

total = pp |> arrange(sampledate) |> 
  group_by(lakeid, sampledate) |> 
  summarise(total = sum(biovolume_conc, na.rm = T)) 

df.division.wide = df.division |> 
  mutate(total = log10(total)) |> 
  pivot_wider(names_from = division,
              values_from = total,
              values_fill = 0) |>
  ungroup() |> 
  select(-lakeid, - sampledate) |> 
  select(-Miscellaneous, -Haptophyta, -Pyrrhophyta, -Euglenophyta, -Cryptophyta) |> 
  # select(Chlorophyta, Cyanophyta)
  bind_cols(PP.Biovolume = log10(total$total))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rda_phyto <- rda(df.division.wide ~ 
                   `PAR 0.7 m` + `Black Ice`  + `White Ice` + Secchi ,
                 data = useVars, na.action = 'na.exclude') 

# Testing the significance of the rda model:
anova.cca(rda_phyto)
# Testing the significance of terms (environmental variables):
anova.cca(rda_phyto, by="terms")
# Testing the significance of rda axes (at least the first two or three should present a significant p value):
anova.cca(rda_phyto, by="axis")

RsquareAdj(rda_phyto)

#Get rda scores
df_species  <- data.frame(summary(rda_phyto)$species[,1:2]) |> 
  mutate(label = rownames(summary(rda_phyto)$species))# get the species CC1 and CC2 scores
df_environ  <- data.frame(scores(rda_phyto, display = 'bp')) #get the environment vars CC1 and CC2 scores
rownames(df_environ) = gsub(pattern = '`',replacement = '', rownames(df_environ))
RDA1_varex <- round(summary(rda_phyto)$cont$importance[2,1]*100,2) #Get percentage of variance explained by first axis
RDA2_varex <- round(summary(rda_phyto)$cont$importance[2,2]*100,2) #Get percentage of variance explained by second axis
# rda_labels = names(df.division.wide)
# Set a scaling variable to multiply the rda values, in order to get a very similar plot to the the one generated by plot(rda_model). 
# Adjust it according to your data
scaling_factor <- 2

# rda PLOT
my.colors <- c("#94C971", 
               "#226231",
               # "#B5D6E2", 
               "#1D3E68",
               # "#FFA17F", 
               # "#BF4904", 
               # "#EBADBE",
               # "#702539"
               'black'
               )

rda.plot1 = ggplot(df_species, aes(x=RDA1, y=RDA2)) + 
  geom_hline(yintercept=0, linetype="dashed", size = 0.3) +
  geom_vline(xintercept=0, linetype="dashed", size = 0.3) +
  #Add environmental vars arrows
  geom_segment(data = df_environ, 
               aes(x=0, #Starting coordinate in RDA1 = 0 
                   xend = RDA1*scaling_factor,#Ending coordinate in RDA1  
                   y=0, #Start in RDA2 = 0
                   yend = RDA2*scaling_factor), #Ending coordinate in RDA2 
               color = "lightblue4", #set color
               arrow = arrow(length=unit(0.02,"npc"))) + #Set the size of the lines that form the tip of the arrow
  #Add species text
  geom_point(data = df_species, aes(x=RDA1, y = RDA2, fill = label), size = 3, shape = 21, alpha = 0.8, stroke = 0.2) +
  scale_fill_manual(values = my.colors, name = 'Division') +
  #Add environmental vars text
  geom_label_repel(data = df_environ, 
                   aes(x = RDA1*scaling_factor, y = RDA2*scaling_factor),
                   label = rownames(df_environ),
                   label.padding = 0.1,
                   nudge_y = 0.55, color="lightblue4", fill = alpha("white", 0.9),
                   segment.size = 0.2, size = 2.5) +
  #Set x and y axis titles
  labs(x=paste0("RDA1 (",RDA1_varex," %)"),
       y=paste0("RDA2 (",RDA2_varex," %)"),
       title = 'Phytoplankton') +
  #Set bw theme
  theme_bw(base_size = 9) +
  theme(legend.text = element_text(size = 7), 
        plot.title = element_text(size = 8, color = '#bfac17', face = 2, margin=margin(0,0,1,0)),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin = margin(c(0,0,0,0), unit = "cm"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.15,"cm")); rda.plot1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Group by morpho group ####
df.group = pp |> arrange(sampledate) |> 
  group_by(lakeid, sampledate, grouping) |> 
  summarise(total = sum(cells_per_ml, na.rm = T))

df.group.wide = df.group |> 
  mutate(total = log10(total)) |> 
  pivot_wider(names_from = grouping,
              values_from = total,
              values_fill = 0) |>
  ungroup() |> 
  select(-lakeid, - sampledate) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rda_phyto.group <- rda(df.group.wide ~ 
                         `PAR 0.7 m` +`Black Ice`  + `White Ice` + Secchi,
                       data = useVars, na.action = 'na.exclude') 

# Testing the significance of the rda model:
anova.cca(rda_phyto.group, model = 'direct')
# Testing the significance of terms (environmental variables):
anova.cca(rda_phyto.group, by="terms")
# Testing the significance of rda axes (at least the first two or three should present a significant p value):
anova.cca(rda_phyto.group, by="axis")
RsquareAdj(rda_phyto.group)

#Get rda scores
df_species  <- data.frame(summary(rda_phyto.group)$species[,1:2]) |> 
  mutate(label = rownames(summary(rda_phyto.group)$species)) |> # get the species CC1 and CC2 scores
  mutate(label = factor(label, levels = c('small\nmixotrophs','large\nmixotrophs', 
                                              'unicellular\ncyanobacteria', 'colonial\ncyanobacteria', 
                                              'chlorophytes\n/other' )))  
# get the species CC1 and CC2 scores
df_environ  <- data.frame(scores(rda_phyto.group, display = 'bp')) #get the environment vars CC1 and CC2 scores
rownames(df_environ) = gsub(pattern = '`',replacement = '', rownames(df_environ))
RDA1_varex <- round(summary(rda_phyto.group)$cont$importance[2,1]*100,2) #Get percentage of variance explained by first axis
RDA2_varex <- round(summary(rda_phyto.group)$cont$importance[2,2]*100,2) #Get percentage of variance explained by second axis
# Set a scaling variable to multiply the rda values, in order to get a very similar plot to the the one generated by plot(rda_model). 
# Adjust it according to your data
scaling_factor <- 2

# Colors 
my.colors <- c("#ECD698", "#4E4019",
               "#26B7AE", "#097971",
               "#95DAA0")
# rda PLOT
rda.plot2 = ggplot(df_species, aes(x=RDA1, y=RDA2)) + 
  geom_hline(yintercept=0, linetype="dashed", size = 0.3) +
  geom_vline(xintercept=0, linetype="dashed", size = 0.3) +
  #Add environmental vars arrows
  geom_segment(data = df_environ, 
               aes(x=0, #Starting coordinate in RDA1 = 0 
                   xend = RDA1*scaling_factor,#Ending coordinate in RDA1  
                   y=0, #Start in RDA2 = 0
                   yend = RDA2*scaling_factor), #Ending coordinate in RDA2 
               color = "lightblue4", #set color
               arrow = arrow(length=unit(0.02,"npc"))) + #Set the size of the lines that form the tip of the arrow
  #Add species text
  geom_point(data = df_species, aes(x=RDA1, y = RDA2, fill = label), size = 3, shape = 21, alpha = 0.8, stroke = 0.2) +
  scale_fill_manual(values = my.colors, name = 'Group') + 
  #Add environmental vars text
  geom_label_repel(data = df_environ, 
                   aes(x = RDA1*scaling_factor, y = RDA2*scaling_factor),
                   label = rownames(df_environ),
                   label.padding = 0.1,
                   nudge_x = 0.2, nudge_y = -0.2,
                   color="lightblue4", fill = alpha("white", 0.9),
                   segment.size = 0.2, size = 2.2) +
  #Set x and y axis titles
  labs(x=paste0("RDA1 (",RDA1_varex," %)"),
       y=paste0("RDA2 (",RDA2_varex," %)"),
       title = 'Phytoplankton') +
  #Set bw theme
  theme_bw(base_size = 9) +
  theme(legend.text = element_text(size = 7), 
        plot.title = element_text(size = 8, color = '#bfac17', face = 2, margin=margin(0,0,1,0)),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin = margin(c(0,0,0,0), unit = "cm"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.7,"cm")); rda.plot2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# combine plots ####
rda.plot1 + rda.plot2 + plot_layout(widths = c(0.5,0.5)) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size  = 8))

# ggsave('SSBfigures/Figure7_rda_phytoplankton.png', width = 6.5, height = 2.5, dpi = 500)  
#####

