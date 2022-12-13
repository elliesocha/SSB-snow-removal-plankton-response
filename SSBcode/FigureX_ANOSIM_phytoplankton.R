library(tidyverse)
library(dplyr)
library(lubridate)
library(vegan)

Phytos = read_csv("SSBdata/SSB_manipulation_phyto_ANOSIM.csv")
Phytos$sample_date <- as.Date(Phytos$sample_date, "%m/%d/%Y")
is.Date(Phytos$sample_date)
SSB <- Phytos %>%
  filter(month(Phytos$sample_date)  < 4  | month(Phytos$sample_date) >= 12 ) %>%
  filter(sample_id == "SSB")
SSB <- SSB[!(SSB$sample_date=="2019-03-09"),]

#ANOSIM at DIVISION level for manuscript revisions
#Only significant analysis immediately below)
#2019 vs. 2021 --> Different anosim using a dissimilarity matrix --> SIGNIFICANT pvalue = 0.048 ANOSIM R statistics = 0.3846
#####
#ANOSIM
SSB2 <- SSB %>%
  select(sample_date, division, taxa_count) %>%
  filter(sample_date < '2020-01-01' | sample_date > '2020-12-31') %>%
  group_by(sample_date, division) %>%
  summarise(taxa_count = sum(taxa_count)) %>%
  pivot_wider(names_from = division, values_from = taxa_count, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df2 <- SSB %>%
  select(year, sample_date) %>%
  distinct(year, sample_date, .keep_all = TRUE) %>%
  filter(sample_date < '2020-01-01' | sample_date > '2020-12-31') 

p.dist2 <- vegdist(SSB2)
p.ano2 <- with(group.df2, anosim(p.dist2, year))
summary(p.ano2)
plot(p.ano2)

#NMDS attempt overlapping polygons
nmds2 <- metaMDS(SSB2)
nmds2
plot(nmds2)
ordiplot(nmds2, type="n")
orditorp(nmds2,display="species",col="red",air=0.01)
orditorp(nmds2,display="sites",cex=1.25,air=0.01)

treat=c(rep("Reference Year",3), rep("Manipulation Year 2", 5))
colors=c(rep("blue",3), rep("orange",5))
ordiplot(nmds2,type="n")
orditorp(nmds2,display="sites",col=c(rep("blue",3),
                                     rep("orange",5)),air=0.01,cex=1.25)
#Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(nmds2$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds2,display="species",col="black",air=0.01)
#####


#****All other ANOSIMs between all years / some years (using two different methods in the vegan package) were insignificant:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ANOSIM between All years --> pvalue = 0.1542
#####
#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df <- SSB %>%
  select(year, sample_date) %>%
  distinct(year, sample_date, .keep_all = TRUE)

#pivoting table to match abundance matrix 
p.pivoted <- SSB %>%
  select(sample_date, division, taxa_count) %>%
  group_by(sample_date, division) %>%
  summarise(taxa_count = sum(taxa_count)) %>%
  pivot_wider(names_from = division, values_from = taxa_count, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#converting p.pivoted df into matrix
p.matrix = as.matrix(p.pivoted)
#ANOSIM
ano = anosim(p.matrix, group.df$year, distance = "bray", permutations = 9999)
ano
#####

#ANOSIM between reference year (2019) and second manipulation year (2021) --> pvalue = 0.0529
#####
#pivoting table to match abundance matrix 
SSB2 <- SSB %>%
  select(sample_date, division, taxa_count) %>%
  filter(sample_date < '2020-01-01' | sample_date > '2020-12-31') %>%
  group_by(sample_date, division) %>%
  summarise(taxa_count = sum(taxa_count)) %>%
  pivot_wider(names_from = division, values_from = taxa_count, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df2 <- SSB %>%
  select(year, sample_date) %>%
  distinct(year, sample_date, .keep_all = TRUE) %>%
  filter(sample_date < '2020-01-01' | sample_date > '2020-12-31') 

#converting p.pivoted df into matrix
p.matrix2 = as.matrix(SSB2)
#ANOSIM
ano2 = anosim(p.matrix2, group.df2$year, distance = "bray", permutations = 9999)
ano2
#####

#ANOSIM between first manipulation (2020) and second manipulation year (2021) --> pvalue = 0.7586
#####
#pivoting table to match abundance matrix 
SSB3 <- SSB %>%
  select(sample_date, division, taxa_count) %>%
  filter(sample_date > '2019-12-31') %>%
  group_by(sample_date, division) %>%
  summarise(taxa_count = sum(taxa_count)) %>%
  pivot_wider(names_from = division, values_from = taxa_count, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df3 <- SSB %>%
  select(year, sample_date) %>%
  distinct(year, sample_date, .keep_all = TRUE) %>%
  filter(sample_date > '2019-12-31') 

#converting p.pivoted df into matrix
p.matrix3 = as.matrix(SSB3)
#ANOSIM
ano3 = anosim(p.matrix3, group.df3$year, distance = "bray", permutations = 9999)
ano3
#####


#ANOSIM between reference year (2019) and first manipulation (2020) --> NOT SIGNIFICANT pvalue = 0.1714
#####
#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
SSB4 <- SSB %>%
  select(sample_date, division, taxa_count) %>%
  filter(sample_date < '2021-01-01') %>%
  group_by(sample_date, division) %>%
  summarise(taxa_count = sum(taxa_count)) %>%
  pivot_wider(names_from = division, values_from = taxa_count, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df4 <- SSB %>%
  select(year, sample_date) %>%
  distinct(year, sample_date, .keep_all = TRUE) %>%
  filter(sample_date < '2021-01-01') 

#converting p.pivoted df into matrix
p.matrix4 = as.matrix(SSB4)
#ANOSIM
ano4 = anosim(p.matrix4, group.df4$year, distance = "bray", permutations = 9999)
ano4
#####

#~~~~~~~~~~~~~~~~~~~~

#ANOSIM a different way:
#ALL YEARS Different anosim using a dissimilarity matrix--> NOT SIGNIFICANT (p = 0.159)
#####
p.dist <- vegdist(p.pivoted)
p.ano <- with(group.df, anosim(p.dist, year))
summary(p.ano)
plot(p.ano)

#NMDS attempt(?) overlapping polygons
nmds <- metaMDS(p.pivoted)
nmds
plot(nmds)
ordiplot(nmds, type="n")
orditorp(nmds,display="species",col="red",air=0.01)
orditorp(nmds,display="sites",cex=1.25,air=0.01)

treat=c(rep("Reference Year",3),rep("Manipulation Year 1",3), rep("Manipulation Year 2", 5))
colors=c(rep("blue",3),rep("red",3), rep("orange",5))
ordiplot(nmds,type="n")
orditorp(nmds,display="sites",col=c(rep("blue",3),
                                    rep("red",3), rep("orange",5)),air=0.01,cex=1.25)
#Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(nmds$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds,display="species",col="black",air=0.01)
#####

#ANOSIM a different way:
#2020 va 2021 Different anosim using a dissimilarity matrix --> NON SIGNIFICANT pvalue = 0.783
#####
p.dist3 <- vegdist(SSB3)
p.ano3 <- with(group.df3, anosim(p.dist3, year))
summary(p.ano3)
plot(p.ano3)
#NMDS attempt(?) overlapping polygons
nmds3 <- metaMDS(SSB3)
nmds3
plot(nmds3)
ordiplot(nmds3, type="n")
orditorp(nmds3,display="species",col="red",air=0.01)
orditorp(nmds3,display="sites",cex=1.25,air=0.01)

treat=c(rep("Manipulation Year 1",3), rep("Manipulation Year 2", 5))
colors=c(rep("red",3), rep("orange",5))
ordiplot(nmds3,type="n")
orditorp(nmds3,display="sites",col=c(rep("red",3),
                                     rep("orange",5)),air=0.01,cex=1.25)
#Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(nmds3$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds3,display="species",col="black",air=0.01)
#####

#ANOSIM a different way:
#2019 va 2020 Different anosim using a dissimilarity matrix --> NON SIGNIFICANT pvalue = 0.166
#####
p.dist4 <- vegdist(SSB4)
p.ano4 <- with(group.df4, anosim(p.dist4, year))
summary(p.ano4)
plot(p.ano4)
#NMDS attempt(?) overlapping polygons
nmds4 <- metaMDS(SSB4)
nmds4
plot(nmds4)
ordiplot(nmds4, type="n")
orditorp(nmds4,display="species",col="red",air=0.01)
orditorp(nmds4,display="sites",cex=1.25,air=0.01)

treat=c(rep("Manipulation Year 1",3), rep("Manipulation Year 1", 5))
colors=c(rep("blue",3), rep("red",5))
ordiplot(nmds4,type="n")
orditorp(nmds4,display="sites",col=c(rep("blue",3),
                                     rep("red",5)),air=0.01,cex=1.25)
#Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(nmds4$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(nmds4,display="species",col="black",air=0.01)
#####
