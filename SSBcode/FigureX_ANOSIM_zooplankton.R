#The analysis that we care about is at the very bottom with ******** (until Ellie cleans this up!)

library(tidyverse)
library(lubridate)
library(dplyr)
library(vegan)

# Zooplankton data from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/414/1/63bfa1566eaf39762e3036b8a7906359" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
zoops.raw = read_csv(infile1)

#anosim with all sample dates / years --> NOT SIGNIFICANT (p = 0.0798)
#Only dates under ice and combining and combining duplicate "Unknown rotifer" counts (from 2020-01-31 and 2021-03-15) to make matrix
zoops.winter <- zoops.raw %>%
  filter(month(zoops.raw$sample_date)  < 4  | month(zoops.raw$sample_date) >= 12 ) %>%
  group_by(sample_date, species_name) %>% summarise(individuals_measured = sum(individuals_measured))

#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df <- zoops.raw %>%
  filter(month(zoops.raw$sample_date)  < 4  | month(zoops.raw$sample_date) >= 12 ) %>%
  select(year, sample_date) %>%
  distinct(year, sample_date, .keep_all = TRUE)

#pivoting table to match abundance matrix 
zoop.pivoted <- zoops.winter %>%
  select(sample_date, individuals_measured, species_name) %>%
  group_by(sample_date, species_name) %>%
  pivot_wider(names_from = species_name, values_from = individuals_measured, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#converting zoop.pivoted df into matrix
zoop.matrix = as.matrix(zoop.pivoted)

#ANOSIM
ano = anosim(zoop.matrix, group.df$year, distance = "bray", permutations = 9999)
ano

#~~~~~~~~~~~~~~~
#Different anosim using a dissimilarity matrix --> NOT SIGNIFICANT (p = 0.084)
data(zoop.pivoted)
data(group.df)
zoop.dist <- vegdist(zoop.pivoted)
zoop.ano <- with(group.df, anosim(zoop.dist, year))
summary(zoop.ano)
plot(zoop.ano)

#NMDS attempt(?) overlapping polygons
nmds <- metaMDS(zoop.pivoted)
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

#~~~~~~~~~~~~~~~
#anosim between reference year and second manipulation year(?) --> NOT SIGNIFICANT (p = 0.0538)

#Only dates under ice and combining and combining duplicate "Unknown rotifer" counts (from 2020-01-31 and 2021-03-15) to make matrix
zoops.winter2 <- zoops.raw %>%
  filter(month(zoops.raw$sample_date)  < 4  | month(zoops.raw$sample_date) >= 12 ) %>% 
  group_by(sample_date, species_name) %>% summarise(individuals_measured = sum(individuals_measured)) %>%
  filter(sample_date < '2020-01-01' | sample_date > '2020-12-31') 

#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df2 <- zoops.raw %>%
  filter(month(zoops.raw$sample_date)  < 4  | month(zoops.raw$sample_date) >= 12 ) %>%
  distinct(year, sample_date, .keep_all = TRUE) %>%
  filter(sample_date < '2020-01-01' | sample_date > '2020-12-31') 

#pivoting table to match abundance matrix 
zoop.pivoted2 <- zoops.winter2 %>%
  select(sample_date, individuals_measured, species_name) %>%
  group_by(sample_date, species_name) %>%
  pivot_wider(names_from = species_name, values_from = individuals_measured, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#converting zoop.pivoted df into matrix
zoop.matrix2 = as.matrix(zoop.pivoted2)

#ANOSIM
ano2 = anosim(zoop.matrix2, group.df2$year, distance = "bray", permutations = 9999)
ano2

#~~~~~~~~~~~~~~
#anosim between first manipulation and second manipulation year(?) --> NOT SIGNIFICANT (p = 0.0733)

#Only dates under ice and combining and combining duplicate "Unknown rotifer" counts (from 2020-01-31 and 2021-03-15) to make matrix
zoops.winter3 <- zoops.raw %>%
  filter(month(zoops.raw$sample_date)  < 4  | month(zoops.raw$sample_date) >= 12 ) %>% 
  group_by(sample_date, species_name) %>% summarise(individuals_measured = sum(individuals_measured)) %>%
  filter(sample_date > '2019-12-31') 

#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df3 <- zoops.raw %>%
  filter(month(zoops.raw$sample_date)  < 4  | month(zoops.raw$sample_date) >= 12 ) %>%
  distinct(year, sample_date, .keep_all = TRUE) %>%
  filter(sample_date > '2019-12-31') 

#pivoting table to match abundance matrix 
zoop.pivoted3 <- zoops.winter3 %>%
  select(sample_date, individuals_measured, species_name) %>%
  group_by(sample_date, species_name) %>%
  pivot_wider(names_from = species_name, values_from = individuals_measured, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#converting zoop.pivoted df into matrix
zoop.matrix3 = as.matrix(zoop.pivoted3)

#ANOSIM
ano3 = anosim(zoop.matrix3, group.df3$year, distance = "bray", permutations = 9999)
ano3

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#*********************
#At the genus-level

#anosim with all sample dates / years --> N(p = 0.0798)
#Only dates under ice and combining and combining duplicate "Unknown rotifer" counts (from 2020-01-31 and 2021-03-15) to make matrix
zooplankton <- read_csv("SSBdata/SSB_zooplankton_ANOSIM.csv")
zooplankton$sample_date <- as.Date(zooplankton$sample_date, "%m/%d/%Y")

zoops.genus <- zooplankton %>%
  filter(month(zooplankton$sample_date)  < 4  | month(zooplankton$sample_date) >= 12 ) %>%
  group_by(sample_date, genus) %>% summarise(individuals_measured = sum(individuals_measured))

#dataframe with grouping info (sample year and date) to be used in ANOSIM analysis
group.df <- zooplankton %>%
  filter(month(zooplankton$sample_date)  < 4  | month(zooplankton$sample_date) >= 12 ) %>%
  select(year, sample_date) %>%
  distinct(year, sample_date, .keep_all = TRUE)

#pivoting table to match abundance matrix 
zoop.pivoted <- zoops.genus %>%
  select(sample_date, individuals_measured, genus) %>%
  group_by(sample_date, genus) %>%
  pivot_wider(names_from = genus, values_from = individuals_measured, values_fill = 0) %>%
  ungroup() %>% select(-sample_date)

#converting zoop.pivoted df into matrix
zoop.matrix = as.matrix(zoop.pivoted)

#ANOSIM
ano = anosim(zoop.matrix, group.df$year, distance = "bray", permutations = 9999)
ano

#~~~~~~~~~~~~~~~
#Different anosim using a dissimilarity matrix --> NOT SIGNIFICANT (p = 0.084)
data(zoop.pivoted)
data(group.df)
zoop.dist <- vegdist(zoop.pivoted)
zoop.ano <- with(group.df, anosim(zoop.dist, year))
summary(zoop.ano)
plot(zoop.ano)

#NMDS attempt(?) overlapping polygons
nmds <- metaMDS(zoop.pivoted)
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




