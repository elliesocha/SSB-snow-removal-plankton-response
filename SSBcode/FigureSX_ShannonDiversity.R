library(tidyverse)
library(vegan)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('SSBcode/00_LoadData.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pp.diversity <- pp %>% 
  arrange(sampledate) |> 
  select(sampledate, taxa_name, cells_per_ml) |> 
  group_by(sampledate, taxa_name) |> 
  summarise(cells_per_ml = sum(cells_per_ml)) |> 
  ungroup() |> 
  pivot_wider(names_from = taxa_name, values_from = cells_per_ml, values_fill = 0) |> 
  select(-sampledate)

pp.d = data.frame(sampledate = sort(unique(pp$sampledate)), shannonDiversity = diversity(pp.diversity, index = 'shannon'))

zoops.diversity = zoops %>% 
  select(sample_date, species_name, individuals_measured) |> 
  group_by(sample_date, species_name) |> 
  summarise(individuals_measured = sum(individuals_measured)) |> 
  ungroup() |> 
  pivot_wider(names_from = species_name, values_from = individuals_measured, values_fill = 0) |> 
  select(-sample_date) 

z.d = data.frame(sampledate = sort(unique(zoops$sample_date)), shannonDiversity = diversity(zoops.diversity, index = 'shannon')) |> 
  bind_rows(data.frame(sampledate = as.Date('2020-02-14'), shannonDiversity = NA))



p1 = ggplot(pp.d) +
  geom_point(aes(x = factor(sampledate), y = shannonDiversity), fill = 'lightblue3', shape = 21, stroke = 0.2, size = 3) +
  ylab('Shannon Diversity') +
  labs(title = 'Phytoplankton') +
  geom_vline(aes(xintercept = 3.5), linetype = 2) +
  geom_vline(aes(xintercept = 7.5), linetype = 2) +
  theme_bw(base_size = 9) +
  theme(#axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid = element_line(size = rel(0.3)),
    axis.title.x = element_blank())

p2 = ggplot(z.d) +
  geom_point(aes(x = factor(sampledate), y = shannonDiversity), fill = 'lightblue3', shape = 21, stroke = 0.2, size = 3) +
  ylab('Shannon Diversity') +
  labs(title = 'Zooplankton') +
  geom_vline(aes(xintercept = 3.5), linetype = 2) +
  geom_vline(aes(xintercept = 7.5), linetype = 2) +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid = element_line(size = rel(0.3)),
    axis.title.x = element_blank())

p1/p2

ggsave('SSBfigures/FigureSX_ShannonDiversity.png', width = 6.5, height = 4, dpi = 500)  

