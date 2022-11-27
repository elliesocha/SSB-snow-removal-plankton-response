library(ggspatial)
library(sf)
library(ggrepel)
library(tidyverse)
library(maps)
library(mapdata)
library(patchwork)
library(jpeg)
library(png)
library(grid)

# SSB png
img1 <- readJPEG("SSBcode/Map/SSB_googleearth.jpg", native = T)
img2 <- readPNG("SSBcode/Map/SSBplowed.png")

#
ssb <- data.frame(site = c("South Sparkling Bog"),
                                lat = c(46.003285),
                                lon = c(-89.705346)) %>%
  st_as_sf(coords = c('lon','lat'),crs = 4326) 

## Map of Wisconsin
states = st_read('SSBcode/Map/WI_borderstates.shp')
NHD.simple = st_read('SSBcode/Map/NHD_simple.shp')
wi.simple = st_read('SSBcode/Map/Wisconsin_State_Boundary_simple.shp')
greatLakes = st_read('SSBcode/Map/greatLakes.shp')


w1 = ggplot(wi.simple) +
  geom_sf(data = states, col = 'grey50', fill = 'grey90', alpha = 0.5, size = 0.2) +
  geom_sf(data = NHD.simple, col = NA, fill = 'lightsteelblue2') +
  geom_sf(data = greatLakes, fill = 'lightsteelblue2', col = 'lightsteelblue2') +
  # geom_sf(data = st_as_sfc(st_bbox(ssb)), fill = 'red4', color = 'red4', size = 5) + # Inset box
  geom_sf(data = ssb, fill = 'red4', color = 'black', size = 4, shape = 22, stroke = 1) + # Inset box
  coord_sf(ylim = c(42.3,47.5), xlim = c(-93, -86), expand = FALSE) + # limit axes
  theme_bw(base_size = 9) +
  theme(#plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1))

# Patchwork combine map
layout <- "
AB
CC
"

w2 = ggplot(data.frame(x = 0:1, y = 0:1), aes(x,y)) +
  geom_blank() +
  annotation_custom(grid::rasterGrob(img1)) +
  annotate('text', label = 'South Sparkling Bog', x = 0.2, y = 0.16, color = 'white', size = 3) +
  annotate("segment", x = 0.14, y = 0.23, xend = 0.23, yend = 0.27,
           arrow = arrow(angle = 30, length = unit(.2,"cm")), color = 'white', size = 1) +
  theme_void()

w3 = ggplot() + annotation_custom(grid::rasterGrob(img2)) +
  theme_void()

# Patchwork 
w1 + w2 + w3 + 
plot_layout(design = layout, widths = c(1,1.25)) +
plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
theme(plot.tag = element_text(size  = 8), 
      plot.margin = unit(c(0, 0, 0, 0), "cm")) 

ggsave('SSBfigures/Figure1_Map.png', width = 6.5, height = 5, dpi = 500, bg = "transparent")
  

# w1 + 
#   wrap_elements(grobTree(
#     rasterGrob(img1, interpolate=TRUE), 
#     textGrob('South Sparkling Bog', x = 0.3, y = 0.28, gp=gpar(fontsize=9, col="white")),
#   )) +
#   wrap_elements(grobTree(
#     rasterGrob(img2, just = 'center', interpolate=TRUE)
#   )) + 
#   plot_layout(design = layout, widths = c(1,1.25)) +
#   plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
#   theme(plot.tag = element_text(size  = 8), 
#         plot.margin = unit(c(0, 0, 0, 0), "cm"))

  
