rm(list=ls())
dev.off()
setwd("/Users/daisymitchell/MyRCoursework/Code")


library(ggplot2)
library(dplyr)
library(tidyr)

library(osmdata)
library(sf)
library(ggmap)
library(tmap)





####### plotting buildings in CoL #########

# Define the location
bbox <- getbb("City of London, UK")

# Get OpenStreetMap data for CoL
osm_data <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'building') %>%
  osmdata_sf()

summary(osm_data)

# Plot map
tmap_mode("view")
tm_shape(osm_data$osm_polygons) +
  tm_polygons() +
  tm_layout(title = "Map of the City of London")





####### Add points for my study sites, coloured by site type ########

mapping_sites <- read.csv("/Users/daisymitchell/Desktop/CoL_site_coords.csv")


# Split coordinates into Latitude and Longitude
mapping_sites <- mapping_sites %>%
  separate(Coords, into = c("Latitude", "Longitude"), sep = ", ") %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Convert to sf
sites_sf <- st_as_sf(mapping_sites, coords = c("Longitude", "Latitude"), crs = 4326)

# Define the location
bbox <- getbb("City of London, UK")

# Get OpenStreetMap data
osm_data <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'building') %>%
  osmdata_sf()

# Extract building polygons
buildings_sf <- osm_data$osm_polygons

# Colours
custom_palette <- c('Intensive' = 'chartreuse2',
                    'Garden' = 'chartreuse4',
                    'Extensive' = 'darkorange2',
                    'Conventional' = 'darkgray')



# Want labels joined to points with line - need to specify line shortening
sites_sf <- sites_sf %>%
  mutate(nudge_y = case_when(
    Site_type %in% c("Conventional", "Extensive") ~ -0.0005,
    Site_type %in% c("Intensive", "Garden") ~ 0.0005
  ),
  line_shortening = 0.5)


# Create a base map with buildings
ggplot() +
  geom_sf(data = buildings_sf, fill = "lightgray", color = "gray") +  # Buildings
  geom_sf(data = sites_sf, aes(color = Site_type), size = 3) +  # Sites
  geom_segment(data = sites_sf, aes(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2],
    xend = st_coordinates(geometry)[,1],
    yend = st_coordinates(geometry)[,2] + nudge_y - sign(nudge_y) * 0.0002,
    color = Site_type
  ),
  size = 1, lineend = "round") + # Connecting lines
  geom_text(data = sites_sf, aes(
  label = Site,
  color = Site_type,
  x = st_coordinates(geometry)[,1],
  y = st_coordinates(geometry)[,2] + nudge_y
  ),
  fontface = "bold", size = 4) +  # Site labels
  scale_color_manual(values = custom_palette) +
  coord_sf(xlim = c(bbox[1,1], bbox[1,2]), ylim = c(bbox[2,1], bbox[2,2]), expand = FALSE) +
  labs(color = "Site type") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(color = "gray", linewidth = 0.5))



###### Add coloured background boxes behind labels ##########

# Create a base map with buildings
ggplot() +
  geom_sf(data = buildings_sf, fill = "#D1D3D5", color = "gray") +  # Buildings
  geom_sf(data = sites_sf, aes(color = Site_type), size = 3) +  # Site points
  geom_segment(data = sites_sf, aes(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2],
    xend = st_coordinates(geometry)[,1],
    yend = st_coordinates(geometry)[,2] + nudge_y - sign(nudge_y) * 0.0002,
    color = Site_type
  ),
  size = 1, lineend = "round") + # Connecting lines
  
  # Label with background rectangle
  geom_label(data = sites_sf, aes(
    label = Site,
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2] + nudge_y,
    fill = Site_type
  ),
  color = sites_sf$Site_type %>% 
    sapply(function(x) ifelse(x %in% c("Conventional", "Intensive"), "black", "white")),
  fontface = "bold", size = 4, label.size = NA) +  # Site labels with no border
  
  scale_color_manual(values = custom_palette) +
  scale_fill_manual(values = custom_palette) +  
  coord_sf(xlim = c(bbox[1,1], bbox[1,2]), ylim = c(bbox[2,1], bbox[2,2]), expand = FALSE) +
  labs(color = "Site type", fill = "Site type") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(color = "gray", linewidth = 0.5))






# Labels always overlap - remove and add manually in PowerPoint
############ Remove labels and increase axis and legend text size ##############

# Create a base map with buildings
ggplot() +
  geom_sf(data = buildings_sf, fill = "#D1D3D5", color = "gray") +  # Buildings
  geom_sf(data = sites_sf, aes(color = Site_type), size = 4) +  # Site points
  # No labels, lines, or background boxes anymore
  scale_color_manual(values = custom_palette) +
  coord_sf(xlim = c(bbox[1,1], bbox[1,2]), ylim = c(bbox[2,1], bbox[2,2]), expand = FALSE) +
  labs(color = "Site type", fill = "Site type") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),   
        axis.text.y = element_text(size = 14),  
        axis.ticks = element_line(color = "gray", linewidth = 0.5),
        legend.text = element_text(size = 18),   
        legend.title = element_text(size = 20))







