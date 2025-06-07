###############################################|
#--- Using {terra} to rasterise point data ---#
###############################################|

# Load packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(here, tidyverse, sf, terra, tidyterra, patchwork,
               rnaturalearth, rnaturalearthdata, rnaturalearthhires)

#define chosen projection with st_crs (optional)
#laea = st_crs("+proj=laea +lat_0=30 +lon_0=-95") #custom crs for lambert azimuthal equal area projection

# read in  shapefile data
shapefile <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(admin == "United Kingdom") %>%
  st_crop(xmin = -7, xmax = -4.1, ymin = 49, ymax = 51.1) %>%
  st_transform(3857)

#make simple plot
simple <- ggplot() +
  geom_sf(data = shapefile, fill = "white", colour = "black") +
  #lims(x = c(-706761, -472639), y = c(6430637, 6600000)) +
  theme_bw(base_size = 9) +
  theme(panel.background = element_rect(fill = "azure2"),
        panel.grid.major = element_line(colour = "white"))

#read in/create dataset of occurrence points
occurrence <- shapefile %>%
  st_buffer(1500) %>%
  sf::st_sample(., size=500) %>%
  st_as_sf()

#add occurrence data to simple plot
simple +
  geom_sf(data = occurrence, colour = "plum", size = 2, shape = 18)

# create template raster
raster_template <- terra::rast(shapefile, res = 5000) #create 5km grid

#binarise records data to template
occurrence_5km <- terra::rasterize(occurrence, raster_template) %>%
  subst(., NA, 0) %>% #substituting all NA's with zeroes
  mutate(last = as.factor(last)) #check if last column is a factor

#finally add the rasterised data to the simple plot
full <- simple + 
  tidyterra::geom_spatraster(data = occurrence_5km) +
  scale_fill_manual(values = c("NA","plum"), labels = c("No", "Yes")) + #specifying no colour for zeroes in raster
  geom_sf(data = occurrence, colour = "black", pch=20, cex=0.01) + #add original record points to plot
  labs(fill = "Presence") +
  coord_sf(x = c(-706761, -472639), y = c(6430637, 6600000)) + #specify limits (projected crs)
  theme_bw(base_size = 9) +
  theme(panel.background = element_rect(fill = "azure2"),
        panel.grid.major = element_line(colour = "white"))
  
###########################|
# Rasterize line object ---#
###########################|

#rasterise coastline
coastline_raster <- st_cast(shapefile,"MULTILINESTRING") %>% #turn polygon into line
  terra::rasterize(., raster_template) %>% #rasterize the line
  subst(., NA, 0) #substituting all NA's with zeroes

# Compare and convert logical to numeric (1 = overlap, NA = no match)
coastline_records <- occurrence_5km * coastline_raster
coastline_records[coastline_records <= 1] <- NA
coastline_records <- as.factor(coastline_records)

#add the new coastline records raster to the plot
coastline <- simple +  
  geom_spatraster(data = coastline_records) + 
  scale_fill_manual(values = c("2" = "brown2"), 
                    labels = c("Yes", "No"), 
                    na.value = NA) + #specifying no colour for zeroes in raster
  labs(fill = "Coastline \nintersection") + #rename legend
  coord_sf(x = c(-706761, -472639), y = c(6430637, 6600000)) #specify limits (projected crs)

# combined plot with coastline and occurrence records
final <- full / coastline
final

#save the full plot as a png
ggsave(final, filename = here("plots","terra_rasterizing.png"),
       width = 12, height = 14, 
       units = "cm", dpi = 300)

