###############################################|
#--- Cropping OSM data to make highres maps ---#
###############################################|

# OpenStreetMap have a lots of publicly available high-res map data
# osmdata provides this in several formats and projections
# To see what is available go to: https://osmdata.openstreetmap.de/data/

# Load packageshttp://127.0.0.1:37723/graphics/plot_zoom_png?width=965&height=900
if(!require(pacman)) install.packages("pacman")
pacman::p_load(here, curl, osmdata, tidyverse, sf, ggspatial)

# Download the high resolution world basemap as a zip file
curl::curl_download(url = "https://osmdata.openstreetmap.de/download/land-polygons-complete-4326.zip", #source
                    destfile = here("land_polygons.zip"), #destination on your machine
                    quiet = FALSE) #show progress
# Can also be done manually by going to: https://osmdata.openstreetmap.de/download/land-polygons-complete-4326.zip

# Unzip the downloaded file and delete the zip (unhash file.remove line - optional)
unzip(zipfile = here("land_polygons.zip"), #zip file to extract
      exdir = here("land_shapefiles"), #destination folder
      junkpaths = TRUE) #use directory folder name (not folder from zip)
#file.remove(here("land_polygons.zip")) # this deletes the original zip file (take care editing this line)

# Crop the high resolution world shapefile
sf_use_s2(FALSE) #disable spherical geometry for cropping
worldmap <- st_read(here("land_shapefiles","land_polygons.shp"))
UK_extents <- st_bbox(c(xmin = -14, xmax = 2.5,
                        ymin = 49.5, ymax = 61), crs = 4326)
st_crop(worldmap, UK_extents, progress = TRUE) %>% #crop giant world map to UK (might take a while)
  saveRDS(object = ., file = here("UK_map.RDS")) #save to RDS
sf_use_s2(TRUE) #renable spherical geometry

# Flatten the high resolution UK basemap for zoomed plotting
basemap <- #read and simplify UK map
  readRDS(here("UK_map.RDS")) %>% 
  st_combine() #flatten the multipolygon sf object (i.e., into one layer)

# Plot the high resolution UK basemap
basemap_simple <- basemap %>% #create a simplified basemap for intersection comparison
  st_transform(crs = 3857) %>% #convert to a metres CRS for distance
  st_simplify(preserveTopology = FALSE, dTolerance = 1000) %>% #1KM tolerance
  st_transform(crs = 4326) #transform back to 4326

# Download some example point data
cities <- readr::read_csv("https://simplemaps.com/static/data/country-cities/gb/gb.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) #convert to sf object

# Create scalebar to add to maps
scalebar_km <- ggspatial::annotation_scale(location = "br", plot_unit = "km",
                                        bar_cols = "grey30", line_col = "grey30") 
# Plot simple basemap
UK_map <- ggplot() +
  geom_sf(data = basemap_simple, fill = "darkseagreen", colour = NA) + #plot country polygons
  geom_sf(data = cities, colour = "tomato3", size = 1) + #plot cities
  coord_sf(expand = FALSE, xlim = c(-12, 2.5), ylim = c(49.5, 61)) + #specify plot limits
  scalebar_km + #add scale bar
  labs(subtitle = "UK cities") #add subtitle
UK_map #print map to have a look

# Save the map as png
ggsave(plot = UK_map, filename = here("UK_map.png"), 
       width = 10, height = 14, units = "cm", dpi = 200) #size and resolution

# Cropping high resolution world basemap to smaller custom area (e.g. Isle of Man)
# https://norbertrenner.de/osm/bbox.html is a good place to get custom lat-lon coords
sf_use_s2(FALSE) #disable spherical geometry for cropping
iom_extents <- st_bbox(c(xmin = -5.099, xmax = -3.969,
                         ymin = 53.923, ymax = 54.508), crs = 4326)
basemap_close <- st_crop(worldmap, iom_extents, progress = TRUE) #crop giant world map to UK (might take a second)
sf_use_s2(TRUE) #renable spherical geometry

# Plot the close-up map
iom_map <- ggplot() +
  geom_sf(data = basemap_close, fill = "darkseagreen", colour = NA) + #plot country polygons
  coord_sf(expand = TRUE) + #specify plot limits
  scalebar_km + #add scale bar
  labs(subtitle = "Isle of Man") #add subtitle
iom_map #print map to have a look
