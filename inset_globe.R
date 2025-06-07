##############################################|
#--- Making custom globe inset maps in sf ---#
##############################################|

# Go to any bounding box map maker 
# (e.g., http://bboxfinder.com/ or https://boundingbox.klokantech.com/)
# choose an area that you want to map and copy the coordinates as text/csv to paste below
your_bbox <- c(112, 21, 116, 24) # <<< paste your copied coordinates inside these brackets

# Load packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(here, tidyverse, dplyr, #tidyr
               s2, sf, ggspatial, terra, #spatial packages
               rnaturalearth, rnaturalearthdata, maps, #spatial data
               patchwork) #plotting

# OVERVIEW OF THIS CODE      
#1. Make a simple basemap
#2. Generate a globe map (with shading!)
#3. Combine into one plot
    
#----------------------#
# 1. Simple basemap ####
#----------------------#

# turn off spherical geometries (not needed for most maps)
sf::sf_use_s2(FALSE)

# specify limits/range of your map with a bounding box (aka bbox)
bbox <- your_bbox %>% # <<< paste your copied coordinates inside these brackets
  setNames(c("xmin", "ymin", "xmax", "ymax")) %>% 
  sf::st_bbox(crs = 4326)

# you can alternatively specify your bounding box manually:
#bbox <- st_bbox(c(xmin=112, xmax=116, ymin=21, ymax=24))

# we're going to use EPSG:3857 as the projection, so it's worth reprojecting our bbox too
bbox_3857 <- st_as_sfc(bbox) %>% st_transform(., crs = 3857) %>% st_bbox(.)

# grab lower resolution data of the world (rnaturalearthhighres) and subset to your bounding box
basemap_sf <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_transform(crs = 3857) %>%
  st_crop(., bbox_3857) #crop to area of bounding box
# (You can ignore the "attribute variables" warnings)

# for a high-resolution basemap, see the guide in this same repo
# https://github.com/sketchkey/R-spatial/blob/main/highres_maps.R
# replace basemap above with the code to read the RDS file you've created

# check the cropping worked by doing basic plot
ggplot(basemap_sf, aes(geometry=geometry)) +
  geom_sf()

# get centre of bbox (or specify focal point manually) for inset globe
basemap_centre <- st_as_sfc(bbox) %>% st_centroid(.)
#basemap_centre <- c(x = 113.5, y = 21.5)

# create annotated basemap
basemap <- ggplot() +
  geom_sf(data = basemap_sf, colour = "grey50", fill = "grey80", lwd = 0.4) +
  coord_sf(
    xlim = c(bbox_3857["xmin"], bbox_3857["xmax"]),
    ylim = c(bbox_3857["ymin"], bbox_3857["ymax"]),
    expand = FALSE
  ) +
  ggspatial::annotation_scale(style="bar", 
                              location = "bl", 
                              height = unit(0.15, "cm"), #scale bar
                              bar_cols = c("grey30","grey30"), 
                              line_col="grey30", 
                              text_col="grey30") +
  theme_bw() +
  NULL
basemap

#-----------------------#
# 2. Inset globe map ####
#-----------------------#

# this section heavily reuses verbatim code from Liam Bailey's excellent guide:
# https://liamdbailey.com/posts/2024_08_31-azimuthalproj/

# whilst also being inspired by the D3 genius of Jacob Rus
# https://observablehq.com/@jrus/shaded-sphere-from-svg-gradient

# turn spherical geometry back on (previously turned off to remove headaches)
sf::sf_use_s2(TRUE)

# convert basemap centre to a simple vector
centre <- c(x = basemap_centre[[1]][1], y = basemap_centre[[1]][2])

# grab some nice spherical-friendly data 
countries <- s2_data_countries()

#Specify a coordinate reference system that sets the centre of your basemap as the centre of the projection
crs_ortho <- paste0("+proj=laea +lat_0=",centre["y"]," +lon_0=",centre["x"])
countries_ortho <- st_transform(st_as_sfc(countries),crs_ortho)

# clip around our central point
buffer <- s2_buffer_cells(
  as_s2_geography(paste0("POINT(",centre["x"]," ",centre["y"],")")),
  distance = 9800000, #radius of the buffer in m (one whole hemisphere of the earth)
  max_cells = 5000) #smooth buffer (higher >> smoother)

# clip our country polygons using this buffer and reproject
countries_clip <- s2_intersection(buffer, countries)
countries_clip_ortho <- st_transform(st_as_sfc(countries_clip),crs_ortho) # reproject to laea

### GRATICULES
## Specify range of lines of longitudinal

# (i.e. lines running N-S)
meridian_long <- seq(-180, 175, 10)
# Create linestring for each line
long_lines <- lapply(X = meridian_long,
                     FUN = \(x){
                       sf::st_linestring(x = data.frame(V1 = x,
                                                        V2 = seq(-90, 90, 1)) |>
                                           as.matrix())}) |> 
  sf::st_sfc(crs = "EPSG:4326")

# do the same process for lines of latitude (i.e. E-W)
meridian_lat <- seq(-80, 80, 10)
lat_lines <- lapply(X = meridian_lat,
                    FUN = \(x){
                      sf::st_linestring(x = data.frame(V1 = seq(-180, 180, 1),
                                                       V2 = x) |>
                                          as.matrix())}) |> 
  sf::st_sfc(crs = "EPSG:4326")

# combine together into a single dataset, then clip and reproject
graticules <- c(lat_lines, long_lines)
graticules_clip <- s2_intersection(buffer, graticules)
graticules_clip_ortho <- st_transform(st_as_sfc(graticules_clip), crs_ortho)

### OCEAN
# to create ocean polygon, get the *inverse* of all countries, then clip and transform
globe <- s2::as_s2_geography(TRUE) ## The whole Earth
ocean <- s2_difference(globe, s2_union_agg(countries))
ocean_clip <- s2_intersection(buffer, ocean)
ocean_clip_ortho <- st_transform(st_as_sfc(ocean_clip), crs_ortho)

# create ortho projection using our centre point
crs_ortho <- paste0("+proj=ortho +lat_0=", centre["y"]," +lon_0=", centre["x"])

# transform all the spatial data using our now orthographic projection
countries_clip_ortho <- st_transform(st_as_sfc(countries_clip), crs_ortho)
ocean_clip_ortho <- st_transform(st_as_sfc(ocean_clip), crs_ortho)
buffer_circle_ortho <- st_transform(st_as_sfc(buffer), crs_ortho)
graticules_clip_ortho <- st_transform(st_as_sfc(graticules_clip), crs_ortho)

# create a simple gradient raster for shading our globe
shading <- rast(nrows = 2000, ncols = 2000,
          xmin = -10000000, xmax = 10000000,
          ymin = -10000000, ymax = 10000000,
          crs = crs_ortho)

# coordinates in projected space
xy <- crds(shading, df = TRUE)

# adjust gradient parameters (so it "looks" 3D by having light hit top left)
centre_sf <- sf::st_sfc(sf::st_point(c(centre["x"], centre["y"])), crs = 4326)
centre_proj <- centre_sf %>% st_transform(., crs = crs_ortho) %>% st_coordinates(.)
radius <- 9400000 # radius of the gradient circle
fx <- as.numeric(centre_proj[,"X"]) - (radius * 0.20) #adjust focal x east a bit
fy <- as.numeric(centre_proj[,"Y"]) + (radius * 0.25) #adjust focal y north a bit

# distance from focal point
grad_radius <- radius * 1.6  # define radius of our gradient
dist <- sqrt((xy$x - fx)^2 + (xy$y - fy)^2) / grad_radius

# define stops of the gradient
alpha <- case_when(
  dist < 0.3 ~ 0 + (0.1 - 0) * (dist / 0.3),
  dist < 0.5 ~ 0.1 + (0.3 - 0.1) * ((dist - 0.3) / 0.2),
  dist < 0.9 ~ 0.3 + (1.0 - 0.3) * ((dist - 0.5) / 0.4),
  TRUE       ~ 1) * 0.3

# mask to circular area (same radius)
mask <- sqrt((xy$x)^2 + (xy$y)^2) <= radius

# add to raster and rename
values(shading) <- alpha
names(shading) <- "alpha"

# make a vector of the buffer circle
buffer_vect <- st_as_sf(buffer_circle_ortho) %>%
  st_transform(crs = crs_ortho) %>%
  terra::vect()

# use buffer to mask the shading raster
shading_masked <- shading %>%
  terra::crop(buffer_vect) %>%
  terra::mask(buffer_vect)

# convert raster to data frame
shading_df <- as.data.frame(shading_masked, xy = TRUE, na.rm = TRUE)

# showing the shading plot
ggplot(shading_df) +
  geom_raster(aes(x = x, y = y, alpha = alpha), fill = "black") +
  scale_alpha(range = c(0, 0.3)) +  # keep alpha stays within our desired range
  theme_void() +
  coord_fixed()

# make the inset plot
inset_map <- ggplot() +
  geom_sf(data = st_as_sf(buffer_circle_ortho), fill = NA, color = "white", linewidth = 2) +
  geom_sf(data = ocean_clip_ortho, fill = "#E3F1FE") +
  geom_sf(data = graticules_clip_ortho, colour = "grey50", linewidth = 0.2, alpha = 0.2) +
  geom_sf(data = countries_clip_ortho, fill = "#B1CBB7", colour = "#B1CBB7") +
  geom_raster(data = shading_df, aes(x = x, y = y, alpha = alpha), 
              fill = "black", interpolate = TRUE, show.legend = FALSE) +
  scale_alpha(range = c(0, 0.3)) +
  geom_sf(data = st_as_sf(buffer_circle_ortho), fill = NA, color = "grey50", linewidth = 0.05) +
  geom_sf(data = st_as_sfc(bbox),   #red focal area
          aes(geometry = geometry), fill = NA, colour = "red", linewidth = 0.5) +
  coord_sf(crs = crs_ortho) +
  theme_void() +
  NULL
inset_map

#---------------------------------#
# 3. Combine basemap and globe ####
#---------------------------------#

# combine basemap and inset map with some extra space on right
final_map <- basemap +
  plot_spacer() +
  plot_layout(widths = c(3, 1)) +
  inset_element(inset_map, 
                 left = -0.2,  right = 1, top = 0.7, bottom = 0, 
                 align_to = "full") +
  NULL
final_map

# save the map if needed
ggsave(final_map, filename = here("inset_globe.png"),
        width = 22, height = 15, 
        units = "cm", dpi = 300)
