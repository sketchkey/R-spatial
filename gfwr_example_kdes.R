##################################|
#--- Using the {gfwr} package ---#
##################################|

# Go to any bounding box map maker 
# (e.g., https://boundingbox.klokantech.com/)
# choose an area that you want to map and copy the coordinates as text/csv to paste below
your_bbox <- c(16, -35.5, 20.99, -31) # <<< paste your copied coordinates inside these brackets

#---------------------#
# PREP ENVIRONMENT ####
#---------------------#

# Check/install required core packages
if(!require(pacman))install.packages("pacman")
if(!require(devtools))install.packages("devtools")

# Install and load packages required (including gfwr)
devtools::install_github("GlobalFishingWatch/gfwr") #install {gfwr} with devtools
pacman::p_load(here, usethis, gfwr, tidyverse, #core packages
               sf, eks, rnaturalearth, #spatial
               patchwork) #plotting

# Adding a GFW API token to your R environment (4 steps)
#1) Generate a GWF API token (https://globalfishingwatch.org/our-apis/tokens)
#2) Open the .Renviron file with the edit_r_environ function from {usethis}
usethis::edit_r_environ("project") # open .Renviron file (remove "project" to open global file instead)
#3) Copy-paste the line below into your .Renviron file (without '#' and with your token inside the quotations)
#GFW_TOKEN = "PASTE_YOUR_TOKEN_HERE" 
#4) Save and close the edited .Renviron file and restart the R session

#-----------------------#
# GET FISHING EVENTS ####
#-----------------------#
bbox <- your_bbox %>% # <<< paste your copied coordinates inside these brackets
  setNames(c("xmin", "ymin", "xmax", "ymax")) %>% 
  sf::st_bbox(crs = 4326) |>
  sf::st_as_sfc() |>
  sf::st_as_sf()

bbox_4326 <- st_bbox(bbox) #for plotting

# extract all fishing events within bbox for last two years
fishing_all <- gfwr::get_event(event_type = "FISHING",
                               start_date = "2023-11-03",
                               end_date = "2024-01-31",
                               region = bbox,
                               region_source = "USER_SHAPEFILE")

#------------------------#
# PLOT FISHING EVENTS ####
#------------------------#

# Extract yearly sample sizes
samplesizes <- fishing_all %>% 
  mutate(year = lubridate::year(start)) |>
  group_by(year) %>% 
  tally()

# Create simple basemap
basemap <- rnaturalearth::ne_countries(type = 'countries', 
                                       scale = 10, 
                                       returnclass = "sf") |>
  filter(continent == "Africa")

# Calculate KDEs for plotting
skde_fishing <- fishing_all %>%
  mutate(year = lubridate::year(start)) %>%
  select(eventId, year, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  group_by(year) %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(50,95))

# Plot 50% and 95% KDEs by year
skde_map <- ggplot(data = skde_fishing, aes(geometry = geometry, group = year)) + 
  geom_sf(data = basemap, size = 0.3, fill = "grey70", colour = NA, inherit.aes = FALSE) + #background map
  geom_sf(aes(fill = contlabel, colour = contlabel)) +
  scale_fill_manual(values = c("50" = "#E41A1CB3", "95" = NA), na.value = NA) + #fill 50s red
  scale_colour_manual(values = c("50" = NA, "95" = "#E41A1CB3"), na.value = NA) + #colour 95s red
  geom_text(data = samplesizes, 
            aes(label=paste0('n = ', n), x = Inf, y = Inf), #sample sizes
            vjust = 2, hjust = 1.2, size = 2, colour = "white", inherit.aes = FALSE) +
  coord_sf(
    xlim = c(bbox_4326["xmin"], bbox_4326["xmax"]),
    ylim = c(bbox_4326["ymin"], bbox_4326["ymax"]),
    expand = TRUE
  ) +
  labs(x = "Longitude", y = "Latitude", fill = "KDE", colour = "KDE") +
  facet_wrap(~ year) +
  theme_bw(base_size = 9) +
  theme(strip.background =element_rect(fill="grey95"))

# save the map if needed
ggsave(skde_map, filename = here("plots","gfwr_example_kdes.png"),
       width = 18, height = 10, 
       units = "cm", dpi = 300)
