#############################################################|
#--- Using {eks} for calculating spatial overlap of KDEs ---#
#############################################################|

#Personal disclaimer: This script is a work in progress and has not been fully tested/verified!

# Load packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(here, tidyverse, sf, eks, lwgeom, terra, tidyterra, rnaturalearth, rnaturalearthdata, rnaturalearthhires) 

# Calculate some random spatial GPS data (lat-lon) for two groups
set.seed(123) #for consistent results
n <- 100 #number of samples per group
spatial_data <- tibble(
  id = rep(1:2, each = n),
  year = rep(2010:2011, each = n),
  lon = c(rnorm(n, mean = 18, sd = 0.5), rnorm(n, mean = 19, sd = 0.5)),
  lat = c(rnorm(n, mean = -33, sd = 0.5), rnorm(n, mean = -32, sd = 0.5))) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Calculate KDEs for plotting
spatial_skde <- spatial_data %>%
  group_by(id) %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(50,95), disjoint = FALSE) #disjoint turns off russian doll kdes

# Plot 50% and 95% KDEs by year
ggplot(data = spatial_skde, aes(geometry = geometry, group = id)) + 
  #geom_sf(data = basemap, size = 0.3, fill = "grey70", colour = NA, inherit.aes = FALSE) + #background map
  geom_sf(aes(fill = contlabel, colour = contlabel)) +
  scale_fill_manual(values = c("50" = "#E41A1CB3", "95" = NA), na.value = NA) + #fill 50s red
  scale_colour_manual(values = c("50" = NA, "95" = "#E41A1CB3"), na.value = NA) + #colour 95s red
  labs(x = "Longitude", y = "Latitude", fill = "KDE", colour = "KDE") +
  #facet_wrap(~ year) +
  theme_bw(base_size = 9) +
  theme(strip.background =element_rect(fill="grey95"))

#turn spherical geometry off - temp fixing when using unprojected data
sf::sf_use_s2(FALSE)

# Calculate spatial intersection (95% KDE)
spatial_intersection <- spatial_skde %>% filter(contlabel == "95") %>%
  sf::st_intersection() %>% filter(n.overlaps > 1)
 
# Calculate combined overlap (95% KDE)
spatial_overlap <- spatial_skde %>% filter(contlabel == "95") %>%
  sf::st_union()

# Calculate proportional overlap (total area divided by intersecting area)
prop_overlap <- (sf::st_area(spatial_intersection) / sf::st_area(spatial_overlap)) * 100

#Do final plot as above but including the intersection with a different colour and add the prop_overlap as text in figure
ggplot(data = spatial_skde, aes(geometry = geometry, group = id)) + 
  geom_sf(aes(fill = contlabel, colour = contlabel)) +
  geom_sf(data = spatial_intersection, fill = "black", colour = "black", alpha = 0.5) +
  geom_text(x = 19.75, y = -34.0, aes(label = paste("KDE overlap", round(prop_overlap, 2), "%")), colour = "black", size = 3.5) +
  scale_fill_manual(values = c("50" = "#E41A1CB3", "95" = NA), na.value = NA) +
  scale_colour_manual(values = c("50" = NA, "95" = "#E41A1CB3"), na.value = NA) +
  labs(x = "Longitude", y = "Latitude", fill = "KDE", colour = "KDE") +
  theme_bw(base_size = 9) +
  theme(strip.background =element_rect(fill="grey95"))
