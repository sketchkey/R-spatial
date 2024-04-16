##################################|
#--- Using the {gfwr} package ---#
##################################|

#---------------------#
# PREP ENVIRONMENT ####
#---------------------#

# Check/install required core packages
if(!require(pacman))install.packages("pacman")
if(!require(devtools))install.packages("devtools")

# Install and load packages required (including gfwr)
devtools::install_github("GlobalFishingWatch/gfwr") #install {gfwr} with devtools
pacman::p_load(usethis, gfwr, tidyverse, sf, eks, rnaturalearth) #p_load will install and load packages

# Adding a GFW API token to your R environment (4 steps)
#1) Generate a GWF API token (https://globalfishingwatch.org/our-apis/tokens)
#2) Open the .Renviron file with the edit_r_environ function from {usethis}
usethis::edit_r_environ("project") # open .Renviron file (remove "project" to open global file instead)
#3) Copy-paste the line below into your .Renviron file (without '#' and with your token inside the quotations)
#GFW_TOKEN = "PASTE_YOUR_TOKEN_HERE" 
#4) Save and close the edited .Renviron file and restart the R session

#-------------------#
# GET VESSEL IDs ####
#-------------------#

# Extract vessel ID's from GFW dataset
SA_vessels <- get_vessel_info(query = "flag = 'ZAF' AND geartype = 'other_purse_seines'",
                              search_type = "advanced", 
                              dataset = "fishing_vessel",
                              #key = key #only need to use this if you've not set the API key in your .Renviron
                              )

# Summarise list of vessels extracted from GFW
message("In the GFW dataset of SA's 'other-purse-seine' vessels, there are ",length(unique(SA_vessels$id))," vessel ID's, of which ",
        length(unique(SA_vessels$shipname)), " have publicly available names" )

# Collapse vessel ID's into a single string for passing to get_event
SA_vessel_ids <- paste0(SA_vessels$id, collapse = ',')

#-----------------------#
# GET FISHING EVENTS ####
#-----------------------#

# Extract fishing from full dataset (earliest date: 2014-04-18)
SA_fishing <- get_event(event_type = 'fishing',
                        vessel = SA_vessel_ids, #using the list of vessel ID's made previously
                        start_date = "2014-01-01", #earliest for the dataset
                        end_date = "2022-12-31",
                        #key = key #only use if you've not set the API key in .Renviron
                        )

# Turn the nested SA_fishing dataset into something plottable
SA_fishing_all <- SA_fishing %>% 
  dplyr::select(-c(regions, boundingBox, event_info)) %>% #remove excess columns
  unnest_wider(col=c(vessel), names_sep = "_") %>% #extract specific columns from nested data
  mutate(year = lubridate::year(start)) %>% #create year column for start of fishing event
  st_as_sf(coords = c('lon', 'lat'), crs=4326, remove=FALSE) #turn lat-lon (start point?) into sf geometry

# Summarise vessels in fishing dataset
message("In the fishing dataset there are ",length(unique(SA_fishing_all$vessel_id))," vessel ID's, of which ",
        length(unique(SA_fishing_all$vessel_name)), " have publicly available names" )

#-----------------------#
# PLOT FISHING EVENTS ####
#-----------------------#

# Extract yearly sample sizes
samplesizes <- SA_fishing_all %>% 
  st_drop_geometry() %>%
  group_by(year) %>% 
  tally()

# Create simple basemap
basemap <- rnaturalearth::ne_countries(type = 'countries', scale = 10, returnclass = "sf") %>%
  filter(continent == "Africa")

# Calculate KDEs for plotting
skde_fishing <- SA_fishing_all %>%
  select(id, year, lon, lat) %>%
  group_by(year) %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(50,95))

# Plot 50% and 95% KDEs by year
ggplot(data = skde_fishing, aes(geometry = geometry, group = year)) + 
  geom_sf(data = basemap, size = 0.3, fill = "grey70", colour = NA, inherit.aes = FALSE) + #background map
  geom_sf(aes(fill = contlabel, colour = contlabel)) +
  scale_fill_manual(values = c("50" = "#E41A1CB3", "95" = NA), na.value = NA) + #fill 50s red
  scale_colour_manual(values = c("50" = NA, "95" = "#E41A1CB3"), na.value = NA) + #colour 95s red
  geom_text(data = samplesizes, aes(label=paste0('n = ', n), x = Inf, y = Inf), #sample sizes
            vjust = 2, hjust = 1.2, size = 2, colour = "white", inherit.aes = FALSE) +
  coord_sf(xlim = c(16, 20.99), ylim = c(-35.5, -31), expand=FALSE) + #widest view
  labs(x = "Longitude", y = "Latitude", fill = "KDE", colour = "KDE") +
  facet_wrap(~ year) +
  theme_bw(base_size = 9) +
  theme(strip.background =element_rect(fill="grey95"))

#--------------------#
# GET PORT VISITS ####
#--------------------#

# Extract port visits from full dataset
SA_portvisits <- get_event(event_type = 'port_visit',
                           vessel = SA_vessel_ids,
                           #key = key
                           )

# Turn the nested SA_ports dataset into something plottable
SA_ports_all <- SA_portvisits %>%
  select(-c(regions, boundingBox, distances)) %>% #remove some unnecessary columns
  unnest_wider(col=c(vessel,event_info), names_sep = "_") %>% #unnest vessel and event_info
  select(-c(event_info_intermediateAnchorage)) %>% #removing intermediate anchorage (though could check start/end are same anchorage)
  unnest_wider(col=c(event_info_startAnchorage,event_info_endAnchorage), names_sep = "_") %>% #final unnest of remaining anchorage columns
  rename_with(stringr::str_replace, 
              pattern = "event_info_", replacement = "", 
              matches("event_info_"))

message("In the port visits dataset there are ",length(unique(SA_ports_all$vessel_id))," vessel ID's, of which ",
        length(unique(SA_ports_all$vessel_name)), " have publicly available names" )
  