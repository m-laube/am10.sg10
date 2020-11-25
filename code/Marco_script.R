# load libraries
library(tidyverse)
library(vroom)
library(janitor)
library(sf)
library(here)
library(extrafont)
library(ggtext)
library(lubridate)
library(urbnmapr)

loadfonts(device="win")

# load data
firsts <- read_csv(here("../data/firsts_augmented.csv"), 
                   col_types = cols(year = col_integer(), 
                                    id_num = col_integer())) %>% 
  clean_names()

# glimpse at data
glimpse(firsts)

###############################################################################
## visualise data in a map
###############################################################################

states_sf <- get_urbn_map("states", sf = TRUE)
states_sf$geometry

# transfrom geometry to 4326, or pairs of latitude/longitude numbers
states_sf <-  states_sf %>% 
  st_transform(4326) # transfrom to WGS84, latitude/longitude
states_sf$geometry

# convert firsts to an sf object
firsts_sf <- firsts %>% 
  drop_na(lng, lat, gender) %>% 
  filter(country == "United States of America") %>% 
  # filter irrelevant lng/lat
  filter(lng > -140) %>% 
  st_as_sf(coords = c('lng', 'lat'), 
           crs = st_crs(states_sf))
firsts_sf$geometry

firsts %>%
  select(lng) %>% 
  distinct() %>% 
  arrange(lng)

  
ggplot() +
    # draw polygons from states shapefile
    geom_sf(data = states_sf, fill = "grey99", color = "black")+
    # add points from firsts shapefile
    geom_sf(
      data = firsts_sf, aes(fill = gender), size = 2, shape = 21,
      show.legend = TRUE
    ) + 
    scale_fill_manual(values = c("#000461", "#AD8C00")) +
    theme_minimal()+
    coord_sf(datum = NA) + #remove coordinates
    labs(title = "Most achievers were born in the East!") +
    # hrbrthemes::theme_ft_rc(grid="", strip_text_face = "bold") +
    theme(axis.text = element_blank()) +
    # theme(strip.text = element_text(color = "white"))+
    NULL


# count how many achievers were born in each state
states_sf <- states_sf %>%
  mutate(count = lengths(
    st_contains(states_sf, 
                firsts_sf))) 
  

ggplot(data = states_sf, aes(fill = count)) +
  geom_sf(color = "#04314D") +
  scale_fill_gradient(low = "#ffffff", high = "#04314D", name = NULL) +
  theme_minimal()+
  coord_sf(datum = NA) + #remove coordinates
  labs(title = "Most achievers were born in the East!") +
  # hrbrthemes::theme_ft_rc(grid="", strip_text_face = "bold") +
  theme(axis.text = element_blank()) +
  NULL



