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
library(readxl)
library(stringr)
library(ggnewscale)
library(gganimate)
library(animation)
library(transformr)

loadfonts(device="win")

# load data
firsts <- read_csv(here("../data/firsts_augmented.csv"), 
                   col_types = cols(year = col_integer(), 
                                    id_num = col_integer())) %>% 
  clean_names()

# glimpse at data
glimpse(firsts)


# load population data
# source: https://conservancy.umn.edu/handle/11299/181605
population_abs <- read_xlsx(here("../data/county2010_hist_pops.xlsx"), sheet = "c2010_hist_pops") %>% 
  clean_names()


population_abs <- population_abs %>% 
  pivot_longer(cols = epop1790:pop2010, names_to = "year", values_to = "pop") %>% 
  mutate(year = str_sub(year, -4, -1)) %>% 
  select(geoid10, year, pop)

# load population density data
# source: https://conservancy.umn.edu/handle/11299/181605
population_dens <- read_xlsx(here("../data/county2010_hist_pops.xlsx"), sheet = "densities") %>% 
  clean_names()


population_dens <- population_dens %>% 
  pivot_longer(cols = dens1790:dens2010, names_to = "year", values_to = "dens") %>% 
  mutate(year = str_sub(year, -4, -1))


# have densities and absolute numbers in same table:
population <- population_dens %>% 
  left_join(population_abs) %>% 
  mutate(dens_2 = cut(dens, breaks = c(-0.1, 2, 6, 18, 45, 90, max(dens)), ordered_result = TRUE)) %>% 
  mutate(year = as.integer(year))

# clean up
rm(population_abs, population_dens)


###############################################################################
## visualise data in a map
###############################################################################

# --> Story?! --> why and how did it change over time?

states_sf <- get_urbn_map("states", sf = TRUE)
states_sf$geometry

counties_sf <- get_urbn_map("counties", sf = TRUE)
states_sf$geometry

# transfrom geometry to 4326, or pairs of latitude/longitude numbers
states_sf <-  states_sf %>% 
  st_transform(4326) # transfrom to WGS84, latitude/longitude
states_sf$geometry

# transfrom geometry to 4326, or pairs of latitude/longitude numbers
counties_sf <-  counties_sf %>% 
  st_transform(4326) # transfrom to WGS84, latitude/longitude
counties_sf$geometry


# convert firsts to an sf object
firsts_sf <- firsts %>% 
  drop_na(lng, lat, gender) %>% 
  filter(country == "United States of America") %>% 
  # filter irrelevant lng/lat
  filter(lng > -140) %>% 
  st_as_sf(coords = c('lng', 'lat'), 
           crs = st_crs(states_sf))
firsts_sf$geometry


# join counties_sf and population data

counties_pop_sf <- counties_sf %>% 
  left_join(population, by = c("county_fips" = "geoid10"))


counties_pop_temp_sf <- counties_pop_sf %>% 
  filter(year %in% c(1900, 1950, 2000))



firsts_temp_sf <- firsts_sf %>% 
  mutate(year_2 = cut(year, 
                      breaks = c(min(year)-1, c(seq(1790, 2000, 10), 2020)), 
                      labels = c(seq(1790, 2000, 10), 2010))) %>% 
  mutate(year_2 = as.integer(levels(year_2))[year_2]) %>% 
  mutate(year_3 = year) %>%
  mutate(year = year_2) %>% 
  select(-year_2)
  


glimpse(firsts_temp_sf)


p <- ggplot(data = counties_pop_sf) +
  # draw polygons from counties shapefile
  geom_sf(aes(fill = dens_2), color = "#04314D") +
  scale_fill_manual(values = c("#e9eff2", "#93b4c4", "#57859c", "#2d627d", "#114966", "#04314D")) +
  transition_manual(year)

# animate(p, fps = 10)
# anim_save("pop_test.gif", animation = last_animation())





# try something with population density
p2 <- ggplot() +
  # draw polygons from counties shapefile
  geom_sf(data = counties_pop_sf, aes(fill = dens_2), color = "#04314D") +
  scale_fill_manual(values = c("#e9eff2", "#93b4c4", "#57859c", "#2d627d", "#114966", "#04314D")) +
  new_scale_fill() + 
  # add points from firsts shapefile
  geom_sf(
    data = firsts_temp_sf, aes(fill = gender), size = 3, shape = 21,
    show.legend = TRUE
  ) + 
  scale_fill_manual(values = c("#FF6426", "#F0C400")) +
  theme_minimal()+
  coord_sf(datum = NA) + #remove coordinates
  labs(title = "Year: {previous_frame} - {current_frame}") + 
  # hrbrthemes::theme_ft_rc(grid="", strip_text_face = "bold") +
  theme(axis.text = element_blank()) +
  # theme(strip.text = element_text(color = "white"))+
  transition_manual(year) + 
  NULL


animate(p2, fps = 4)
anim_save("pop_test.gif", animation = last_animation())









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

# 1861, # civil war
# 1901, # Washington, Carver & Du Bois + NAACP + UNIA + Harlem Renaissance
# 1941, # WWII begining and aftermath
# 1963, # i have a dream / civil rights movement +  Civil Rights Act of 1964 + Fair Housing Act, .....
# 1986, # Oprah Winfrey start + Los Angeles Riots + Million Man March
# 2008, # President Barack Obama

## Before Civil War:
firsts_sf_filtered <- firsts_sf %>% 
  filter(year < 1861)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 5, shape = 21,
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


## After Civil War - 1901:
firsts_sf_filtered <- firsts_sf %>% 
  filter(year >= 1861, year < 1901)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 5, shape = 21,
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



## 1901-1941:
firsts_sf_filtered <- firsts_sf %>% 
  filter(year >= 1901, year < 1941)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 5, shape = 21,
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

## 1941-1963:
firsts_sf_filtered <- firsts_sf %>% 
  filter(year >= 1941, year < 1963)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 5, shape = 21,
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



## 1963-1986:
firsts_sf_filtered <- firsts_sf %>% 
  filter(year >= 1963, year < 1986)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 5, shape = 21,
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




## 1986-2008:
firsts_sf_filtered <- firsts_sf %>% 
  filter(year >= 1986, year < 2008)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 5, shape = 21,
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




## 2008-:
firsts_sf_filtered <- firsts_sf %>% 
  filter(year >= 2008)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 5, shape = 21,
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



# Timeline combined with a lesson in History --> that's the story
# Source + Knowledge:
# https://www.history.com/topics/black-history/black-history-milestones

firsts %>% 
  ggplot(aes(x = year)) +
  geom_histogram(boundary = 1861, binwidth = 5, color = "white", fill = "#000461") +
  geom_vline(xintercept = 1861, # civil war
             color = "#AD8C00", size=1) +
  geom_vline(xintercept = 1901, # Washington, Carver & Du Bois + NAACP + UNIA + Harlem Renaissance
             color = "#AD8C00", size=1) +
  geom_vline(xintercept = 1941, # WWII begining and aftermath
             color = "#AD8C00", size=1) +
  geom_vline(xintercept = 1963, # i have a dream / civil rights movement +  Civil Rights Act of 1964 + Fair Housing Act, .....
             color = "#AD8C00", size=1) +
  geom_vline(xintercept = 1986, # Oprah Winfrey start + Los Angeles Riots + Million Man March
             color = "#AD8C00", size=1) +
  geom_vline(xintercept = 2008, # President Barack Obama
             color = "#AD8C00", size=1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL

# first plot
firsts %>% 
  ggplot(aes(x = year)) +
  geom_histogram(boundary = 1861, binwidth = 5, color = "white", fill = "#000461") +
  geom_vline(xintercept = 1861, # civil war
             color = "#AD8C00", size=1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL

firsts %>% 
  ggplot(aes(x = year)) +
  geom_histogram(boundary = 1861, binwidth = 5, color = "white", fill = "#000461") +
  geom_vline(xintercept = 1861, # civil war
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1901, # Washington, Carver & Du Bois + NAACP + UNIA + Harlem Renaissance
             color = "#AD8C00", size=1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL


firsts %>% 
  ggplot(aes(x = year)) +
  geom_histogram(boundary = 1861, binwidth = 5, color = "white", fill = "#000461") +
  geom_vline(xintercept = 1861, # civil war
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1901, # Washington, Carver & Du Bois + NAACP + UNIA + Harlem Renaissance
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1941, # WWII begining and aftermath
             color = "#AD8C00", size=1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL

firsts %>% 
  ggplot(aes(x = year)) +
  geom_histogram(boundary = 1861, binwidth = 5, color = "white", fill = "#000461") +
  geom_vline(xintercept = 1861, # civil war
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1901, # Washington, Carver & Du Bois + NAACP + UNIA + Harlem Renaissance
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1941, # WWII begining and aftermath
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1963, # i have a dream / civil rights movement +  Civil Rights Act of 1964 + Fair Housing Act, .....
             color = "#AD8C00", size=1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL

firsts %>% 
  ggplot(aes(x = year)) +
  geom_histogram(boundary = 1861, binwidth = 5, color = "white", fill = "#000461") +
  geom_vline(xintercept = 1861, # civil war
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1901, # Washington, Carver & Du Bois + NAACP + UNIA + Harlem Renaissance
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1941, # WWII begining and aftermath
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1963, # i have a dream / civil rights movement +  Civil Rights Act of 1964 + Fair Housing Act, .....
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1986, # Oprah Winfrey start + Los Angeles Riots + Million Man March
             color = "#AD8C00", size=1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL

firsts %>% 
  ggplot(aes(x = year)) +
  geom_histogram(boundary = 1861, binwidth = 5, color = "white", fill = "#000461") +
  geom_vline(xintercept = 1861, # civil war
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1901, # Washington, Carver & Du Bois + NAACP + UNIA + Harlem Renaissance
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1941, # WWII begining and aftermath
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1963, # i have a dream / civil rights movement +  Civil Rights Act of 1964 + Fair Housing Act, .....
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 1986, # Oprah Winfrey start + Los Angeles Riots + Million Man March
             color = "#AD8C00", size=1, alpha = 0.2) +
  geom_vline(xintercept = 2008, # President Barack Obama
             color = "#AD8C00", size=1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL



# something with gender --> maybe a statistical test here?
# something with category --> and also the interaction

firsts %>% 
  filter(!is.na(gender)) %>% 
  ggplot(aes(x = gender)) +
  geom_bar() +
  facet_wrap(~category, scales = "free")

firsts %>% 
  filter(!is.na(category)) %>% 
  ggplot(aes(x = category)) +
  geom_bar() +
  coord_flip()
