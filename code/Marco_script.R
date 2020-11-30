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
library(tidyquant)

loadfonts(device="win")

# TODO: build story around the histogram & around the maps
# TODO: tune the visualisations to look great
# TODO: maybe some confidence intervals, time series analysis?!

# Story 1: connection of historic events and african-american "firsts"

# Story 2: Most achievers were born in the east --> connection with population
# density and "going west"!

# Story 3: Gender Gap?!

# we need at least 3 different chart types!
# map, histogram (see below)
# additionally boxplots / confidence intervals for category / gender?
# time series charts?

# presentation: 8-10 slides of main findings

###############################################################################
## Load and transform data
###############################################################################

# load "firsts" data and clean column names
firsts <- read_csv(here("../data/firsts_augmented.csv"), 
                   col_types = cols(year = col_integer(), 
                                    id_num = col_integer())) %>% 
  clean_names()

# glimpse at data
glimpse(firsts)

# cut year into buckets for gganimate
firsts <- firsts %>% 
  mutate(year_2 = cut(year, 
                      breaks = c(min(year)-1, c(seq(1790, 2020, 10))), 
                      labels = c(seq(1790, 2020, 10)))) %>% 
  mutate(year_2 = as.integer(levels(year_2))[year_2]) %>% 
  mutate(year_full = year) %>%
  mutate(year = year_2) %>% 
  select(-year_2)


# load population data from 1790-2010 and clean names
# source: https://conservancy.umn.edu/handle/11299/181605
population_abs <- read_xlsx(here("../data/county2010_hist_pops.xlsx"), sheet = "c2010_hist_pops") %>% 
  clean_names()

# bring data into long format and clean the year
population_abs <- population_abs %>% 
  pivot_longer(cols = epop1790:pop2010, names_to = "year", values_to = "pop") %>% 
  mutate(year = str_sub(year, -4, -1)) %>% 
  select(geoid10, year, pop)

# load population density data from 1790-2010 and clean names
# source: https://conservancy.umn.edu/handle/11299/181605
population_dens <- read_xlsx(here("../data/county2010_hist_pops.xlsx"), sheet = "densities") %>% 
  clean_names()

# bring data into long format and clean the year
population_dens <- population_dens %>% 
  pivot_longer(cols = dens1790:dens2010, names_to = "year", values_to = "dens") %>% 
  mutate(year = str_sub(year, -4, -1))


# have densities and absolute numbers in one table
population <- population_dens %>% 
  left_join(population_abs) %>% 
  # cut density into buckets (otherwise some with values of over 2000, 
  # others with under 2 --> not good for visualisation)
  mutate(dens_2 = cut(dens, 
                      breaks = c(-0.1, 2, 6, 18, 45, 90, max(dens)),
                      labels = c("[0, 2]", 
                                 "(2, 6]", 
                                 "(6, 18]", 
                                 "(18, 45]", 
                                 "(45, 90]", 
                                 "90+"),
                      ordered_result = TRUE)) %>% 
  mutate(year = as.integer(year))

# we only have population data until 2010. However, we also have "firsts" in the period of 2010-2020.
# to be able to animate this properly with gganimate, we will duplicate the 2010 values and set them
# as values for 2020. We will end up with population data for 1790 until 2020.
# To say it clear: we assume that there are no changes in population from 2010 to 2020.

pop_2020 <- population %>% 
  filter(year == 2010) %>% 
  mutate(year = 2020)

population <- bind_rows(population, pop_2020) %>% 
  arrange(geoid10, year)


# clean up
rm(population_abs, population_dens, pop_2020)


###############################################################################
## Load and Transform Shapefiles
###############################################################################

# --> Story?! --> why and how did it change over time?

# load states shapefile
states_sf <- get_urbn_map("states", sf = TRUE)
states_sf$geometry

# load counties shapefile
counties_sf <- get_urbn_map("counties", sf = TRUE)
counties_sf$geometry


# transfrom geometry to 4326, or pairs of latitude/longitude numbers
states_sf <-  states_sf %>% 
  st_transform(4326) # transfrom to WGS84, latitude/longitude
states_sf$geometry

# transfrom geometry to 4326, or pairs of latitude/longitude numbers
counties_sf <-  counties_sf %>% 
  st_transform(4326) # transfrom to WGS84, latitude/longitude
counties_sf$geometry


# join counties_sf and population data
counties_pop_sf <- counties_sf %>% 
  left_join(population, by = c("county_fips" = "geoid10"))


# convert firsts to a sf object
firsts_sf <- firsts %>% 
  drop_na(lng, lat, gender) %>% 
  filter(country == "United States of America") %>% 
  # filter irrelevant lng/lat
  filter(lng > -140) %>% 
  st_as_sf(coords = c('lng', 'lat'), 
           crs = st_crs(states_sf))
firsts_sf$geometry

# convert firsts to a sf object with jitter on coordinates
# jitter points such that they are better visible
set.seed(100)
firsts_sf_jitter <- firsts %>% 
  drop_na(lng, lat, gender) %>% 
  mutate(lng = jitter(lng, amount = 1),
         lat = jitter(lat, amount = 1)) %>% 
  filter(country == "United States of America") %>% 
  # filter irrelevant lng/lat
  filter(lng > -140) %>% 
  st_as_sf(coords = c('lng', 'lat'), 
           crs = st_crs(states_sf))
firsts_sf_jitter$geometry


###############################################################################
## visualise complete data in one map
###############################################################################

# original

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

# jittered

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_jitter, aes(fill = gender), size = 2, shape = 21,
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


###############################################################################
## visualise data in a map --> gganimate
###############################################################################

# visualise population density over time
p <- ggplot(data = counties_pop_sf) +
  # draw polygons from counties shapefile
  geom_sf(aes(fill = dens_2), color = "#04314D") +
  scale_fill_manual(values = c("#e9eff2", "#93b4c4", "#57859c", "#2d627d", "#114966", "#04314D")) +
  transition_manual(year)

# animate(p, fps = 10)
# anim_save("pop_test.gif", animation = last_animation())


# visualise population density and locations of "firsts" over time
p2 <- ggplot() +
  # draw polygons from counties shapefile
  geom_sf(data = counties_pop_sf, aes(fill = dens_2), color = "#04314D") +
  scale_fill_manual(values = c("#e9eff2", "#93b4c4", "#57859c", "#2d627d", "#114966", "#04314D")) +
  new_scale_fill() + 
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_jitter, aes(fill = gender), size = 3, shape = 21,
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


animate(p2, fps = 2)
# anim_save("pop_test.gif", animation = last_animation())


###############################################################################
## visualise data over time in a map
###############################################################################

# 1861, # civil war
# 1901, # Washington, Carver & Du Bois + NAACP + UNIA + Harlem Renaissance
# 1941, # WWII begining and aftermath
# 1963, # i have a dream / civil rights movement +  Civil Rights Act of 1964 + Fair Housing Act, .....
# 1986, # Oprah Winfrey start + Los Angeles Riots + Million Man March
# 2008, # President Barack Obama

## Before Civil War:
firsts_sf_filtered <- firsts_sf_jitter %>% 
  filter(year_full < 1861)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 3, shape = 21,
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
firsts_sf_filtered <- firsts_sf_jitter %>% 
  filter(year_full >= 1861, year_full < 1901)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 3, shape = 21,
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
firsts_sf_filtered <- firsts_sf_jitter %>% 
  filter(year_full >= 1901, year_full < 1941)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 3, shape = 21,
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
firsts_sf_filtered <- firsts_sf_jitter %>% 
  filter(year_full >= 1941, year_full < 1963)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 3, shape = 21,
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
firsts_sf_filtered <- firsts_sf_jitter %>% 
  filter(year_full >= 1963, year_full < 1986)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 3, shape = 21,
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
firsts_sf_filtered <- firsts_sf_jitter %>% 
  filter(year_full >= 1986, year_full < 2008)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 3, shape = 21,
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
firsts_sf_filtered <- firsts_sf_jitter %>% 
  filter(year_full >= 2008)

ggplot() +
  # draw polygons from states shapefile
  geom_sf(data = states_sf, fill = "grey99", color = "black")+
  # add points from firsts shapefile
  geom_sf(
    data = firsts_sf_filtered, aes(fill = gender), size = 3, shape = 21,
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


###############################################################################
## visualise achievers density in a map
###############################################################################


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


###############################################################################
## Look at timeline and connect it with American History
###############################################################################

# Timeline combined with a lesson in History --> that's the story
# Source + Knowledge:
# https://www.history.com/topics/black-history/black-history-milestones

firsts %>% 
  ggplot(aes(x = year_full)) +
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
  ggplot(aes(x = year_full)) +
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
  ggplot(aes(x = year_full)) +
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
  ggplot(aes(x = year_full)) +
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
  ggplot(aes(x = year_full)) +
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
  ggplot(aes(x = year_full)) +
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
  ggplot(aes(x = year_full)) +
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


###############################################################################
## Analysis of gender and category
###############################################################################

# something with gender --> maybe a statistical test here?
# something with category --> and also the interaction

firsts %>% 
  filter(!is.na(gender)) %>% 
  ggplot(aes(x = gender)) +
  geom_bar() +
  facet_wrap(~category, scales = "free") +
  theme_minimal()

firsts %>% 
  filter(!is.na(category)) %>% 
  ggplot(aes(x = category)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()


###############################################################################
## Try to use some statistical and/or ML modelling techniques
###############################################################################

# time series analysis --> number of "firsts" per year

# cut year into same buckets as histogram above
firsts_test <- firsts %>% 
  mutate(year_2 = cut(year_full, 
                      breaks = c(min(year_full)-1, c(seq(1751, 2021, 5))), 
                      labels = c(seq(1751, 2021, 5)),
                      ordered_result = TRUE)) %>% 
  mutate(year_2 = as.integer(levels(year_2))[year_2]) #%>% 
  #mutate(year_full = year) %>%
  #mutate(year = year_2) %>% 
  #select(-year_2)

firsts_test <- firsts_test %>% 
  count(year_2)

empty_values <- tribble(
  ~year_2, ~n,
  1756,   0,
  1766,   0,
  1791,   0,
  1801,   0,
  1811,   0,
)

firsts_test <- bind_rows(firsts_test, empty_values) %>% 
  arrange(year_2)

rm(empty_values)

# Use local polynomial regression fitting
# LOESS = locally estimated scatterplot smoothing

firsts_test %>% 
  ggplot(aes(x = year_2, y = n)) +
  geom_col() +
  #geom_line(color = "#000461", size = 2) +
  #geom_ma(ma_fun = EMA, n = 5, linetype = "solid", color = "#000461", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "#000461", size = 2) + 
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL


# time series decomposition

library(forecast)

firsts_test_ts <- ts(firsts_test$n, start = 1751, end = 2021, deltat = 5)

ts(firsts_test$n, deltat = 1/2) %>% 
  decompose("additive") %>% 
  autoplot()

stl(firsts_test_ts_2)

# confidence intervals for male / female per category



###############################################################################
# Gender Gap over Time
###############################################################################

sex_count <- firsts %>%
  filter(!is.na(gender),
         !is.na(year_bins)) %>% 
  group_by(year_bins, gender) %>% 
  count() %>% 
  arrange(year_bins)

# add missing year 1820
sex_count_1820 <- sex_count %>% 
  filter(year_bins == 1800) %>% 
  mutate(year_bins = as.integer(1820),
         n = as.integer(0))

sex_count <- bind_rows(sex_count, sex_count_1820)

rm(sex_count_1820)

sex_count_wider <- sex_count %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  mutate(female = if_else(is.na(female), as.integer(0), female))

sex_count <- sex_count_wider %>% 
  pivot_longer(female:male, names_to = "gender", values_to = "n")


sex_count_wider <- sex_count_wider %>% 
  mutate(middle = ifelse(female > male,male,female)) %>% 
  mutate(ribbon_helper = if_else(year_bins > 1790, if_else(female > male, male, female), NA_integer_),
         male_helper = if_else(year_bins > 1790, male, NA_integer_),
         female_helper = if_else(year_bins > 1790, female, NA_integer_),
         min = if_else(male > female, female, male),
         min_helper = as.integer(0),
         min = if_else(year_bins > 1790, min, NA_integer_),
         min_helper = if_else(year_bins > 1790, min_helper, NA_integer_))


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

sex_count_wider %>% 
  ggplot(aes(x = year_bins)) +
  
  geom_step(data = sex_count, aes(x = year_bins, y = n, color = gender), size = 1, direction = "mid") +
  
  scale_color_manual(values = c("#D6AF00", "#4D0B0C"), name = "Gender") +
  
  geom_stepribbon(aes(ymin = ribbon_helper, ymax = male_helper, group = 1), 
                  fill = "red3", 
                  alpha = 0.1, 
                  position = position_nudge(x = -5)) +
  
  geom_stepribbon(aes(ymin = female_helper, ymax = ribbon_helper, group = 1), 
                  fill = "green4", 
                  alpha = 0.1, 
                  position = position_nudge(x = -5)) +
  
  geom_stepribbon(aes(ymin = min_helper, ymax = min, group = 1), 
                  fill = "gray20", 
                  alpha = 0.1, 
                  position = position_nudge(x = -5)) +
  
  geom_tile(aes(x = 2017.5, width = 4.5, y = 20.5, height = 6.5), alpha = 0.0058, fill = "red3") +
  
  geom_tile(aes(x = 1792.5, width = 4.5, y = 4, height = 2), alpha = 0.0058, fill = "red3") +
  
  geom_tile(aes(x = 2017.5, width = 5, y = 8.35, height = 16.9), fill = "gray92") +
  
  geom_tile(aes(x = 1792.5, width = 4.85, y = 1.35, height = 2.9), fill = "gray92") +
  
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot") +
  labs(title = "Gender Gap persists over Time",
       subtitle = "Number of \"Firsts\" by Gender",
       x = "Year",
       y = "Number of Firsts",
       caption = "") +
  NULL

sex_count_wider %>% 
  ggplot(aes(x = year_bins)) +
  geom_line(aes(y = female), group = 1, size = 1, color = "pink") +
  geom_line(aes(y = male), group = 1, size = 1, color = "skyblue") +
  geom_ribbon(aes(ymin = middle, ymax = male, group = 1), fill = "grey50", alpha = 0.2) +
  geom_ribbon(aes(ymin = middle, ymax = female, group = 1), fill = "grey100", alpha = 0.2) +
  theme_minimal() +
  labs(title = "Gender gaps have grown over the years",
       subtitle = "First achievements by gender",
       x = "Year",
       y = "Number of Firsts",
       caption = "") +
  NULL


################################################################################

sex_count <- firsts %>%
  filter(!is.na(gender),
         !is.na(year_bins_2)) %>% 
  group_by(year_bins_2, gender) %>% 
  count() %>% 
  ungroup()

# add missing years
sex_count_missing <- tribble(
  ~year_bins_2, ~gender, ~n,
  1765, "male", as.integer(0),
  1780, "male", as.integer(0), 
  1790, "male", as.integer(0), 
  1800, "male", as.integer(0), 
  1810, "male", as.integer(0), 
  1815, "male", as.integer(0), 
  1820, "male", as.integer(0), 
  1830, "male", as.integer(0), 
  1835, "male", as.integer(0)
)

sex_count <- bind_rows(sex_count, sex_count_missing) %>% 
  arrange(year_bins_2)

rm(sex_count_1820)

sex_count_wider <- sex_count %>% 
  pivot_wider(names_from = gender, values_from = n)

sex_count_wider <- sex_count_wider %>% 
  mutate(female = if_else(is.na(female), as.integer(0), female)) %>% 
  mutate(middle = if_else(female > male, male, female))

sex_count_wider %>% 
  ggplot(aes(x = year_bins_2)) +
  geom_line(aes(y = female), group = 1, size = 1, color = "pink") +
  geom_line(aes(y = male), group = 1, size = 1, color = "skyblue") +
  geom_ribbon(aes(ymin = middle, ymax = male, group = 1), fill = "grey50", alpha = 0.2) +
  geom_ribbon(aes(ymin = middle, ymax = female, group = 1), fill = "grey100", alpha = 0.2) +
  labs(title = "Gender gaps have grown over the years",
       subtitle = "First achievements by gender",
       x = "Year",
       y = "Number of Firsts",
       caption = "") +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL


firsts %>% 
  count(year_bins_2) %>% 
  ggplot(aes(x = year_bins_2, y = n)) +
  geom_line(size = 1, color = "#04314D") + 
  geom_vline(xintercept = 1861, # civil war
             color = "#D6AF00", size=1) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Number of \"Firsts\" over Time",
       subtitle = "Civil War") +
  NULL

sex_count_wider %>% 
  ggplot(aes(x = year_bins_2)) +
  geom_line(aes(y = female), group = 1, size = 1, color = "#D6AF00") +
  geom_line(aes(y = male), group = 1, size = 1, color = "#4D0B0C") +
  geom_ribbon(aes(ymin = middle, ymax = male, group = 1), fill = "grey50", alpha = 0.2) +
  geom_ribbon(aes(ymin = middle, ymax = female, group = 1), fill = "grey100", alpha = 0.2) +
  labs(title = "Gender Gap persists over Time",
       subtitle = "Number of \"Firsts\" by Gender",
       x = "Year",
       y = "Number of Firsts",
       caption = "") +
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  NULL
