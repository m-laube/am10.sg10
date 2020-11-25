################################################################################
# In this file, we scrape and enrich the data for the African-American "firsts"
# we crawl several wikipedia pages using rvest
# we get geolocations using opencage
# we infer sex using the gender package
################################################################################

# load libraries
library(tidyverse)
library(vroom)
library(janitor)
library(sf)
library(here)
library(extrafont)
library(ggtext)
library(lubridate)
library(rvest)
library(parsedate)
library(opencage)
library(gender)


################################################################################
# SCRAPING FIRSTS
# Here we scrape African-American "firsts" from 
# https://en.wikipedia.org/wiki/List_of_African-American_firsts
# The code is inspired by 
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-06-09
################################################################################

first_url <- "https://en.wikipedia.org/wiki/List_of_African-American_firsts"

# read complete wikipedia page
raw_first <- read_html(first_url)

# function to extract the year of the firsts from the raw HTML code
get_year <- function(id_num){
  raw_first %>% 
    html_nodes(glue::glue("#mw-content-text > div > h4:nth-child({id_num}) > span.mw-headline")) %>% 
    html_attr("id")
}

# function to extract the complete lines of the "firsts" from the raw HTML code
get_first <- function(id_num){
  raw_first %>% 
    html_nodes(glue::glue("#mw-content-text > div > ul:nth-child({id_num}) > li")) %>% 
    lapply(function(x) x)
}

# function to extract the link to the wikipedia page of the person, that has achieved the "first"
extract_website <- function(x) {
  x %>% 
    str_replace(":.*?<a", ": <a") %>% 
    str_extract(": <a href=\\\".*?\\\"") %>% 
    str_extract("/wiki.*?\\\"") %>% 
    str_replace("\\\"", "")
}

# function to extract the concrete description of the "first"
extract_first <- function(x) {
  x %>% 
    str_extract("^First.*?:") %>% 
    str_replace(":", "")
}

# function to extract the name of the person, that has achieved the "first"
extract_name <- function(x) {
  x %>% 
    str_replace(":.*?<a", ": <a") %>% 
    str_extract(": <a href=\\\".*?\\\">") %>% 
    str_extract("title=.*\\\">") %>% 
    str_replace("title=\\\"", "") %>% 
    str_replace("\\\">", "")
}


# find years and complete lines of the "firsts"
raw_first_df <- tibble(id_num = 1:409) %>% 
  mutate(year = map(id_num, get_year),
         data = map(id_num, get_first))


# clean the parsed data
clean_first <- raw_first_df %>% 
  # save year as integer
  mutate(year = as.integer(year)) %>% 
  # fill empty year cells with the last existing value for year
  fill(year) %>% 
  unnest(data) %>%
  mutate(
         # get raw html string (with tags) for the complete lines of the "firsts" 
         data_string_raw = map_chr(data, toString),
         # get only html text (without tags) for the complete lines of the "firsts
         data_string_cle = map_chr(data, html_text)) %>% 
  
  mutate(
         # get the link to the wikipedia page of the person, that has achieved the "first"
         wiki = map_chr(data_string_raw, extract_website),
         # get the concrete description of the "first"
         first = map_chr(data_string_cle, extract_first),
         # get the name of the person, that has achieved the "first"
         name = map_chr(data_string_raw, extract_name))


################################################################################
# We also want to know, which kind of "first" was achieved,
# i.e. we want to map each "first" into a category.
# Hence, we define words that are indicators for specific categories.
# If we find such a word in the description of a "first", we categorize
# this "first" accordingly.
################################################################################

edu <- c(
  "practice", "graduate", "learning", "college", "university", "medicine",
  "earn", "ph.d.", "professor", "teacher", "school", "nobel", "invent", "patent",
  "medicine", "degree", "doctor", "medical", "nurse", "physician", "m.d.", "b.a.", "b.s.", "m.b.a",
  "principal", "space", "astronaut", "scientific") %>% 
  paste0(collapse = "|")

religion <- c("bishop", "rabbi", "minister", "church", "priest", "pastor", "missionary",
              "denomination", "jesus", "jesuits", "diocese", "buddhis", "cardinal") %>%
  paste0(collapse = "|")

politics <- c(
  "diplomat", "elected", "nominee", "supreme court", "legislature", "mayor", "governor",
  "vice President", "president", "representatives", "political", "department", "peace prize",
  "ambassador", "government", "white house", "postal", "federal", "union", "trade",
  "delegate", "alder", "solicitor", "senator", "intelligience", "combat", "commissioner",
  "state", "first lady", "cabinet", "advisor", "guard", "coast", "secretary", "senate",
  "house", "agency", "staff", "national committee", "lie in honor") %>%
  paste0(collapse = "|")

sports <- c(
  "baseball", "football", "basketball", "hockey", "golf", "tennis",
  "championship", "boxing", "games", "medal", "game", "sport", "olympic", "nascar",
  "coach", "trophy", "nba", "nhl", "nfl", "mlb", "stanley cup", "jockey", "pga",
  "race", "driver", "ufc", "champion", "highest finishing position") %>%
  paste0(collapse = "|")

military <- c(
  "serve", "military", "enlist", "officer", "army", "marine", "naval",
  "officer", "captain", "command", "admiral", "prison", "navy", "general",
  "force") %>%
  paste0(collapse = "|")

law <- c("american bar", "lawyer", "police", "judge", "attorney", "law", 
         "agent", "fbi") %>%
  paste0(collapse = "|")

arts <- c(
  "opera", "sing", "perform", "music", "billboard", "oscar", "television",
  "movie", "network", "tony award", "paint", "author", "book", "academy award", "curator",
  "director", "publish", "novel", "grammy", "emmy", "smithsonian",
  "conduct", "picture", "pulitzer", "channel", "villain", "cartoon", "tv", "golden globe",
  "comic", "magazine", "superhero", "pulitzer", "dancer", "opry", "rock and roll", "radio",
  "record") %>%
  paste0(collapse = "|")

social <- c("community", "freemasons", "vote", "voting", "rights", "signature", 
            "royal", "ceo", "community", "movement", "invited", "greek", "million",
            "billion", "attendant", "chess", "pilot", "playboy", "own", "daughter",
            "coin", "dollar", "stamp", "niagara", "pharmacist",
            "stock", "north pole", "reporter", "sail around the world", "sail solo around the world", "press", "miss ",
            "everest")  %>%
  paste0(collapse = "|")

# categorize "firsts" by looking for indicator words
first_df <- clean_first %>% 
  mutate(category = case_when(
    str_detect(tolower(first), military) ~ "Military",
    str_detect(tolower(first), law) ~ "Law",
    str_detect(tolower(first), arts) ~ "Arts & Entertainment",
    str_detect(tolower(first), social) ~ "Social & Jobs",
    str_detect(tolower(first), religion) ~ "Religion",
    str_detect(tolower(first), edu) ~ "Education & Science",
    str_detect(tolower(first), politics) ~ "Politics",
    str_detect(tolower(first), sports) ~ "Sports",
    TRUE ~ NA_character_
  )) %>% 
  drop_na() %>% 
  rename(accomplishment = first)

# clean up variables
rm(arts, edu, law, first_url, military, politics, religion, social, sports,
   extract_first, extract_name, extract_website, get_first, get_year, raw_first, raw_first_df, clean_first)


################################################################################
# AUGMENTING WITH BDAY AND LOCATION
# We no parse the wikipedia page of the persons, that have achieved the "firsts"
# to get information about their location and birthday
################################################################################

# function to get the location and birthday given a wikipedia link
extract_bday_location <- function(wiki){
  
  # read complete wikipedia page
  html <- read_html(paste0("https://en.wikipedia.org", wiki))
  
  # extract birthdays
  
  bd <- html %>% 
    html_node(glue::glue('span[class="bday"]')) %>% 
    html_text()
  
  if(is.na(bd)){
    
    bd <- html %>% 
      html_node("table.vcard") %>% 
      toString() %>% 
      str_replace_all("\\n", "") %>% 
      str_extract("Born.*?</td>") %>% 
      str_extract("<td>.*?<") %>% 
      str_replace("<td>", "") %>% 
      str_replace("<", "")
    
    bd <- bd %>% 
      str_replace("\\(.*", "") %>% 
      str_trim() %>% 
      str_replace_all("[^[:alnum:] ]", "") %>% 
      # convert parsed birthday string to a date
      parse_date(approx = TRUE)
  }
  
  if(!is.na(bd)){
    bd <- toString(bd)
  }
  
  
  # extract locations
  
  lo <- html %>% 
    html_node("table.vcard") %>% 
    toString() %>% 
    str_replace_all("\\n", "") %>% 
    str_extract("Born.*?</td>") %>% 
    str_replace("Born</th>", "")
  
  if(length(str_locate_all(lo, "<br>")[[1]]) == 4){
    lo <- str_replace(lo, "<br>", "")
  }
  
  lo <- lo %>% str_extract("<br><.*?</td>$")
  
  if(!is.na(lo)){
    lo <- lo %>% 
      read_html() %>% 
      html_text()
  }
  
  return(list(bd, lo))
  
}


# extract birthday and location and store them in new columns
first_df_augmented <- suppressMessages(first_df %>% 
                                         # extract birthday and location form wikipedia
                                         mutate(combi = map(wiki, extract_bday_location)) %>% 
                                         # unnest birthday and location into separate columns
                                         unnest_wider(combi) %>% 
                                         # rename new columns
                                         rename(bday = `...1`, location = `...2`) %>% 
                                         mutate(bday = map_chr(bday, function(x) x))) %>% 
  # it is possible that there was a mistake with the birthday --> filter those out
  mutate(bday = ifelse(year(bday) == 2020, NA_character_, bday))


################################################################################
# AUGMENTING WITH GENDER
# In this step we try to infer the sex of a person, that has achieved a "first"
################################################################################

# add gender

# see if we find words in the description of the "first", that identify gender
first_df_augmented <- first_df_augmented %>% 
  mutate(gender = if_else(str_detect(data_string_cle, 
                                     "\\swoman\\s|\\sWoman\\s|\\sher\\s|\\sshe\\s|\\sfemale\\s"), 
                          "female", 
                  if_else(str_detect(data_string_cle, 
                                     "\\sman\\s|\\sMan\\s|\\shim\\s|\\she\\s|\\smale\\s"), 
                          "male", 
                          "idk")))


# use gender package as second source of info (parse first name)
# input: full name and the year of the "first"
get_gender <- function(name, year){
  
  # get first name
  name <- strsplit(name, split = " ")[[1]][1]
  
  # define right method (see ?gender)
  method = ifelse(year < 1930, "ipums", "ssa")
  
  # get the gender
  ret <- gender(name, method = method, countries = "United States") %>% 
    select(gender) %>% 
    pull()
  
  if(typeof(ret) == "logical"){
    return(NA_character_)
  }
  else{
    return(ret)
  }
  
}


# build final "gender" column
first_df_augmented <- first_df_augmented %>% 
  # use first name and year to infer gender
  mutate(gender_2 = map2(name, year, get_gender)) %>% 
  mutate(gender_2 = map_chr(gender_2, function(x) x)) %>%
  # combine both gender columns into one final column
  mutate(gender = if_else(gender != "idk", gender, gender_2)) %>% 
  select(-gender_2)


################################################################################
# AUGMENTING WITH GEOLOCATION
# To be able to visualise our findings using maps, we need the geolocations
# We use the opencage package to get lng/lat
################################################################################

# add lng / lat, i.e. geocode locations

opencage_custom <- function(x) {
  if(is.na(x)){
    return(NA_character_)
  }
  else{
    return(opencage_forward(x, limit = 1))
  }
}

first_df_augmented <- suppressMessages(first_df_augmented %>% 
                                         # get information from opencage
                                         mutate(location_geo = map(location, opencage_custom)) %>% 
                                         
                                         # parse and clean the information from opencage
                                         unnest_wider(location_geo) %>% 
                                         unnest(results, keep_empty = TRUE) %>% 
                                         rename(lat = geometry.lat,
                                                lng = geometry.lng,
                                                country = components.country,
                                                state = components.state,
                                                county = components.county,
                                                city = components.city,
                                                FIPS_state = annotations.FIPS.state) %>% 
                                         select(id_num, year, data_string_raw, data_string_cle, wiki, 
                                                accomplishment, name, category, bday, gender, 
                                                location, lat, lng, country, state, county, city, FIPS_state))

# write the file as a csv
# write_csv(first_df_augmented, path = "firsts_augmented.csv")

