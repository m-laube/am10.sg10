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

# in this file, we scrape the data for the "firsts"
# we crawl several wikipedia pages, use opencage to find geolocations,
# and the gender package to infer sex


### RESCRAPING FIRSTS ###

# code adapted from https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-06-09

first_url <- "https://en.wikipedia.org/wiki/List_of_African-American_firsts"

raw_first <- read_html(first_url)

get_year <- function(id_num){
  raw_first %>% 
    html_nodes(glue::glue("#mw-content-text > div > h4:nth-child({id_num}) > span.mw-headline")) %>% 
    html_attr("id")
}

get_first <- function(id_num){
  raw_first %>% 
    html_nodes(glue::glue("#mw-content-text > div > ul:nth-child({id_num}) > li")) %>% 
    lapply(function(x) x)
}

extract_website <- function(x) {
  x %>% 
    str_replace(":.*?<a", ": <a") %>% 
    str_extract(": <a href=\\\".*?\\\"") %>% 
    str_extract("/wiki.*?\\\"") %>% 
    str_replace("\\\"", "")
}

extract_first <- function(x) {
  x %>% 
    str_extract("^First.*?:") %>% 
    str_replace(":", "")
}

extract_name <- function(x) {
  x %>% 
    str_replace(":.*?<a", ": <a") %>% 
    str_extract(": <a href=\\\".*?\\\">") %>% 
    str_extract("title=.*\\\">") %>% 
    str_replace("title=\\\"", "") %>% 
    str_replace("\\\">", "")
}

raw_first_df <- tibble(id_num = 1:409) %>% 
  mutate(year = map(id_num, get_year),
         data = map(id_num, get_first))

clean_first <- raw_first_df %>% 
  mutate(year = as.integer(year)) %>% 
  fill(year) %>% 
  unnest(data) %>% 
  mutate(data_string_raw = map_chr(data, toString),
         data_string_cle = map_chr(data, html_text)) %>% 
  mutate(wiki = map_chr(data_string_raw, extract_website),
         first = map_chr(data_string_cle, extract_first),
         name = map_chr(data_string_raw, extract_name))

edu <- paste0(c(
  "practice", "graduate", "learning", "college", "university", "medicine",
  "earn", "ph.d.", "professor", "teacher", "school", "nobel", "invent", "patent",
  "medicine", "degree", "doctor", "medical", "nurse", "physician", "m.d.", "b.a.", "b.s.", "m.b.a",
  "principal", "space", "astronaut", "scientific"
), collapse = "|")

religion <- c("bishop", "rabbi", "minister", "church", "priest", "pastor", "missionary",
              "denomination", "jesus", "jesuits", "diocese", "buddhis", "cardinal") %>%
  paste0(collapse = "|")

politics <- c(
  "diplomat", "elected", "nominee", "supreme court", "legislature", "mayor", "governor",
  "vice President", "president", "representatives", "political", "department", "peace prize",
  "ambassador", "government", "white house", "postal", "federal", "union", "trade",
  "delegate", "alder", "solicitor", "senator", "intelligience", "combat", "commissioner",
  "state", "first lady", "cabinet", "advisor", "guard", "coast", "secretary", "senate",
  "house", "agency", "staff", "national committee", "lie in honor"
) %>%
  paste0(collapse = "|")

sports <- c(
  "baseball", "football", "basketball", "hockey", "golf", "tennis",
  "championship", "boxing", "games", "medal", "game", "sport", "olympic", "nascar",
  "coach", "trophy", "nba", "nhl", "nfl", "mlb", "stanley cup", "jockey", "pga",
  "race", "driver", "ufc", "champion", "highest finishing position"
) %>%
  paste0(collapse = "|")

military <- c(
  "serve", "military", "enlist", "officer", "army", "marine", "naval",
  "officer", "captain", "command", "admiral", "prison", "navy", "general",
  "force"
) %>%
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




### AUGMENTING WITH BDAY AND LOCATION ###

extract_bday_location <- function(wiki){
  
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


first_df_augmented <- suppressMessages(first_df %>% 
                                         mutate(combi = map(wiki, extract_bday_location)) %>% 
                                         unnest_wider(combi) %>% 
                                         rename(bday = `...1`, location = `...2`) %>% 
                                         mutate(bday = map_chr(bday, function(x) x))) %>% 
  mutate(bday = ifelse(year(bday) == 2020, NA_character_, bday))




### AUGMENTING WITH GENDER ###

# add gender

# see if we find words in the description that identify gender
first_df_augmented <- first_df_augmented %>% 
  mutate(gender = if_else(str_detect(data_string_cle, 
                                     "\\swoman\\s|\\sWoman\\s|\\sher\\s|\\sshe\\s|\\sfemale\\s"), 
                          "female", 
                          if_else(str_detect(data_string_cle, 
                                             "\\sman\\s|\\sMan\\s|\\shim\\s|\\she\\s|\\smale\\s"), 
                                  "male", 
                                  "idk")))


# use gender package as second source of info (parse first name)
get_gender <- function(name, year){
  
  # get first name
  name <- strsplit(name, split = " ")[[1]][1]
  
  method = ifelse(year < 1930, "ipums", "ssa")
  
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


first_df_augmented <- first_df_augmented %>% 
  mutate(gender_2 = map2(name, year, get_gender)) %>% 
  mutate(gender_2 = map_chr(gender_2, function(x) x)) %>% 
  mutate(gender = if_else(gender != "idk", gender, gender_2)) %>% 
  select(-gender_2)




### AUGMENTING WITH GEOLOCATION ###

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
                                         mutate(location_geo = map(location, opencage_custom)) %>% 
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

write_csv(first_df_augmented, path = "firsts_augmented.csv")

