library(readr)
library(tidyverse)

firsts_augmented <- read_csv("~/GitHub/am10.sg10/data/firsts_augmented.csv")
skim(firsts_augmented)
glimpse(firsts_augmented)

firsts_augmented <- firsts_augmented %>% 
  mutate(year_bins = as.numeric(as.character(
    cut(year, 
        breaks=c(min(year)-1,c(seq(1790, 2020, 10))), 
        labels=c(seq(1790, 2020, 10))))
  ))

# ======================================================
# PLOT NO SHADE

sex_count <- firsts_augmented %>%
  filter(!is.na(gender),
         !is.na(year_bins)) %>% 
  group_by(year_bins, gender) %>% 
  count() %>% 
  arrange(year_bins)


sex_count %>% 
  ggplot(aes(x = year_bins, y = n, color = gender)) +
  geom_line() +
  theme_minimal() +
  labs(title = "",
       subtitle = "",
       x = "Year",
       y = "Number of Firsts",
       caption = "") +
  NULL

# ======================================================
# PLOT SHADE

sex_count_wider <- sex_count %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  mutate(middle = ifelse(female>male,male,female))

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

# ======================================================
# Explanation

# Although African-Americans have growing achievements over the years, the gender
# gaps have also been broaden. In the two spikes of 1860s and 1940s, male number
# grew much more than female, and the difference between sex stays in following 
# years, which can be clearly demonstrated from the shades in the graph.
# However, entering 21th century, the gender equality has improved a lot.