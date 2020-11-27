library(pacman)

p_load(tidytuesdayR, tidyverse, magrittr,
       tidytext, quanteda, fastDummies)

tuesdata <- tidytuesdayR::tt_load(2020, week = 48)
hike_data <- tuesdata$hike_data

hike_data %<>% 
  mutate(
    id = 1:nrow(hike_data),
    rating = as.numeric(rating),
    highpoint = as.numeric(highpoint),
    gain = as.numeric(gain),
    length = as.numeric(str_extract(length, "\\d+.\\d+"))
  ) 

hike_data_dummy <- hike_data %>%
  unnest(features, keep_empty = TRUE) %>%
  mutate(features = as.factor(features)) %>%
  dummy_cols(select_columns = 'features') %>% 
  select(-(name:description)) %>% 
  group_by(id) %>% 
  summarise_all(sum) %>%
  inner_join(hike_data, by = 'id') %>%
  select(-features_NA)

names(hike_data_dummy) <- map_chr(names(hike_data_dummy), str_replace_all, " ", "_")

# extract key words from descriptions
head(hike_data$description)

# one hot encode from features
head(hike_data$features)

hike_data %<>% 
  unnest(features)
