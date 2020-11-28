library(pacman)

p_load(tidytuesdayR, tidyverse, magrittr,
       tidytext, quanteda, fastDummies,
       GGally)
tuesdata <- tidytuesdayR::tt_load(2020, week = 48)
hike_data <- tuesdata$hike_data

hike_data %<>% 
  mutate(
    id = 1:nrow(hike_data),
    rating = as.numeric(rating),
    highpoint = as.numeric(highpoint),
    gain = as.numeric(gain),
    location = as.factor(str_extract(location, ".*(?= --)|.*")),
    length = as.numeric(str_extract(length, "\\d+.\\d+"))
  ) 

hike_data_dummy <- hike_data %>%
  unnest(features, keep_empty = TRUE) %>%
  dummy_cols(select_columns = 'features') %>%
  select(-(name:description)) %>% 
  replace(is.na(.), 0) %>%
  group_by(id) %>% 
  summarise_all(sum) %>%
  inner_join(hike_data, by = 'id') %>%
  select(-features_NA,features)

names(hike_data_dummy) <- map_chr(names(hike_data_dummy), str_replace_all, " |/", "_")

hike_pairs <- hike_data %>% 
  select(-c(description,features,name,location,id))

# note that 292 trails have a rating of 0
# is this possibly because they haven't ever been rated?
# I don't trust this variable
ggpairs(hike_pairs)
sum(hike_pairs$rating==0)

location_means <- hike_data_dummy %>%
  group_by(location) %>%
  summarise_if(is.numeric, mean)
ggpairs(location_means)

hike_data_dummy %>%
  group_by(location) %>%
  summarise_if(is.factor, sum)

# extract key words from descriptions
descriptions <- corpus(hike_data_dummy$description, 
                       docvars = hike_data_dummy)
raw_tokens <- tokens(descriptions, remove_punct = T)
clean_tokens <- tokens_select(raw_tokens, 
                              pattern = stopwords(language = "en"),
                              selection = "remove")
token_matrix <- dfm(clean_tokens) %>%
  dfm_select(min_nchar = 2) %>% 
  dfm_trim(min_termfreq = 10) 
cotokens <- fcm(clean_tokens)

# top 30 words
tokens_250 <- names(topfeatures(token_matrix, 250))
tm_250 <- token_matrix %>%
  dfm_select(pattern = tokens_250, selection = "keep")

# descriptions in trails good for kids
kids_matrix <- dfm(clean_tokens, groups = "features_Good_for_kids") %>%
  dfm_select(min_nchar = 2) %>% 
  dfm_trim(min_termfreq = 10) 
textplot_wordcloud(loc_matrix, comparison = TRUE, max_words = 50)
