library(pacman)

setwd('C:/Users/s1533194/Documents/Learning/Challenges/TidyTuesday')

p_load(tidytuesdayR, tidyverse, magrittr,
       tidytext, quanteda, fastDummies,
       GGally, jpeg, grid, waffle, extrafont,
       hrbrthemes, ggpubr, png)
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

# theme: what makes a trail good for kids?
## word cloud
## density graph
## graphic
## waffle chart

loadfonts(device = "win")

perc <- c(sum(hike_data_dummy$features_Good_for_kids), nrow(hike_data_dummy) - sum(hike_data_dummy$features_Good_for_kids))
waffle(c(6,10), use_glyph = "child")

# top 30 words
tokens_250 <- names(topfeatures(token_matrix, 250))
tm_250 <- token_matrix %>%
  dfm_select(pattern = tokens_250, selection = "keep")

# descriptions in trails good for kids
kids_matrix <- dfm(clean_tokens, groups = "features_Good_for_kids") %>%
  dfm_select(min_nchar = 2) %>% 
  dfm_trim(min_termfreq = 10) 

#png("week48_wrdcld.png")
wrdcld <- textplot_wordcloud(kids_matrix, comparison = TRUE, max_words = 50, color = c("#9CC1F8", "#3C8F7D"))
#dev.off()

# 
density <- ggplot(aes(x = highpoint, fill = as.factor(features_Good_for_kids)),
       data = hike_data_dummy) +
  geom_density(alpha = 0.8, color = "white") + 
  xlim(c(0,11000)) +
  theme_bw() + 
  theme(axis.text.y=element_blank(),
        axis.text.x = element_text(size = 15, color = "#355389", vjust = 8),
        axis.title = element_text(size = 20, colour = "#13375B"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x = element_text(vjust = 5),
        legend.position = c(0.8,0.8),
        legend.background = element_rect(color = "white"),
        legend.text = element_text(color = "#363636", size = 20, colour = "#13375B"),
        legend.title = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  xlab("Highest Point (feet)") + 
  ylab("Number\nof Trails") +
  scale_fill_manual(values = c("#9CC1F8", "#3C8F7D"),
                    lab = c("Unsuitable for Children", "Suitable for Children"))

ggsave('week48_density.png', density, units = "cm", width = 40, height = 20)


ggplot(aes(x = as.factor(features_Summits), fill = as.factor(features_Good_for_kids)),
       data = hike_data_dummy) +
  geom_bar() +
  theme_bw() + 
  theme(axis.text.y=element_blank(),
        axis.title.x = element_text(color = "#13375B"),
        legend.position = c(0.8,0.8),
        legend.background = element_rect(color = "white"),
        legend.text = element_text(color = "#363636", color = "#13375B"),
        legend.title = element_text(color = "#363636", color = "#13375B"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#212752"),
        panel.grid = element_line(color = "#4d5380")) + 
  scale_fill_manual(values = c("#bababa", "#575757"), name = "Trail Suitable\nfor Children?", 
                    lab = c("NO", "YES"))

img <- readJPEG("images/8355.jpg")
gimg <- rasterGrob(img, interpolate=TRUE)

imgplt <- ggplot() +
  annotation_custom(gimg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

wrdcld <- readPNG("wordcloud.png")
gwrdcld <- rasterGrob(wrdcld, interpolate=TRUE)

wrdcldplt <- ggplot() +
  annotation_custom(gwrdcld, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)


ggarrange(density, imgplt, wrdcldplt, nrow = 2, ncol = 2)
