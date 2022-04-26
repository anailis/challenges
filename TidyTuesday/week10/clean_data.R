library(tidyverse)
library(tidytuesdayR)
library(rnaturalearth)
library(rnaturalearthdata)

tuesdata <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tuesdata$erasmus

head(erasmus)
dim(erasmus)
names(erasmus)

# I noticed that there are a lot of trips that are don't actually involve 
# moving to a different country
erasmus <- erasmus %>%
  mutate(no_move = sending_country_code == receiving_country_code)
# these can also have long durations, I really don't know what they represent
# I couldn't find an explanation online
# for now I just remove them
ggplot(data = erasmus) +
  geom_histogram(aes(x = mobility_duration, fill = no_move),
                 bins = 50) + 
  theme_bw()
erasmus <- erasmus %>%
  filter(!no_move)
dim(erasmus)

country_codes <- unique(c(erasmus$sending_country_code, 
                          erasmus$receiving_country_code))
# 54 unique country codes
length(country_codes)

world <- ne_countries(scale = "medium", 
                       returnclass = "sf")

# 51 of the 54 codes in the Erasmus dataset are identified 
# in the world dataset
rep_country_codes <- world %>%
  filter(iso_a2 %in% country_codes) %>%
  pull(iso_a2)

# countries not identified: 
## the UK (should be GB)
## Greece (should be GR)
## Kosovo (is not an ISO standard country, so has NA in world)
setdiff(country_codes, rep_country_codes)

# fix UK and Greece in erasmus dataframe
erasmus <- erasmus %>%
  mutate(across(contains("country_code"), ~ str_replace(., "UK", "GB"))) %>%
  mutate(across(contains("country_code"), ~ str_replace(., "EL", "GR")))

country_codes <- unique(c(erasmus$sending_country_code, 
                          erasmus$receiving_country_code))

# fix Kosovo in the world dataframe
world <- world %>%
  mutate(iso_a2 = ifelse(name == "Kosovo", "XK", iso_a2)) %>%
  mutate(erasmus = (iso_a2 %in% c(country_codes, "GB", "GR")))
sum(world$erasmus)

sent <- erasmus %>%
  full_join(world %>% select(iso_a2, iso_a3), 
            by = c("sending_country_code" = "iso_a2")) %>%
  full_join(world %>% select(iso_a2, iso_a3), 
            by = c("receiving_country_code" = "iso_a2"),
            suffix = c(".sending", ".receiving")) 

erasmus_iso_a3 <- world %>%
  filter(erasmus) %>%
  filter(!is.na(iso_a3)) %>%
  pull(iso_a3)
erasmus_iso_a3 <- c(erasmus_iso_a3, "XKX")
length(erasmus_iso_a3)

sent <- sent %>%
  # use .drop = FALSE and levels = country_codes to include combinations with no counts 
  # e.g. UK and Russia
  mutate(across(contains("iso_a3"), ~ factor(., levels = unique(world$iso_a3)))) %>%
  group_by(iso_a3.sending, iso_a3.receiving, .drop = FALSE) %>%
  summarise(total_participants = sum(participants), .groups = "keep") %>%
  ungroup() %>%
  mutate(
    total_participants = ifelse(iso_a3.receiving %in% erasmus_iso_a3 & iso_a3.sending %in% erasmus_iso_a3,
                                total_participants,
                                NA)
    )

sent %>%
  select(iso_a3.sending, iso_a3.receiving, total_participants) %>%
  write_csv("erasmus_exchanges.csv")
