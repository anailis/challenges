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
  filter(!no_move) %>%
  # ignore if either receiving/sending country is NA
  filter(!is.na(sending_country_code) & !is.na(receiving_country_code))
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
length(rep_country_codes)

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
  mutate(iso_a3 = ifelse(name == "Kosovo", "XKX", iso_a3)) %>%
  mutate(erasmus = (iso_a2 %in% c(country_codes, "GB", "GR")))
sum(world$erasmus)

code_dict <- data.frame(iso_a2 = world$iso_a2, 
                        iso_a3 = world$iso_a3,
                        country_name = world$name) %>%
  drop_na()

erasmus_country_names <- world %>%
  filter(erasmus) %>%
  pull(name)
length(erasmus_country_names)

sent <- erasmus %>%
  left_join(code_dict, 
            by = c("sending_country_code" = "iso_a2")) %>%
  left_join(code_dict, 
            by = c("receiving_country_code" = "iso_a2"),
            suffix = c(".sending", ".receiving")) 

sent <- sent %>%
  # use .drop = FALSE and levels = country_codes to include combinations with no counts 
  # e.g. UK and Russia
  mutate(across(contains("iso_a3"), ~ factor(., levels = unique(world$iso_a3)))) %>%
  group_by(iso_a3.sending, iso_a3.receiving, .drop = FALSE) %>%
  summarise(total_participants = sum(participants), .groups = "keep") %>%
  ungroup()
  
sent <- sent %>%
  left_join(code_dict, by = c("iso_a3.sending" = "iso_a3")) %>%
  left_join(code_dict, by = c("iso_a3.receiving" = "iso_a3"),
            suffix = c(".sending", ".receiving")) %>%
  # if a country is not in erasmus, total_participants should be NA not 0
  mutate(
    total_participants = ifelse(country_name.sending %in% erasmus_country_names & country_name.receiving %in% erasmus_country_names,
                                total_participants,
                                NA)
  )

write_csv(sent, "erasmus_exchanges.csv")
write_lines(erasmus_country_names, "erasmus_countries.txt")
