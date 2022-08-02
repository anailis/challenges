library(moveVis)
library(lubridate)
library(terra)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2022-08-02')
nrow(tuesdata$frogs)

tuesdata$frogs$SurveyDate <- as.POSIXct.Date(mdy(tuesdata$frogs$SurveyDate))
sum(is.na(tuesdata$frogs$SurveyDate))

utm_coords <- tuesdata$frogs[,c("UTME_83", "UTMN_83")]
utm_coords <- as.matrix(rename(utm_coords, "x" = "UTME_83", "y" = "UTMN_83"))
utm_coords <- vect(utm_coords, crs="+proj=utm +zone=10T +datum=WGS84  +units=m")

long_lat <- project(utm_coords, "+proj=longlat +datum=WGS84")
long_lat <- geom(long_lat)[, c("x", "y")]
head(long_lat, 3)

frogs <- cbind(long_lat, tuesdata$frogs)

frog_moves <- df2move(frogs, 
        proj = CRS("+proj=longlat +datum=WGS84"),
        x = "x",
        y = "y",
        time = "SurveyDate",
        track_id = "Frequency")

frog_moves <- align_move(frog_moves, 
                         res = 1,
                         unit = "days")

frames <- frames_spatial(frog_moves, 
                         map_service = "osm", 
                         alpha = 0.5,
                         path_legend = FALSE) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(frog_moves, type = "label") %>%
  add_progress()

animate_frames(frames, out_file = "frogmovements.gif", overwrite = TRUE)
