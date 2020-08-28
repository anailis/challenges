library(tidyverse)
library(tidytuesdayR)
library(fmsb)
library(extrafont)

tuesdata <- tidytuesdayR::tt_load(2020, week = 34)
plants <- tuesdata$plants

#### barplot

threats_key <- c("Argiculture &\nAquaculture", "Biological\nResource Use", 
                 "Natural\nSystems\nModifications", "Commerical\nDevelopment", "Invasive\nSpecies", "Energy Production\n& Mining", 
             "Climate\nChange", "Human\nIntrusion", "Geological\nEvents", "Pollution", "Transport\nCorridors")
threats <- c("AA", "BRU", "NSM", "RCD", "ISGD", "EPM", 
             "CC", "HID", "GE", 
             "P", "TS")

proportion <- function(x) {
  return(sum(x)/n())
}

threat_world <- plants %>%
  summarize_at(colnames(plants)[6:16], proportion) %>%
  pivot_longer(colnames(plants)[6:16], names_to = "threat", values_to = "proportion") %>%
  arrange(proportion)

colorbars <- c(rep("grey", 4), "purple", "grey", "purple", "grey", "grey", "grey", "orange")

ggplot(aes(y = reorder(threat, proportion), x = proportion, fill = colorbars), data = threat_world) +
  geom_col() + 
  theme_minimal() + 
  labs(title = "WORLDWIDE, THE MOST COMMON THREAT TO EXTINCT PLANTS WAS",
       subtitle = "AGRICULTURE AND AQUACULTURE",
       x = "Proportion of Extinct Plants Threatened By ________") +
  xlim(c(0, 0.5)) +
  scale_y_discrete(labels = rev(threats_key)) + 
  scale_fill_manual(values = c("grey", "#725a8a", "#b04730"), guide = NULL) +
  theme(plot.title = element_text(hjust = 0.7, vjust = -100, size = 31, family = "Arial", face = "bold"),
        plot.subtitle = element_text(hjust = 0.8, vjust = -70, size = 45, colour = "#725a8a", family = "Arial", face = "bold"),
        axis.title.x = element_text(angle = 0, family = "Arial", size = 15),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 20),
        panel.grid.major = element_blank(),
        axis.line.x = element_line(color = "grey")) +
  geom_text(aes(label=rev(threats)), position=position_dodge(width=1), hjust=1.1, size = 10, colour = "white")
ggsave("week34_barplot.png", width = 21.875, height = 11.771, units = "in")

#### radar plots

threat_props <- plants %>% 
  group_by(continent) %>%
  summarize_at(c("threat_AA", "threat_BRU", "threat_NSM", "threat_RCD", "threat_ISGD",
                 "threat_EPM", "threat_CC", "threat_HID", "threat_GE", "threat_P", "threat_TS"), proportion)

createRadarPlots <- function(continent, data, palette) {
  props <- data[data$continent == continent, 2:12]
  props <- rbind(rep(1,12), rep(0,12), props)
  radarchart(props, axistype = 1, 
             cglcol="grey", cglty=1, axislabcol="grey",
             pcol=palette, pfcol=palette, plwd=4,
             vlabels = threats, vlcex = 2, palcex = 5)
  title(continent, line = -30, cex.main = 2.5)
  return(recordPlot())
}


par(mfrow=c(2, 4), mar=c(0.5, 0.5, 0.5, 0.5))
createRadarPlots(continent = "Europe", threat_props, rgb(0.012,0.207,0.012,0.5))
map(c("Africa", "Asia"), createRadarPlots, threat_props, rgb(0.137,0.033,0.181,0.5))
plot.new()
title(main = "Extinct plant species in\nAsia and Africa\nwere the worst affected\nby agriculture\nand aquaculture", line = -28, cex.main = 3)
abline(0.75, 0, col = "#725a8a", lwd = 20)
abline(0.25, 0, col = "#725a8a", lwd = 20)
plot.new()
title(main = "Invasive species and\nclimate change\nare major threats\nin North America,\nbut much less influential\nin other continents", line = -30, cex.main = 3)
abline(0.75, 0, col = "#b04730", lwd = 20)
abline(0.25, 0, col = "#b04730", lwd = 20)
createRadarPlots(continent = "North America", threat_props, rgb(0.255,0.0,0,0.5))
map(c("South America", "Oceania"), createRadarPlots, threat_props, rgb(0.012,0.207,0.012,0.5))

radar <- recordPlot()
