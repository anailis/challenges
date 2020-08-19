library(tidyverse)
library(tidytuesdayR)
library(fmsb)
library(cowplot)
library(gridGraphics)
library(extrafont)

tuesdata <- tidytuesdayR::tt_load(2020, week = 34)
plants <- tuesdata$plants

threats <- c("Argiculture &\nAquaculture", "Biological\nResource\nUse", "Commercial\nDevelopement", "Invasive\nSpecies", "Energy\nProduction", "Climate\nChange", 
             "Human\nIntrusion", "Pollution", "Transportation\nCorridor", 
             "Natural\nSystem\nModifications", "Geological\nEvents")

proportion <- function(x) {
  return(sum(x)/n())
}

threat_world <- plants %>%
  summarize_at(colnames(plants)[6:16], proportion) %>%
  pivot_longer(colnames(plants)[6:16], names_to = "threat", values_to = "proportion") %>%
  arrange(proportion)

colorbars <- c(rep("rest", 10), "top")

bars <- ggplot(aes(x = reorder(threat, -proportion), y = proportion, fill = colorbars), data = threat_world) +
  geom_col() + 
  theme_minimal() + 
  labs(title = "WORLDWIDE, THE MOST COMMON THREAT TO EXTINCT PLANTS WAS",
       subtitle = "AGRICULTURE AND AQUACULTURE",
       y = "Proportion of\nExtinct Plants\nThreatened By\n________") +
  ylim(c(0, 0.5)) +
  scale_x_discrete(labels = threats) + 
  scale_fill_manual(values = c("grey", "#548554"), guide = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, family = "Arial"),
        plot.subtitle = element_text(hjust = 0.8, size = 15, colour = "#548554", family = "Arial", face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, family = "Arial"),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_line(color = "grey"))


threat_props <- plants %>% 
  group_by(continent) %>%
  summarize_at(colnames(plants)[6:16], proportion)

createRadarPlots <- function(continent, data) {
  props <- data[data$continent == continent, 2:12]
  props <- rbind(rep(1,12), rep(0,12), props)
  par(mar=c(0.5, 0, 0.5, 0))
  radarchart(props, axistype = 1, 
             cglcol="grey", cglty=1, axislabcol="grey",
             pcol=rgb(0.012,0.207,0.012,0.5) , pfcol=rgb(0.012,0.207,0.012,0.5), plwd=4,
             vlabels = threats)
  title(continent, line = -20)
  return(recordPlot())
}

par(mfrow=c(2, 3))
map(unique(plants$continent), createRadarPlots, threat_props)
webs <- recordPlot()
