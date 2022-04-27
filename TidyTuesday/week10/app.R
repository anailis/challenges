library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)

exchanges <- read_csv("erasmus_exchanges.csv", col_types = "cci") %>%
  drop_na()
erasmus_countries <- read_lines("erasmus_countries.txt")

world <- ne_countries(scale = "medium")
world <- world[world$name %in% erasmus_countries,]

ui <- fluidPage(
  
  title = "Erasmus Mobility",
  titlePanel("Erasmus Mobility"),
  mainPanel(leafletOutput("map"))
  
)


server <- function(input, output, session) {

  output$map <- renderLeaflet({
    # need to create a slot in world that contains 
    # counts for any given sending country
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 45, zoom = 2) %>%
      addPolygons(
        data = world,
        weight = 0.5,
        group = "base",
        layerId = world$name,
        label = world$name,
        fillColor = "red",
        highlightOptions = highlightOptions(
          color = "white", fillColor = "white",
          weight = 2, bringToFront = TRUE)
      )
  })
  
  observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      
      sending_polygon <- world[which(world$name == click$id),]
      receiving_polygons <- world[which(world$name != click$id),]
      
      countries <- data.frame(name = receiving_polygons$name)

      receiving_polygons$participants <- exchanges %>%
        filter(country_name.sending == click$id) %>%
        filter(country_name.receiving != click$id) %>%
        select(country_name.receiving, total_participants) %>%
        right_join(countries, by = c("country_name.receiving" = "name")) %>%
        pull(total_participants)
      
      pal <- colorNumeric(
        palette = "Blues",
        domain = receiving_polygons$participants)
      
      leafletProxy("map") %>%
        addPolygons(
          data = receiving_polygons,
          layerId = receiving_polygons$name,
          fillColor = ~pal(receiving_polygons$participants),
          weight = 0.5,
          fillOpacity = 1,
          highlightOptions = highlightOptions(
            color = "white", fillColor = "white",
            weight = 2, bringToFront = TRUE)
        ) %>%
        addPolygons(
          data = sending_polygon,
          layerId = "sending",
          weight = 2,
          fillColor = "red"
        ) 
    }
  )
  
}

shinyApp(ui = ui, server = server)

# tasks 
# - highlight some countries in a different colour
# - 