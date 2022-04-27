library(htmltools)
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

help_text <- "Click a country to view the Erasmus destinations of students from that nation"

# 2014 - 2019
ui <- fluidPage(
  
  theme = shinytheme("lumen"),
  titlePanel("Where do Erasmus students go on exchange?"),
  
  fluidRow(
    column(12,
      mainPanel(leafletOutput("map"))
    ),
    column(6,
      textOutput("selected_country"),
      a("Data: data.europa.eu", 
        href = "https://data.europa.eu/data/datasets/erasmus-mobility-statistics-2014-2019-v2")
    ),
    column(6,
      actionButton(
      inputId = "clear",
      icon = icon(name = "eraser"),
      label = "Reset map"
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$selected_country <- renderText({ help_text })

  foundational_map <- reactive({
    # need to create a slot in world that contains 
    # counts for any given sending country
    leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = 0, lat = 45, zoom = 2.5) %>%
      addPolygons(
        data = world,
        weight = 1,
        group = "base",
        layerId = world$name,
        label = world$name,
        fillOpacity = 1,
        fillColor = "#5f8c6b",
        color = "black",
        highlightOptions = highlightOptions(
          color = "white", 
          weight = 2, bringToFront = TRUE)
      )
  })
  
  output$map <- renderLeaflet({
    foundational_map()
  })
  
  observeEvent(input$map_shape_click, {
    
      click <- input$map_shape_click
      output$selected_country <- renderText({ paste("Showing Erasmus destinations for students from", click$id) })  
        
      sending_polygon <- world[which(world$name == click$id),]
      receiving_polygons <- world[which(world$name != click$id),]
      
      countries <- data.frame(name = receiving_polygons$name)

      receiving_polygons$participants <- exchanges %>%
        filter(country_name.sending == click$id) %>%
        filter(country_name.receiving != click$id) %>%
        dplyr::select(country_name.receiving, total_participants) %>%
        right_join(countries, by = c("country_name.receiving" = "name")) %>%
        pull(total_participants)
      receiving_polygons$labels <- paste(
        receiving_polygons$name,
        paste("Students received:",
        receiving_polygons$participants),
        sep = "<br>"
      )
      
      pal <- colorNumeric(
        palette = "Oranges",
        domain = receiving_polygons$participants)
      
      leafletProxy("map") %>%
        addPolygons(
          data = receiving_polygons,
          layerId = receiving_polygons$name,
          fillColor = ~pal(receiving_polygons$participants),
          weight = 1,
          fillOpacity = 1,
          color = "black",
          label = lapply(receiving_polygons$labels, htmltools::HTML),
          highlightOptions = highlightOptions(
            color = "white", 
            weight = 2, bringToFront = TRUE)
        ) %>%
        addPolygons(
          data = sending_polygon,
          layerId = "sending",
          weight = 2,
          fillColor = "#5f8c6b",
          fillOpacity = 1,
          color = "black"
        ) 
  })
  
  observeEvent(input$clear, {
    output$map <- renderLeaflet({
      foundational_map()
    })
    output$selected_country <- renderText({ help_text })
  })
  
}

shinyApp(ui = ui, server = server)