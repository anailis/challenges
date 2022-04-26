library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)

erasmus <- read_csv("erasmus_exchanges.csv", col_types = "cci")
world <- ne_countries()
world 

ui <- fluidPage(
  
  title = "Erasmus Mobility",
  titlePanel("Erasmus Mobility"),
  
  theme = shinytheme("superhero"),
  
  sidebarLayout(
    sidebarPanel(
           selectInput("country", h3("Country"),
                       choices = unique(erasmus$iso_a3.sending),
                       selected = "GBR")
    ),
    mainPanel(leafletOutput(outputId = "map"))
  )
)

server <- function(input, output) {

  foundational_map <- reactive({
    # need to create a slot in world that contains 
    # counts for any given sending country
    polygons <- subset(world, iso_a3 == input$country)
    leaflet(polygons) %>%
      addPolygons(
        weight = 1,
        opacity = 1.0,
        fillOpacity = 0.5,
        smoothFactor = 0.5,
        group = "base",
        highlightOptions = highlightOptions(
          color = "white", fillColor = "white",
          weight = 2, bringToFront = TRUE)
      )
  })
  
  output$map <- renderLeaflet({
    foundational_map()
  })
  
}

shinyApp(ui = ui, server = server)

# tasks 
# - highlight some countries in a different colour
# - 