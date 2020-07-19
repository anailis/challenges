library(tidyverse)
library(plotly)
library(shiny)
library(shinythemes)

df <- read.csv("Week29.csv", header = T, stringsAsFactors = F)

df[['VI']] <- as.numeric(gsub("\\%", "", df[['VI']]))
df[['NI']] <- as.numeric(gsub("\\%", "", df[['NI']]))

df <- mutate(df, diff = VI - NI)

df <- pivot_longer(df, c('VI', 'NI'), names_to = 'Religion', values_to = 'Acceptance')

df[['Religion']] <- as.factor(df[['Religion']])
df[['Country']] <- as.factor(df[['Country']])

levels(df[['Religion']]) <- c('Not important', 'Very important')

ui <- fluidPage(theme = shinytheme("superhero"),
                
  title = "Religion and Acceptance of Homosexuality",
  
  plotlyOutput(outputId = "plot"),

  fluidRow(
    column(6,
           h4("Religion & Acceptance of Homosexuality"),
           p("Pew Research asked individuals whether or not they thought homosexuality should be accepted in society. Out of 34 countries polled, 25 showed significantly higher rates of acceptance of homosexuality in individuals who reported that religion was not very important to them compared to those who reported that religion was very important to them. This visualisation allows you to compare countries of interest, by either clicking the country names in the legend to add/remove them from the graph or filtering countries by the difference in opinion between respondants who thought religion was very important and those that did not.",
             style = "font-size:13px;")
    ),
    column(6,
      wellPanel(
        sliderInput(inputId = "change",
                    label = "Difference between the % of people who think that homosexuality should be accepted by society and do not think religion is very important, and the % of people who think homosexuality should be accepted by society and do think religion is very important",
                    min = -39,
                    max = -8,
                    value = c(-39,-8))
      )
    ),
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    df <- filter(df, diff < input$change[2] & diff > input$change[1])
    
     plot <- ggplot(aes(x = Religion, y = Acceptance, color = Country), data = df) +
      geom_point(shape = 23, fill = "white") + 
      geom_path(aes(group = Country), alpha = 0.45) +
      theme_bw() +
      ylab('% who say homosexuality\nshould be accepted by society') + 
      ylim(c(0,100)) +
      scale_x_discrete(labels=c('Religion is\nNOT very important', 'Religion is\nvery important')) + 
      theme(axis.title.x = element_blank(), 
            plot.background = element_rect(fill = "transparent"), 
            text = element_text(colour = "white"),
            axis.text = element_text(color = "white"),
            panel.background = element_rect(fill = "#717E8E"),
            legend.background = element_rect(fill = "transparent")) +
            scale_colour_hue(l = 70, c = 150)
      
      ggplotly(plot)
        
    
  })
  
  
}


shinyApp(ui = ui, server = server)