#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(geojsonio)
library(leaflet)
library(RColorBrewer)
library(psych)
library(htmltools)

#Палитра
seqpals <- RColorBrewer::brewer.pal.info %>% 
  filter(category == "seq") %>% 
  rownames()


ui <- fluidPage(
  leafletOutput("rus"),
  p(),
  selectInput("pal", "Выберите палитру:", choices = pals),
  selectInput("value", "Выберите показатель:", choices = c("Индекс потребительской активности", 
                                                           "Доля безналичных платежей", 
                                                           "Среднемесячная заработная плата"))
  
)

server <- function(input, output, session) {
  output$rus <- renderLeaflet({
    leaflet() %>% 
      addTiles()
  })
  
}

shinyApp(ui, server)



