library(shiny)
library(tidyverse)
library(geojsonio)
library(leaflet)
library(RColorBrewer)
library(psych)
library(htmltools)

rus <- geojson_read("geojson_rus_fin2.geojson", what = "sp")

ui <- fluidPage(
  leafletOutput("map"),
  selectInput("value", "Показатель", choices = colnames(rus@data[6:8])),
  selectInput("color", "Схема", choices = rownames(subset(brewer.pal.info, category == "seq")))
)

server <- function(input, output, session) {
  
  actualvalue <- reactive({
    input$value
  })
  
  colorpal <-  reactive({
    colorBin(input$color,  rus@data[[input$value]], bins = 4, pretty = TRUE, na.color = "wheat")
    })

  labels <- reactive({paste("</strong>", rus@data$name, "</strong><br>", input$value, ":", round(rus@data[[input$value]], 2)) %>% 
      lapply(htmltools::HTML)
  })
  
  output$map <- renderLeaflet({
    leaflet(rus) %>% 
      addTiles()
    })
  
  observe({
    pal <- colorpal()
    leafletProxy("map", data = rus) %>% 
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(fillColor = ~pal(rus[[actualvalue()]]), 
                  weight = 0.8, 
                  opacity = 1, 
                  color = "steelblue", 
                  dashArray = "3", 
                  fillOpacity = 0.7,  
                  highlightOptions = highlightOptions(weight = 1, color = "coral", fillOpacity = 0.85, dashArray = "", bringToFront = TRUE),  
                  label = ~labels()) %>% 
      addLegend(pal = pal, values = rus@data[[input$value]], opacity = 0.7, title = NULL, position = "bottomright")
  })
  
}


shinyApp(ui, server)

?addPolygons

rus@data$salary
rus@data["salary"]


?addPolygons()
