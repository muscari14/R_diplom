#Необходимые библиотеки:
library(shiny)
library(tidyverse)
library(geojsonio)
library(leaflet)
library(RColorBrewer)
library(htmltools)

#Чтение данных:
rus <- geojson_read("rus_fin.geojson", what = "sp")
gsub("\\.", " ", colnames(rus@data[6:8]))
colnames(rus@data)[6:8] <- gsub("\\.", " ", colnames(rus@data)[6:8])

#Пользовательский интерфейс
ui <- fluidPage(
  leafletOutput("map"),
  selectInput("color", "Выберите палитру:", choices = rownames(subset(brewer.pal.info, category == "seq"))),
  selectInput("value", "Выберите показатель:", choices = colnames(rus@data)[6:8])
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
      addTiles() %>% 
      setView(lng = 100, lat = 60, zoom = 2)
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
                  fillOpacity = 0.65,  
                  highlightOptions = highlightOptions(weight = 1, color = "coral", fillOpacity = 0.85, dashArray = "", bringToFront = TRUE),  
                  label = ~labels()) %>% 
      addLegend(pal = pal, values = rus@data[[input$value]], opacity = 0.7, position = "bottomright", title = input$value)
  })
}

shinyApp(ui, server)

