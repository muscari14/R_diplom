library(shiny)
library(RColorBrewer)

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
    colorBin(input$color,  rus@data[[input$value]], bins = 4, pretty = TRUE)
    })

  
  output$map <- renderLeaflet({
    leaflet(rus) %>% 
      addTiles()
    })
  
  observe({
    pal <- colorpal()
    leafletProxy("map", data = rus) %>% 
      clearShapes() %>% 
      addPolygons(fillColor = ~pal(rus[[actualvalue()]]))
  })
  
}


shinyApp(ui, server)

?addPolygons

rus@data$salary
rus@data["salary"]



