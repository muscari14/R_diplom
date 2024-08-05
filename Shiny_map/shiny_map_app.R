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

#Загружаем данные:
rus <- geojson_read("rus_fin.geojson", what = "sp")

#Создаем сиписок последовательных палитр из библиотеки RColorBrewer:
seqpals <- RColorBrewer::brewer.pal.info

seqpals <- seqpals %>% 
  filter(category == "seq") %>% 
  row.names()

#Пользовательский интерфейс:
ui <- fluidPage(
    titlePanel("Социоэкономические показатели регионов России"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("mmap"),
           selectInput("pallette",
                       "Выберите палитру:", 
                       choices = seqpals),
           selectInput("value",
                       "Выберите показатель:",
                       choices = c("Индекс потребительской активности", "Доля безналичных платежей", "Среднемесячная заработная плата")))))   
 


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mmap <- renderLeaflet({
      leaflet(rus) %>% 
        addTiles()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
