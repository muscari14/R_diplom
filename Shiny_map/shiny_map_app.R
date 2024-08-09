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

##Препроцессинг данных:

#Загружаем социоэкономические данные по регионам:
dat <- read.csv("Dat.csv", check.names = FALSE)

#Читаем geojson-файл с координатами регионов России:
rus <- geojson_read("russia.geojson", what = "sp")

#Модифицируем geojson-файл, чтобы корректно отображалась Чукотка:
chukotka <- rus@polygons[[18]]
for (i in 1:length(chukotka@Polygons)) {
  polygon_long <- chukotka@Polygons[[i]]@coords[, 1]
  if (mean(polygon_long) < 0) {
    polygon_long <- 360 + polygon_long
  }
  chukotka@Polygons[[i]]@coords[, 1] <- polygon_long
}

rus_corrected <- rus
rus_corrected@polygons[[18]] <- chukotka

#Приводим названия регионов к единому виду:

rus_corrected@data$name <- str_replace_all(rus_corrected@data$name, c("Кабардино-Балкарская республика" = "Кабардино-Балкарская Республика",
                                                                      "Бурятия" = "Республика Бурятия",
                                                                      "Марий Эл" = "Республика Марий Эл",
                                                                      "Тыва" = "Республика Тыва",
                                                                      "Чувашия" = "Чувашская Республика",
                                                                      "Дагестан" = "Республика Дагестан",
                                                                      "Северная Осетия - Алания" = "Республика Северная Осетия-Алания",
                                                                      "Удмуртская республика" = "Удмуртская Республика",
                                                                      "Ямало-Ненецкий автономный округ" = "Ямало-Ненецкий АО",
                                                                      "Башкортостан" = "Республика Башкортостан",
                                                                      "Карачаево-Черкесская республика" = "Республика Карачаево-Черкессия",
                                                                      "Татарстан" = "Республика Татарстан",
                                                                      "Ханты-Мансийский автономный округ - Югра" = "Ханты-Мансийский АО - Югра",
                                                                      "Республика Мордовия" = "Мордовия",
                                                                      "Ненецкий автономный округ" = "Ямало-Ненецкий АО",
                                                                      "Чукотский автономный округ" = "Чукотский АО",
                                                                      "Чеченская республика" = "Чеченская Республика",
                                                                      "Еврейская автономная область" = "Еврейская АО",
                                                                      "Ингушетия" = "Республика Ингушетия"))

rus_corrected@data$name[rus_corrected@data$name == "Алтай"] <- "Республика Алтай"

#Добавляем социоэкономические показатели:
dat_small <- dat %>% 
  select(Регион, `Индекс потребительской активности`, `Доля безналичных платежей`, `Среднемесячная заработная плата`)

dat_small <- dat_small %>% 
  group_by(Регион) %>% 
  reframe(`Индекс потребительской активности` = mean(`Индекс потребительской активности`), 
          `Доля безналичных платежей` = mean(`Доля безналичных платежей`), 
          `Среднемесячная заработная плата` = mean(`Среднемесячная заработная плата`))

rus_corrected@data <- left_join(rus_corrected@data, dat_small, join_by(name == `Регион`))

#С этим файлом далее будем работать:
geojson_write(rus_corrected, file = "geojson_rus_fin.geojson")

rus <- rus_corrected

#Пользовательский интерфейс:
ui <- fluidPage(
  leafletOutput("rus"),
  textOutput("txtt"),
  p(),
  selectInput("pal", "Выберите палитру:", choices = rownames(subset(brewer.pal.info, category == "seq"))),
  selectInput("value", "Выберите показатель:", choices = c("Индекс потребительской активности", 
                                                           "Доля безналичных платежей", 
                                                           "Среднемесячная заработная плата"))
)

server <- function(input, output, session) {
  output$rus <- renderLeaflet(
    leaflet(rus) %>% 
      addTiles() %>% 
      addPolygons(fillColor = "tomato", 
                  weight = 0.8, 
                  opacity = 1, 
                  color = "steelblue", 
                  dashArray = "3", 
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(weight = 1, color = "coral", fillOpacity = 0.8, dashArray = "", bringToFront = TRUE))
  )
}

shinyApp(ui, server)



