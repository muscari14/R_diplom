library(tidyverse)
library(geojsonio)
library(leaflet)
library(RColorBrewer)
library(psych)
library(htmltools)

#Препроцессинг данных:

dat <- read.csv("Dat.csv", check.names = FALSE)

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

geojson_write(rus_corrected, file = "russia_modified.geojson")

#Приводим названия регионов к единому виду:

rus <- geojson_read("russia_modified.geojson", what = "sp")

rus@data$name <- str_replace_all(rus@data$name, c("Кабардино-Балкарская республика" = "Кабардино-Балкарская Республика",
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

rus@data$name[rus@data$name == "Алтай"] <- "Республика Алтай"

#Добавляем социоэкономические показатели:
dat_small <- dat %>% 
  select(Регион, `Индекс потребительской активности`, `Доля безналичных платежей`, `Среднемесячная заработная плата`)

dat_small <- dat_small %>% 
  group_by(Регион) %>% 
  reframe(`Индекс потребительской активности` = mean(`Индекс потребительской активности`), 
          `Доля безналичных платежей` = mean(`Доля безналичных платежей`), 
          `Среднемесячная заработная плата` = mean(`Среднемесячная заработная плата`))

rus@data <- left_join(rus@data, dat_small, join_by(name == `Регион`))

#Создаем палитру:
pal <- colorBin("YlOrRd", domain = rus@data$`Среднемесячная заработная плата`, bins = 4, pretty = TRUE)

#Создаем подписи:
labels <- paste(
  "<strong>", rus@data$name, 
  "</strong><br>Среднемесячная зарплата:", round(rus@data$`Среднемесячная заработная плата`, 2)) %>% 
  lapply(htmltools::HTML)

map <- leaflet(rus) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~pal(`Среднемесячная заработная плата`), 
              weight = 0.8, 
              opacity = 1, 
              color = "steelblue", 
              dashArray = "3", 
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(weight = 1, color = "coral", fillOpacity = 0.8, dashArray = "", bringToFront = TRUE),
              label = labels) %>% 
  addLegend(pal = pal, values = ~`Среднемесячная заработная плата`, opacity = 0.7, title = NULL, position = "bottomright")


map
