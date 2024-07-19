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
library(scales)

dat <- drop_na(read.csv("forshiny.csv", check.names = FALSE))
dat <- dat[-1]

#Препроцессинг:
# dat$`Число активных абонентов беспроводного доступа к сети` <- as.numeric(dat$`Число активных абонентов беспроводного доступа к сети`)
dat$Месяц <- factor(dat$Месяц, ordered = TRUE,
                    levels = c("Январь", "Февраль", "Март", 
                               "Апрель", "Май", "Июнь", "Июль",
                               "Август", "Сентябрь", "Октябрь",
                               "Ноябрь", "Декабрь"))
dat$Квартал <- factor(dat$Квартал,
                      levels = c("Квартал 1", "Квартал 2", "Квартал 3", "Квартал 4"))

dat$`Всего одобренных заявок` <- factor(dat$`Всего одобренных заявок`,
                                      levels = c("100 - 500",
                                                 "500 - 1 000",
                                                 "1 000 - 5 000",
                                                 "5 000 - 10 000",
                                                 "> 10 000"), ordered = TRUE)
                                        


dat$`Всего ипотечных сделок` <- factor(dat$`Всего ипотечных сделок`,
                                     levels = c("10 - 50",
                                                "50 - 100",
                                                "100 - 500",
                                                "500 - 1 000",
                                                "> 1 000"), ordered = TRUE)

#Пользовательский интерфейс:
ui <- fluidPage(
  titlePanel("Социоэкономические показатели регионов России"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region",
                  "Регион:",
                  choices = c("Россия", as.character(sort(unique(dat$Регион))))),
      selectInput("value",
                  "Показатель:",
                  choices = sort(colnames(dat)[5 : length(colnames(dat))])),
    radioButtons("mode",
                 "Режим:",
                 choices = c("Распределение" = "distribution",
                             "Динамика" = "dynamic")),
    textInput("plot_col",
              "Цвет:",
              "#808080"),
    actionLink("panthone", "PANTONE 13-1023 Peach Fuzz")),
    mainPanel(plotOutput("Plot"))
  )
)


#Обработка и вывод:
#В датафрейме имеется два факторных показателя: `Всего одобренных заявок` и `Всего ипотечных сделок`.
#Для них построим отдельные графики: барплот для режима `Распределение` и барплот с разбивкой по месяцам
#для режима `Динамика`
server <- function(input, output){
  output$Plot <- renderPlot({
    if(input$mode == "distribution"){
      if(input$value == "Всего одобренных заявок" | input$value == "Всего ипотечных сделок"){
        if(input$region != "Россия"){
          dat_f <- dat %>% 
            filter(Регион == input$region)
          dat_f %>% 
            count(!!sym(input$value)) %>% 
            mutate(prop = n/sum(n)) %>% 
            ggplot(aes(x = !!sym(input$value), y = prop, fill = !!sym(input$value))) +
            geom_col(color = "grey20") +
            geom_text(aes(label = percent(prop), vjust = -1)) +
            scale_y_continuous(labels = percent_format()) +
            theme_minimal() +
            theme(axis.title = element_blank()) +
            scale_fill_manual(values = c("lightpink", "steelblue", "palegreen3", "coral", "wheat"))
        }
        else{
          dat %>% 
            count(!!sym(input$value)) %>% 
            mutate(prop = n/sum(n)) %>% 
            ggplot(aes(x = !!sym(input$value), y = prop, fill = !!sym(input$value))) +
            geom_col(color = "grey20") +
            geom_text(aes(label = percent(prop), vjust = -1)) +
            scale_y_continuous(labels = percent_format()) +
            theme_minimal() +
            theme(axis.title = element_blank()) +
            scale_fill_manual(values = c("lightpink", "steelblue", "palegreen3", "coral", "wheat")) 
        }
      }
      else{
        if(input$region != "Россия"){
          dat_f <- dat %>% 
            filter(Регион == input$region)
          dat_f %>% 
            ggplot(aes(y = !!sym(input$value))) +
            geom_boxplot(fill = input$plot_col) +
            labs(x = input$region) +
            scale_x_continuous(breaks = NULL) +
            theme_minimal()
        }
        else{
          dat %>% 
            ggplot(aes(y = !!sym(input$value))) +
            geom_boxplot(fill = input$plot_col) +
            labs(x = input$region) +
            scale_x_continuous(breaks = NULL) +
            theme_minimal()
        }
      }
    }
    else{
      if(input$value == "Всего одобренных заявок" | input$value == "Всего ипотечных сделок"){
        if(input$region != "Россия"){
      dat_f <- dat %>% 
        filter(Регион == input$region)
      dat_f %>% 
        ggplot(aes(x = `Месяц`, y = !!sym(input$value), group = 1)) +
        geom_line(color = input$plot_col) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45))
        }
        else{
          dat %>% 
            count(!!sym(input$value), Месяц) %>% 
            ggplot(aes(x = Месяц, y = !!sym(input$value), size = n, color = !!sym(input$value))) +
            geom_point() +
            scale_color_manual(values = c("lightpink", "tomato", "steelblue", "palegreen3", "coral")) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45))
        }
      }
      else{
        if(input$region != "Россия"){
          dat_f <- dat %>% 
            filter(Регион == input$region)
          dat_f %>% 
            ggplot(aes(x = `Месяц`, y = !!sym(input$value), group = 1)) +
            geom_line() +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, size = rel(1.5)))
        }
        else{
          dat %>% 
            group_by(Месяц) %>% 
            summarise(value = mean(!!sym(input$value))) %>% 
            ggplot(aes(x = `Месяц`, y = value, group = 1)) +
            geom_line() +
            labs(y = input$value) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, size = rel(1.5)))
        }
      }
    }
  }
  )
}


 

    



shinyApp(ui = ui, server = server)

 
