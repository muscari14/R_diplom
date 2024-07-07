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

dat <- drop_na(read.csv("forshiny.csv"))


# dat$`Число активных абонентов беспроводного доступа к сети` <- as.numeric(dat$`Число активных абонентов беспроводного доступа к сети`)
dat$Месяц <- factor(dat$Месяц, ordered = TRUE,
                    levels = c("Январь", "Февраль", "Март", 
                               "Апрель", "Май", "Июнь", "Июль",
                               "Август", "Сентябрь", "Октябрь",
                               "Ноябрь", "Декабрь"))
dat$Квартал <- factor(dat$Квартал,
                      levels = c("Квартал 1", "Квартал 2", "Квартал 3", "Квартал 4"))

dat$Всего.одобренных.заявок <- factor(dat$Всего.одобренных.заявок,
                                      levels = c("100 - 500",
                                                 "500 - 1 000",
                                                 "1 000 - 5 000",
                                                 "5 000 - 10 000",
                                                 "> 10 000"), ordered = TRUE,
                                      labels = paste("111", levels(dat$Всего.одобренных.заявок)))


dat$Всего.ипотечных.сделок <- factor(dat$Всего.ипотечных.сделок,
                                     levels = c("10 - 50",
                                                "50 - 100",
                                                "100 - 500",
                                                "500 - 1 000",
                                                "> 1 000"), ordered = TRUE)


ui <- fluidPage(
  titlePanel("Социоэкономические показатели регионов России"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region",
                  "Регион:",
                  choices = c("Россия", as.character(sort(unique(dat$Регион))))),
      selectInput("value",
                  "Показатель:",
                  choices = sort(colnames(dat)[5:length(colnames(dat))])),
    radioButtons("mode",
                 "Режим:",
                 choices = c("Распределение" = "distribution",
                             "Динамика" = "dynamic")),
    textInput("plot_col",
              "Цвет:",
              "#808080")),
    mainPanel(plotOutput("Plot"))
  )
)


server <- function(input, output){
  output$Plot <- renderPlot({
    if(input$mode == "distribution"){
      if(input$region != "Россия"){
        dat_f <- dat %>% 
          filter(Регион == input$region)
        dat_f %>% 
        ggplot(aes(y = !!sym(input$value))) +
        geom_boxplot() +
        scale_x_continuous(breaks = NULL)+
        theme_minimal()
}}})}



shinyApp(ui = ui, server = server)

