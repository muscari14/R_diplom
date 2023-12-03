# Препроцессинг данных

# Загружаем библиотеки:
library(tidyverse)


# Sber --------------------------------------------------------------------

# Читаем исходные файлы:
sber1 <- read.csv("Sber_index1.csv")
sber2 <- read.csv2("Sber_index2.csv") 
sber3 <- read.csv2("Sber_index3.csv")

# Так как данные СберИндекса имеют одинаковую структуру, мы можем обработать их 
# единообразно, написав функцию:
sber_prep <- function(dat){
  dat_clean <- dat %>% 
    separate(`Дата`, into = c("Год", "Месяц", "День"), sep = "-") %>% 
    filter(`Год` == 2020, `Регион` != "Россия")
  dat_clean$Значение <- as.numeric(dat_clean$Значение)
  dat_clean$Год <-  as.integer(dat_clean$Год)
  dat_clean$Месяц <- factor(dat_clean$Месяц, 
                        labels = c("Январь", "Февраль", "Март", 
                                                "Апрель", "Май", "Июнь", "Июль",
                                                "Август", "Сентябрь", "Октябрь",
                                                "Ноябрь", "Декабрь"), 
                        ordered = TRUE)
  dat_clean <- dat_clean %>% 
    summarise(.by = c("Регион", "Год", "Месяц"),indx = median(Значение))
  dat_clean <- dat_clean %>% 
    arrange(`Регион`)
  return(dat_clean)}

sber1_res <- sber_prep(sber1)      
sber2_res <- sber_prep(sber2)  
sber3_res <- sber_prep(sber3)

# Объединяем полученные датафреймы:
colnames(sber1_res)[4] <- "Индекс потребительской активности"
colnames(sber2_res)[4] <- "Доля безналичных платежей"
colnames(sber3_res)[4] <- "Количество внутренних туристов"

sber_un <- left_join(sber1_res, sber2_res)
sber_un <- left_join(sber_un, sber3_res)

# Заполняем пропущенные значения:
sber_un <- sber_un %>% 
  fill(`Доля безналичных платежей`, .direction = "down")
anyNA(sber_un)
      
