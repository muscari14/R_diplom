# Препроцессинг данных

# Загружаем библиотеки:
library(tidyverse)
library(readxl)
library(writexl)

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
      
# DomClick ----------------------------------------------------------------
### Domclick_credit:

# Создаем функцию для чтения и обработки данных:
domclck_prep <- function(dat){
  dat$Месяц <- "X"
  dat$Год <- 2020
  dat <- dat %>% 
    select(`Регион`, `Год`, `Месяц`, `Всего одобренных заявок` : `Доля заявок в офисе банка`)
  dat <- dat %>% 
    mutate(`Доля онлайн-заявок` = as.numeric(str_replace(`Доля онлайн-заявок`, "%", "")) / 100)
  dat <- dat %>% 
    mutate(`Доля заявок в офисе банка` = as.numeric(str_replace(`Доля заявок в офисе банка`, "%", "")) / 100)
  dat$`Всего одобренных заявок` <- factor(dat$`Всего одобренных заявок`,
                                               ordered = TRUE,
                                               levels = c("100 - 500",
                                                          "500 - 1 000",
                                                          "1 000 - 5 000",
                                                          "5 000 - 10 000",
                                                          "> 10 000"))
  dat <- drop_na(dat)
  return(dat)
}

# Создаем вектор с именами:
names_ls = c()
for(i in seq(1:12)){
  x <- paste("dmclck_cred", i, sep = "_")
  names_ls[i] <- x
}

# Создаем вектор с месяцами:
month <- as.character(sber_un$Месяц[1:12])

# Читаем, обрабатываем и записываем файлы:
ls = list.files("Рейтинг регионов по количеству заявок на кредит/")
for(i in seq(1, 12)){
  dat <- read_xlsx(paste0("Рейтинг регионов по количеству заявок на кредит/", ls[i]))
  dat <- domclck_prep(dat)
  dat$Месяц <- month[i]
  assign(paste("domclck", i, sep = "_"), dat)
}

domclck_un <- rbind(domclck_1, 
                   domclck_2, 
                   domclck_3,
                   domclck_4,
                   domclck_5,
                   domclck_6,
                   domclck_7,
                   domclck_8,
                   domclck_9,
                   domclck_10,
                   domclck_11,
                   domclck_12)

domclck_un <- domclck_un %>% 
  arrange(Регион)

### Domclick_hypothec:

# Создаем функцию для чтения и обработки данных:
domclck_hyp_prep <- function(dat){
  dat$Месяц <- "X"
  dat$Год <- 2020
  dat <- dat %>% 
    select(`Регион`, `Год`, `Месяц`, `Всего ипотечных сделок` : `Доля сделок, вторичка`)
  dat <- dat %>% 
    mutate(`Доля сделок, первичка` = as.numeric(str_replace(`Доля сделок, первичка`, "%", "")) / 100)
  dat <- dat %>% 
    mutate(`Доля сделок, вторичка` = as.numeric(str_replace(`Доля сделок, вторичка`, "%", "")) / 100)
  dat$`Всего ипотечных сделок` <- factor(dat$`Всего ипотечных сделок`,
                                          ordered = TRUE,
                                          levels = c("10 - 50",
                                                     "50 - 100",
                                                     "100 - 500",
                                                     "500 - 1 000",
                                                     "> 1 000"))
  dat <- drop_na(dat)
  return(dat)
}

# Создаем вектор с именами:
names2_ls = c()
for(i in seq(1:12)){
  x <- paste("dmclck_hyp", i, sep = "_")
  names2_ls[i] <- x
}

# Создаем вектор с месяцами:
month <- as.character(sber_un$Месяц[1:12])

# Читаем, обрабатываем и записываем файлы:
ls2 = list.files("Рейтинг регионов по количеству ипотечных сделок/")
for(i in seq(1, 12)){
  dat <- read_xlsx(paste0("Рейтинг регионов по количеству ипотечных сделок/", ls2[i]))
  dat <- domclck_hyp_prep(dat)
  dat$Месяц <- month[i]
  assign(paste("domclck_hyp", i, sep = "_"), dat)
}

domclck_hyp_un <- rbind(domclck_hyp_1, 
                    domclck_hyp_2, 
                    domclck_hyp_3,
                    domclck_hyp_4,
                    domclck_hyp_5,
                    domclck_hyp_6,
                    domclck_hyp_7,
                    domclck_hyp_8,
                    domclck_hyp_9,
                    domclck_hyp_10,
                    domclck_hyp_11,
                    domclck_hyp_12)

domclck_hyp_un <- domclck_hyp_un %>% 
  arrange(Регион)

domclck_both <- left_join(domclck_un, domclck_hyp_un)


# Rosstat -----------------------------------------------------------------
### Росстат - зарплата:
rosstat_zp <- read_xlsx("Rosstat_Заработная плата.xlsx", sheet = "с 2019", skip = 2)
rosstat_zp <-  rosstat_zp %>% 
  select(...1, январь...14 : декабрь...25)
colnames(rosstat_zp) <- c("Регион", month)
rosstat_zp <- rosstat_zp[-c(24, 69), ]
rosstat_zp <- pivot_longer(rosstat_zp, cols = "Январь" : "Декабрь", names_to = "Месяц", values_to = "Среднемесячная заработная плата")
rosstat_zp <- rosstat_zp %>% 
  filter(Регион != "Российская Федерация")
rosstat_zp <- rosstat_zp %>% filter(rosstat_zp$Регион != c("Архангельская область",
                                                                          "Тюменская область"))
rosstat_zp <- rosstat_zp %>% 
  mutate(`Квартал` = case_when(`Месяц` %in% month[1:3] ~ "Квартал 1",
                               `Месяц` %in% month[4:6] ~ "Квартал 2",
                               `Месяц` %in% month[7:9] ~ "Квартал 3",
                               `Месяц` %in% month[10:12] ~ "Квартал 4"), .after = `Месяц`)
### Росстат - интернет:
rosstat_net <- read_xlsx("Rosstat_Интернет.xlsx", sheet = "Беспроводная наземная", 
                         skip = 2)

# В этой таблице нет данных за 2020 г., поэтому выберу данные за 2019 г.
rosstat_net <- rosstat_net[-c(1, 2), c(1, 25:28)]
colnames(rosstat_net) <- c("Регион", "Квартал 1", "Квартал 2", "Квартал 3",
                           "Квартал 4")
rosstat_net <- pivot_longer(rosstat_net, cols = "Квартал 1" : "Квартал 4", names_to = "Число активных абонентов беспроводного доступа к сети")
colnames(rosstat_net) <- c("Регион", "Квартал", "Число активных абонентов беспроводного доступа к сети")
rosstat_net <- rosstat_net %>% 
  filter(`Регион` != "Тюменская область")

### Росстат - безработица:
rosstat_chomage <- read_xls("Rosstat_Уровень безработицы населения.xls",
                            sheet = 2,
                            skip = 3)
rosstat_chomage <- rosstat_chomage %>%
  select(...1, `январь  - март 
2020`, `апрель  - июнь
 2020`, `июль  - сентябрь
 2020`, `октябрь - декабрь
 2020`)
colnames(rosstat_chomage) <- c("Регион", "Квартал 1", "Квартал 2", "Квартал 3",
                               "Квартал 4")
rosstat_chomage <- pivot_longer(rosstat_chomage, 
                                cols = "Квартал 1" : "Квартал 4", names_to = "Уровень безработицы населения")
rosstat_chomage <- rosstat_chomage[rosstat_chomage$Регион != "Российская Федерация", ]
colnames(rosstat_chomage) <- c("Регион", "Квартал", "Уровень безработицы населения")
rosstat_chomage <- rosstat_chomage %>% filter(rosstat_chomage$Регион != c("Тюменская область"))
rosstat_chomage <- rosstat_chomage %>% 
  filter(Регион != "Архангельская область")
                                            
### Приводим названия регионов к общему виду,
# за стандарт возьмем названия регионов по данным Сбербанка:

# Сбер и ДомкКлик:
setdiff(sber_un$Регион, domclck_both$Регион)
setdiff(domclck_both$Регион, sber_un$Регион)

setdiff(sber_un$Регион, rosstat_zp$Регион)
setdiff(rosstat_zp$Регион, sber_un$Регион)

setdiff(domclck_both$Регион, rosstat_zp$Регион)
setdiff(rosstat_zp$Регион, domclck_both$Регион)

setdiff(rosstat_net$Регион, rosstat_zp$Регион)
setdiff(rosstat_zp$Регион, rosstat_net$Регион)

domclck_both$Регион <- str_replace_all(domclck_both$Регион,
                                       c("Область" = "область",
                                         "Край" = "край",
                                         "Карачаево-Черкесская Республика" = "Республика Карачаево-Черкессия",
                                         "Республика Адыгея" = "Адыгея",
                                         "Республика Мордовия" = "Мордовия",
                                         "Республика Северная Осетия - Алания" = "Республика Северная Осетия-Алания",
                                         "Ханты-Мансийский Автономный округ - Югра" = "Ханты-Мансийский АО - Югра" ,
                                         "Ямало-Ненецкий Автономный округ" = "Ямало-Ненецкий АО",
                                         "Еврейская Автономная область" = "Еврейская АО"))

## Росстат:
# rosstat_zp:

setdiff(x = domclck_both$Регион, y = rosstat_zp$Регион)
setdiff(x = rosstat_zp$Регион, y = domclck_both$Регион)

rosstat_zp$Регион <- str_replace_all(rosstat_zp$Регион, pattern = c("г.Москва" = "Москва",
                                                                    "Ханты-Мансийский  авт. округ - Югра" = "Ханты-Мансийский АО - Югра",
                                                                    "Архангельская область без авт. округа." = "Архангельская область",
                                                                    "Тюменская область без авт. округов" = "Тюменская область",
                                                                    "в том числе Ненецкий авт.округ" = "Ненецкий АО",
                                                                    "авт. округ" = "АО",
                                                                    "авт.округ" = "АО",
                                                                    "г.Санкт-Петербург" = "Санкт-Петербург",
                                                                    "федеральный округ" = "ФО",
                                                                    "Республика Адыгея" = "Адыгея",
                                                                    "Карачаево-Черкесская Республика" = "Республика Карачаево-Черкессия",
                                                                    "Республика Северная  Осетия - Алания" = "Республика Северная Осетия-Алания",
                                                                    "Республика Мордовия" = "Мордовия",
                                                                    "Ямало-Ненецкий авт. округ" = "Ямало-Ненецкий АО",
                                                                    "Еврейская авт.область" = "Еврейская АО",
                                                                    "г. Севастополь" = "Севастополь",
                                                                    "г.Севастополь" = "Севастополь"))
rosstat_zp <- rosstat_zp %>% 
  filter(Регион != "в том числе:")

# rosstat_net:

setdiff(x = domclck_both$Регион, y = rosstat_net$Регион)
setdiff(x = rosstat_net$Регион, y = domclck_both$Регион)

setdiff(x = rosstat_zp$Регион, y = rosstat_net$Регион)
setdiff(x = rosstat_net$Регион, y = rosstat_zp$Регион)




rosstat_net$Регион <- str_replace_all(rosstat_net$Регион, pattern = c("Центральный                федеральный округ" = "Центральный ФО",
                                                                      "г. Москва" = "Москва",
                                                                      "Северо-Западный                    федеральный округ" = "Северо-Западный ФО",
                                                                      "г. Санкт-Петербург" = "Санкт-Петербург",
                                                                      "Южный                              федеральный округ" = "Южный ФО",
                                                                      "Республика Адыгея" = "Адыгея",
                                                                      "г. Севастополь" = "Севастополь",
                                                                      "Северо-Кавказский                                                                               федеральный округ" = "Северо-Кавказский ФО",
                                                                      "Карачаево-Черкесская Республика" = "Республика Карачаево-Черкессия",
                                                                      "Республика Северная Осетия - Алания" = "Республика Северная Осетия-Алания",
                                                                      "Приволжский                     федеральный округ" = "Приволжский ФО",
                                                                      "Республика Мордовия" = "Мордовия",
                                                                      "Удмуртская республика" = "Удмуртская Республика",
                                                                      "Чувашская республика" = "Чувашская Республика",
                                                                      "Уральский                           федеральный округ" = "Уральский ФО",
                                                                      "в том числе Ханты-Мансийский автономный АО - Югра" = "Ханты-Мансийский АО - Югра",
                                                                      "Ямало-Ненецкий автономный АО" = "Ямало-Ненецкий АО",
                                                                      "Тюменская область без АО" = "Тюменская область",
                                                                      "Сибирский                         федеральный округ" = "Сибирский ФО",                                                            
                                                                      "Дальневосточный                   федеральный округ" = "Дальневосточный ФО",                                                              
                                                                      "Еврейская автономная область" = "Еврейская АО",                                                                                     
                                                                      "Чукотский автономный округ" = "Чукотский АО")) 

# rosstat chomage:

setdiff(x = rosstat_zp$Регион, y = rosstat_chomage$Регион)
setdiff(x = rosstat_chomage$Регион, rosstat_zp$Регион)

setdiff(x = rosstat_net$Регион, y = rosstat_chomage$Регион)
setdiff(x = rosstat_chomage$Регион, y = rosstat_net$Регион)

rosstat_chomage$Регион <- str_replace_all(string = rosstat_chomage$Регион, 
                                          pattern = c("федеральный округ" = "ФО",
                                                      "в том числе: \nНенецкий автономный округ" = "Ненецкий АО",
                                                      "Архангельская область без авт. округа" = "Архангельская область",
                                                      "г. Москва" = "Москва",
                                                      "г. Санкт-Петербург" = "Санкт-Петербург",
                                                      "г.Санкт-Петербург" = "Санкт-Петербург",
                                                      "Республика Адыгея" = "Адыгея",
                                                      "г. Севастополь" = "Севастополь",
                                                      "Карачаево-Черкесская Республика" = "Республика Карачаево-Черкессия",
                                                      "Республика Северная Осетия - Алания" = "Республика Северная Осетия-Алания",
                                                      "Республика Мордовия" = "Мордовия",
                                                      "в том числе: \nХанты-Мансийский автономный округ - Югра" = "Ханты-Мансийский АО - Югра",
                                                      "Ямало-Ненецкий автономный округ" = "Ямало-Ненецкий АО",
                                                      "Тюменская область без авт. округов" = "Тюменская область",
                                                      "Еврейская автономная область" = "Еврейская АО",
                                                      "Чукотский автономный округ" = "Чукотский АО"))
                                                      
                                                      
rosstat_un <- full_join(x = rosstat_zp, y = rosstat_net,
                         by = c("Регион", "Квартал"))

rosstat_un <- full_join(x = rosstat_un, y = rosstat_chomage,
                        by = c("Регион", "Квартал"))

# В датасете про беспроводной интернет отсутствуют данные по Ненецкому округу,
# к сожалению, придется исключить его из итогового датафрейма,
# чтобы избежать пропущенных значений

rosstat_un <- rosstat_un %>% 
  filter(Регион != "Ненецкий АО")

anyNA(rosstat_un)

## Объединение в итоговый датафрейм

dat <- full_join(x = sber_un, y = domclck_both)
dat <- full_join(x = dat, y = rosstat_un)

# Дополнительные преобразования:

dat <- dat %>% 
  select(!Год)

dat$Квартал <- as.factor(dat$Квартал)

dat <- dat %>% relocate(Квартал, .after = Месяц)

write_xlsx(dat, "Итоговая таблица.xlsx")                                                                  
                                                                    




