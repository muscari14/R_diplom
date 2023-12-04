# Препроцессинг данных

# Загружаем библиотеки:
library(tidyverse)
library(readxl)

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
ls = list.files("C:/R/Skillbox/R_diplom/Рейтинг регионов по количеству заявок на кредит/")
for(i in seq(1, 12)){
  dat <- read_xlsx(paste0("C:/R/Skillbox/R_diplom/Рейтинг регионов по количеству заявок на кредит/", ls[i]))
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
ls2 = list.files("C:/R/Skillbox/R_diplom/Рейтинг регионов по количеству ипотечных сделок/")
for(i in seq(1, 12)){
  dat <- read_xlsx(paste0("C:/R/Skillbox/R_diplom/Рейтинг регионов по количеству ипотечных сделок/", ls2[i]))
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

rosstat_zp <- read_xlsx("Rosstat_Заработная плата.xlsx", sheet = "с 2019", skip = 2)
rosstat_zp <-  rosstat_zp %>% 
  select(...1, январь...14 : декабрь...25)
colnames(rosstat_zp) <- c("Регион", month)
rosstat_zp <- rosstat_zp[-c(24, 69), ]
rosstat_zp <- pivot_longer(rosstat_zp, cols = "Январь" : "Декабрь", names_to = "Месяц", values_to = "Среднемесячная заработная плата")
rosstat_zp <- rosstat_zp %>% 
  filter(Регион != "Российская Федерация")
rosstat_zp <- rosstat_zp %>% 
  mutate(`Квартал` = case_when(`Месяц` %in% month[1:3] ~ "Квартал 1",
                               `Месяц` %in% month[4:6] ~ "Квартал 2",
                               `Месяц` %in% month[7:9] ~ "Квартал 3",
                               `Месяц` %in% month[10:12] ~ "Квартал 4"), .after = `Месяц`)
