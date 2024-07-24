library(scales)
library(tidyverse)

dat %>% 
  count(`Всего одобренных заявок`) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = `Всего одобренных заявок`, y = prop, fill = `Всего одобренных заявок`)) +
  geom_col(color = "grey20") +
  geom_text(aes(label = percent(prop), vjust = -1)) +http://127.0.0.1:22779/graphics/plot_zoom_png?width=1856&height=861
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  scale_fill_manual(values = c("lightpink", "steelblue", "palegreen3", "coral", "wheat"))

dat %>% 
  count(Месяц, `Всего одобренных заявок`) %>% 
  group_by(Месяц) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(x = `Всего одобренных заявок`, y = prop, fill = `Всего одобренных заявок`)) +
  geom_col() +
  facet_wrap(~ `Месяц`)

dat %>%
  filter(Регион == "Саратовская область") %>% 
  ggplot(aes(x= `Месяц`, y = `Всего одобренных заявок`, group = 1)) +
  geom_line()


dat %>% 
  filter(Регион == "Москва") %>% 
  ggplot(aes(x = Месяц, y = `Всего одобренных заявок`, group = 1)) +
  geom_line(color = "orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
      
dat_f <- dat %>% 
  count(`Всего одобренных заявок`, Месяц)

dat_f %>% 
  ggplot(aes(x = Месяц, y = `Всего одобренных заявок`, size = n, color = `Всего одобренных заявок`)) +
  geom_point() +
  scale_color_manual(values = c("lightpink", "tomato", "steelblue", "palegreen3", "coral")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

dat %>%
  group_by(Месяц) %>% 
  summarise(mean = mean(`Среднемесячная заработная плата`)) %>% 
  ggplot(aes(x = Месяц, y =  mean, group = 1)) +
  geom_line()

dat %>% 
  group_by(Месяц) %>% 
  summarise(value = mean(`Среднемесячная заработная плата`)) %>% 
  ggplot(aes(x = Месяц, y = value, group = 1)) +
  geom_line()


dat_f %>% 
  count(`Всего одобренных заявок`) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = `Всего одобренных заявок`, y = prop, fill = `Всего одобренных заявок`)) +
  geom_col(color = "grey20") +
  geom_text(aes(label = percent(prop), vjust = -1)) +
  scale_y_continuous(name = "Доля", labels = percent_format()) +
  scale_x_discrete(name = "Категория") +
  theme_minimal() +
  #theme(axis.title = element_blank()) +
  scale_fill_manual(values = c("lightpink", "steelblue", "palegreen3", "coral", "wheat")) +
  labs(y = "Lola")

dat %>% 
  ggplot(aes(y = `Среднемесячная заработная плата`)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(-2, 2))
