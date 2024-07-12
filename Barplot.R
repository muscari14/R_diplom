library(scales)

dat %>% 
  count(`Всего одобренных заявок`) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = `Всего одобренных заявок`, y = prop, fill = `Всего одобренных заявок`)) +
  geom_col(color = "grey20") +
  geom_text(aes(label = percent(prop), vjust = -1)) +
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
  
  
