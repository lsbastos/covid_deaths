library(tidyverse)
library(vroom)
library(lubridate)
#library(geobr)


# Lendo os bancos

# Dados do opendatasus
# www.opendatasus.gov.br

# Data downloaded from opendatasus
srag2020 <- vroom("~/Downloads/INFLUD20-17-10-2022.csv")
srag2021 <- vroom("~/Downloads/INFLUD21-17-10-2022.csv")
srag2022 <- vroom("~/Downloads/INFLUD22-17-10-2022.csv")

obitos.all <- srag2020 %>% 
  filter(EVOLUCAO == 2) %>% 
  select(DT_EVOLUCA, CLASSI_FIN, NU_IDADE_N, TP_IDADE) %>% 
  bind_rows(
    srag2021 %>% 
      filter(EVOLUCAO == 2) %>% 
      select(DT_EVOLUCA, CLASSI_FIN, NU_IDADE_N, TP_IDADE)
  ) %>% 
  bind_rows(
    srag2022 %>% 
      filter(EVOLUCAO == 2) %>% 
      select(DT_EVOLUCA, CLASSI_FIN, NU_IDADE_N, TP_IDADE)  
  ) 

obitos.kids <- obitos.all %>% 
  mutate(
    Idade = case_when(
      TP_IDADE == 1 ~ "< 6 meses",
      TP_IDADE == 2 & NU_IDADE_N < 6 ~ "< 6 meses",
      TP_IDADE == 2 & NU_IDADE_N >= 6 ~ "De 6 meses a 2 anos incompletos",
      TP_IDADE == 3 & NU_IDADE_N < 2  ~ "De 6 meses a 2 anos incompletos",
      TP_IDADE == 3 & NU_IDADE_N >= 2 & NU_IDADE_N < 5  ~ "De 2 anos a 4 anos",
      TP_IDADE == 3 & NU_IDADE_N >= 5 & NU_IDADE_N < 10  ~ "De 5 anos a 9 anos",
      TRUE ~ NA_character_
    ),
    DT_EVOLUCA = dmy(DT_EVOLUCA),
    DT_EVOLUCA_year = year(DT_EVOLUCA)
  ) %>% drop_na(Idade)


obitos.kids <- obitos.kids %>% 
  mutate(
    Idade = factor(Idade, levels = c("< 6 meses","De 6 meses a 2 anos incompletos",
                                     "De 2 anos a 4 anos", "De 5 anos a 9 anos"),
                   ordered = T)
  ) 

obitos.kids %>% 
  # group_by(
  #   # DT_EVOLUCA_year,
  #   Idade
  # ) %>%
  summarise(
    obitosrag = n(),
    obitocovid = sum(CLASSI_FIN == 5, na.rm = T)
  )

obitos.kids %>% 
  drop_na(DT_EVOLUCA_year) %>% 
  group_by(
    DT_EVOLUCA_year,
    Idade
  ) %>%
  summarise(
    obitocovid = sum(CLASSI_FIN == 5, na.rm = T)
  ) %>% spread(key = DT_EVOLUCA_year, value = obitocovid)


obitos.kids %>% 
  group_by(DT_EVOLUCA) %>% 
  summarise(
    SRAG = n(),
    COVID = sum(CLASSI_FIN == 5, na.rm = T)
  ) %>% 
  gather(key = "Obito", value = "n", -DT_EVOLUCA) %>% 
  ggplot(aes(x = DT_EVOLUCA, y = n, color = Obito)) + 
  geom_line() + 
  labs(
    x = "Data do óbito",
    y = "Óbitos diários",
    color = ""
  ) +
  theme_classic(base_size = 16) + 
  theme( legend.position = c(0.8, 0.8))



obitos.kids %>% 
  filter(
    CLASSI_FIN == 5
  ) %>% 
  ggplot(aes(x = DT_EVOLUCA_year, fill = Idade)) + 
  geom_bar(position = 'dodge') + 
  labs(
    x = "Ano do óbito",
    y = "Óbitos por COVID-19 notificados no SIVEP",
    fill = "Idade"
  ) +
  theme_classic(base_size = 14) + 
  theme( legend.position = c(0.8, 0.85))
