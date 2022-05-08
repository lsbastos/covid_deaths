library(tidyverse)
library(vroom)
library(lubridate)
#library(geobr)


# Lendo os bancos

# Data downloaded from opendatasus
srag2020 <- vroom("~/Downloads/INFLUD20-02-05-2022.csv")
srag2021 <- vroom("~/Downloads/INFLUD21-02-05-2022.csv")
srag2022 <- vroom("~/Downloads/INFLUD22-02-05-2022.csv")


srag.all <- srag2020 %>% 
  select(SG_UF, CO_MUN_RES, DT_SIN_PRI, DT_NOTIFIC, DT_COLETA,
         EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI) %>% 
  bind_rows(
    srag2021 %>% 
      select(SG_UF, CO_MUN_RES, DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, 
             EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI) 
  ) %>% 
  bind_rows(
    srag2022 %>% 
      select(SG_UF, CO_MUN_RES, DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, 
             EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI) 
  )

srag.all %>% 
  mutate(
    DT_NOTIFIC = dmy(DT_NOTIFIC),
    DT_EVOLUCA = dmy(DT_EVOLUCA)
  ) %>% 
  group_by(CLASSI_FIN, EVOLUCAO) %>% 
  summarise(
    Notificacao = min(DT_NOTIFIC, na.rm = T),
    Evolucao = min(DT_EVOLUCA, na.rm = T)
  ) -> aaa


srag.all %>% 
  mutate(
    DT_NOTIFIC = dmy(DT_NOTIFIC),
    DT_EVOLUCA = dmy(DT_EVOLUCA)
  ) %>% 
  filter(EVOLUCAO ==2) %>% 
  group_by(DT_EVOLUCA) %>% 
  summarise(
    srag = n(),
    covid_susp = sum(CLASSI_FIN > 3 | is.na(CLASSI_FIN)),
    covid_conf = sum(CLASSI_FIN == 5, na.rm = T)
  ) -> aaa

aaa %>% ggplot(aes(x = DT_EVOLUCA)) +
  geom_line(aes(y = srag, color = "SRAG")) + 
  geom_line(aes(y = covid_susp, color = "Suspected COVID")) + 
  geom_line(aes(y = covid_conf, color = "Confirmed COVID")) + 
  theme_bw() + 
  labs(
    x = "Data do óbito",
    y = "Óbitos",
    color = ""
  )
  
aaa %>% 
  #group_by(ANO = year(DT_EVOLUCA)) %>% 
  summarise(
    srag = sum(srag), 
    covid_susp = sum(covid_susp), 
    covid_conf = sum(covid_conf)) %>% 
  mutate(prop = 1-covid_conf/covid_susp)
  

srag.all %>% 
  mutate(
    DT_SIN_PRI = dmy(DT_SIN_PRI),
    DT_COLETA = dmy(DT_COLETA),
    DT_EVOLUCA = dmy(DT_EVOLUCA),
    Tempo = as.numeric(DT_COLETA - DT_SIN_PRI)
  ) %>% 
  filter(EVOLUCAO ==2) %>% 
  group_by(DT_EVOLUCA, CLASSI_FIN) %>% 
  summarise(
    n = n(),
    TimeQ1 = quantile(Tempo, probs = .25, na.rm=T),
    TimeQ2 = quantile(Tempo, probs = .5, na.rm=T),
    TimeQ3 = quantile(Tempo, probs = .75, na.rm=T),
    TimeNA = sum(is.na(Tempo))
    # covid_susp = sum(CLASSI_FIN > 3 | is.na(CLASSI_FIN)),
    # covid_conf = sum(CLASSI_FIN == 5, na.rm = T)
  ) -> bbb

bbb %>% ggplot(aes(x = DT_EVOLUCA)) +
  geom_pointrange(aes(y = TimeQ2, ymin = TimeQ1, ymax = TimeQ3, color = factor(CLASSI_FIN), group = CLASSI_FIN)) + 
  theme_bw() + 
  labs(
    x = "Data do óbito",
    y = "Tempo entre 1os sintomas e coleta",
    color = ""
  ) + ylim(0,100) + facet_wrap(~CLASSI_FIN, scales = "free_y")

bbb %>% ggplot(aes(x = DT_EVOLUCA)) +
  geom_line(aes(y = n, color = factor(CLASSI_FIN), group = CLASSI_FIN)) + 
  theme_bw() + 
  labs(
    x = "Data do óbito",
    y = "Óbitos",
    color = ""
  ) + facet_wrap(~CLASSI_FIN, scales = "free")




srag.all %>% 
  mutate(
    DT_SIN_PRI = dmy(DT_SIN_PRI),
    DT_COLETA = dmy(DT_COLETA),
    DT_EVOLUCA = dmy(DT_EVOLUCA),
    DT_NOTIFIC = dmy(DT_NOTIFIC),
    Tempo = as.numeric(DT_COLETA - DT_SIN_PRI)
  ) %>% filter(Tempo < 30) %>% 
  ggplot(aes(x = Tempo)) +
  geom_histogram() +
  #geom_boxplot(aes(y = Tempo, fill = factor(CLASSI_FIN))) + 
  theme_bw() + 
  labs(
    x = "Classificacao final",
    y = "Tempo entre 1os sintomas e coleta",
    color = ""
  ) + facet_wrap(~CLASSI_FIN, scales = "free")
