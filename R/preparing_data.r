library(tidyverse)
library(vroom)
library(lubridate)
#library(geobr)


# Lendo os bancos

# Data downloaded from opendatasus
# www.opendatasus
srag2020 <- vroom("~/Downloads/INFLUD20-18-07-2022.csv")
srag2021 <- vroom("~/Downloads/INFLUD21-18-07-2022.csv")
srag2022 <- vroom("~/Downloads/INFLUD22-18-07-2022.csv")


srag.all <- srag2020 %>% 
  select(SG_UF, CO_MUN_RES, NU_IDADE_N, TP_IDADE, 
         DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, DT_DIGITA, DT_ENTUTI,
         EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI, 
         PCR_RESUL, PCR_SARS2, AN_SARS2) %>% 
  bind_rows(
    srag2021 %>% 
      select(SG_UF, CO_MUN_RES, NU_IDADE_N, TP_IDADE,  
             DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, DT_DIGITA, DT_ENTUTI,
             EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI, 
             PCR_RESUL, PCR_SARS2, AN_SARS2) 
  ) %>% 
  bind_rows(
    srag2022 %>% 
      select(SG_UF, CO_MUN_RES, NU_IDADE_N, TP_IDADE, 
             DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, DT_DIGITA, DT_ENTUTI,
             EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI, 
             PCR_RESUL, PCR_SARS2, AN_SARS2) 
  )



srag2019 <- vroom("~/Downloads/INFLUD19.csv")

srag.all <- srag.all %>% 
  bind_rows(
  srag2019 %>% 
  select(SG_UF, CO_MUN_RES, NU_IDADE_N, TP_IDADE, 
         DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, DT_DIGITA, DT_ENTUTI,
         EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI, 
         PCR_RESUL)) 


srag.all <- srag.all %>% 
  mutate_at( vars(starts_with("DT_")), dmy) %>% 
  mutate(
    COVID = case_when(
      CLASSI_FIN == 5 | PCR_SARS2 == 1 | AN_SARS2 == 1 ~ 1,
      CLASSI_FIN == 3 ~ NA_real_,
      CLASSI_FIN == 2 ~ NA_real_,
      CLASSI_FIN == 1 ~ NA_real_,
      TRUE ~ 0
    )
  )

# srag.all %>% filter(EVOLUCAO == 2, SG_UF == "SP") %>%
#   group_by(DT_EVOLUCA) %>%
#   summarise(
#     SRAG = n(),
#     SRAG_COVID = sum(COVID == 1, na.rm = T),
#     ) %>%
#   mutate(
#     SRAGcum = cumsum(SRAG),
#     SRAG_COVIDcum = cumsum(SRAG_COVID),
#   ) %>% write_csv2(file = "obitos_srag_sragcovid_SP.csv")

srag.all %>% 
  filter(EVOLUCAO == 2, COVID ==1, DT_EVOLUCA < "2020-03-18") %>% 
  View()

srag.all %>% 
  filter(EVOLUCAO == 2) %>%
  group_by(COVID) %>% 
  summarise(
    n = n(),
    n2 = sum(DT_EVOLUCA <= "2022-03-11", na.rm = T)
  )

srag.all %>% 
  filter(COVID == 0, EVOLUCAO == 2) %>%
  #group_by(PCR_RESUL, PCR_SARS2, AN_SARS2) %>% 
  summarise(
    n = n(),
    n2 = sum(DT_DIGITA <= "2022-03-11")
  )


srag.all %>% 
  filter(CLASSI_FIN == 4, EVOLUCAO == 2, 
         DT_DIGITA <= "2022-03-11") %>% 
  mutate(
    Coleta = cut(as.numeric(DT_COLETA - DT_SIN_PRI), 
                 breaks = c(0,7,14,21,28,Inf), right = F)
  ) %>% 
  group_by(PCR_RESUL == 2, Coleta) %>% tally() %>% 
  mutate(Prop = n/sum(n) * 100)

srag.all %>% 
  filter(CLASSI_FIN == 5, EVOLUCAO == 2, 
         DT_DIGITA <= "2022-03-11") %>% 
  mutate(
    Coleta = cut(as.numeric(DT_COLETA - DT_SIN_PRI), 
                 breaks = c(0,7,14,21,28,Inf), right = F)
  ) %>% 
  group_by(Coleta, PCR_SARS2, is.na(DT_COLETA)) %>% tally() %>% 
  mutate(Prop = n/sum(n) * 100)


srag.all %>% 
  filter(EVOLUCAO == 2, 
         #DT_DIGITA <= "2022-03-11",
         DT_EVOLUCA >= "2020-03-11",
         DT_EVOLUCA < "2022-03-11") %>% 
  summarise(
    srag = n(),
    covid_susp = sum(CLASSI_FIN > 3 | is.na(CLASSI_FIN)),
    covid_conf = sum(CLASSI_FIN == 5, na.rm = T)
  ) 



# srag.all %>% 
#   # mutate(
#   #   DT_NOTIFIC = dmy(DT_NOTIFIC),
#   #   DT_EVOLUCA = dmy(DT_EVOLUCA)
#   # ) %>% 
#   group_by(CLASSI_FIN, EVOLUCAO) %>% 
#   summarise(
#     Notificacao = min(DT_NOTIFIC, na.rm = T),
#     Evolucao = min(DT_EVOLUCA, na.rm = T)
#   ) -> aaa


srag.all %>% 
  # mutate(
  #   DT_NOTIFIC = dmy(DT_NOTIFIC),
  #   DT_EVOLUCA = dmy(DT_EVOLUCA)
  # ) %>% 
  filter(EVOLUCAO ==2) %>% 
  group_by(SG_UF, DT_EVOLUCA) %>% 
  summarise(
    srag = n(),
    covid_susp = sum(CLASSI_FIN > 3 | is.na(CLASSI_FIN)),
    covid_conf = sum(CLASSI_FIN == 5, na.rm = T)
  ) -> aaa

library(geofacet)

aaa %>% 
  drop_na(SG_UF) %>% 
  #filter(SG_UF == "SP") %>% 
  ggplot(aes(x = DT_EVOLUCA)) +
  geom_line(aes(y = srag, color = "SRAG")) + 
  geom_line(aes(y = covid_susp, color = "Suspected COVID")) + 
  geom_line(aes(y = covid_conf, color = "Confirmed COVID")) + 
  theme_bw() + 
  labs(
    x = "Data do óbito",
    y = "Óbitos",
    color = ""
  ) +
  facet_geo(~ SG_UF, grid = "br_states_grid1", 
            scales = "free_y") 
  

aaa %>% 
  drop_na(SG_UF) %>% 
#  filter(SG_UF == "AM") %>% 
  ggplot(aes(x = DT_EVOLUCA)) +
  geom_line(aes(y = covid_susp - covid_conf)) + 
  # geom_line(aes(y = srag - covid_susp, color = "SRAG - Suspected COVID")) + 
  # geom_line(aes(y = srag - covid_conf, color = "SRAG - Confirmed COVID")) + 
  theme_bw() + 
  labs(
    x = "Data do óbito",
    y = "Diferença entre óbitos suspeitos e confirmados") +
  facet_geo(~ SG_UF, grid = "br_states_grid2", 
            scales = "free_y") 



  
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
