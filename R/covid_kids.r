library(tidyverse)
library(vroom)
library(lubridate)
library(gridExtra)
library(ggthemes)


#########################################
# Parte comentado pode ser ignorada 
# Dados de criancas salvo no formato rds
# Ir direto para linha 93
#########################################

# # Lendo os bancos
# # Data downloaded from opendatasus
# # www.opendatasus
# srag2020 <- vroom("~/Downloads/INFLUD20-18-07-2022.csv")
# srag2021 <- vroom("~/Downloads/INFLUD21-18-07-2022.csv")
# srag2022 <- vroom("~/Downloads/INFLUD22-18-07-2022.csv")
# 
# 
# srag.all <- srag2020 %>% 
#   select(SG_UF, CO_MUN_RES, NU_IDADE_N, TP_IDADE, 
#          DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, DT_DIGITA, DT_ENTUTI,
#          EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI, 
#          PCR_RESUL, PCR_SARS2, AN_SARS2) %>% 
#   bind_rows(
#     srag2021 %>% 
#       select(SG_UF, CO_MUN_RES, NU_IDADE_N, TP_IDADE,  
#              DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, DT_DIGITA, DT_ENTUTI,
#              EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI, 
#              PCR_RESUL, PCR_SARS2, AN_SARS2) 
#   ) %>% 
#   bind_rows(
#     srag2022 %>% 
#       select(SG_UF, CO_MUN_RES, NU_IDADE_N, TP_IDADE, 
#              DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, DT_DIGITA, DT_ENTUTI,
#              EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI, 
#              PCR_RESUL, PCR_SARS2, AN_SARS2) 
#   )
# 
# 
# 
# srag2019 <- vroom("~/Downloads/INFLUD19.csv")
# 
# srag.all <- srag.all %>% 
#   bind_rows(
#     srag2019 %>% 
#       select(SG_UF, CO_MUN_RES, NU_IDADE_N, TP_IDADE, 
#              DT_SIN_PRI, DT_NOTIFIC, DT_COLETA, DT_DIGITA, DT_ENTUTI,
#              EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI, 
#              PCR_RESUL)) 
# 
# 
# srag.all <- srag.all %>% 
#   mutate_at( vars(starts_with("DT_")), dmy) %>% 
#   mutate(
#     COVID = case_when(
#       CLASSI_FIN == 5 | PCR_SARS2 == 1 | AN_SARS2 == 1 ~ 1,
#       CLASSI_FIN == 3 ~ NA_real_,
#       CLASSI_FIN == 2 ~ NA_real_,
#       CLASSI_FIN == 1 ~ NA_real_,
#       TRUE ~ 0
#     )
#   )

# # Usando arquivos brutos do opendatasus
# INFLUD20-18-07-2022.csv
# INFLUD21-18-07-2022.csv
# INFLUD22-18-07-2022.csv
# INFLUD19.csv

# srag.kids <- srag.all %>% 
#   mutate(
#     IDADE_cat = case_when(
#       TP_IDADE != 3 | NU_IDADE_N < 3 ~ 1,
#       TP_IDADE == 3 & NU_IDADE_N >= 3 & NU_IDADE_N < 5 ~ 2,
#       TP_IDADE == 3 & NU_IDADE_N >= 5 & NU_IDADE_N < 12 ~ 3,
#       TP_IDADE == 3 & NU_IDADE_N >= 12 & NU_IDADE_N < 18 ~ 4,
#       TP_IDADE == 3 & NU_IDADE_N >= 18 ~ 99,
#       TRUE ~ 99
#     ),
#     IDADE_cat = factor(IDADE_cat, 
#                        levels = c(1:4,99), 
#                        labels = c("[0 - 3)", "[3 - 5)",
#                                   "[5 - 12)", "[12 - 17)",
#                                   "99"), 
#                        ordered = T)
#   ) %>% 
#   filter(IDADE_cat != "99" ) 

## Salvando srag.kids 
# srag.kids %>% saveRDS(file = "Data/srag.kids.SIVEP-18-07-2022.rds")
srag.kids <- readRDS(file = "Data/srag.kids.SIVEP-18-07-2022.rds")

srag.kids %>% 
#  filter(TP_IDADE == 3, NU_IDADE_N > 5, NU_IDADE_N < 12, 
  filter(IDADE_cat == "[3 - 5)",
         DT_SIN_PRI < "2022-06-12",
  ) %>% 
  group_by(Ano = epiyear(DT_SIN_PRI)==2019) %>% 
  summarise(
    srag = n(),
    covid = sum(COVID, na.rm = T),
    obitosrag = sum(EVOLUCAO == 2, na.rm = T),
    obitocovid = sum(EVOLUCAO == 2 & COVID == 1, na.rm = T),
  )


pop <- tibble(
  IDADE_cat = factor(1:4, 
                     levels = 1:4, 
                     labels = c("[0 - 3)", "[3 - 5)",
                                "[5 - 12)", "[12 - 17)"), 
                     ordered = T),
  # POP projecao 2020 por idade IBGE (datasus)
  # http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/projpopbr.def
  pop = c(8214342,5630916,20877843,19577266))


srag.kids %>%  
  filter(DT_SIN_PRI < "2022-06-12") %>% 
  group_by(Ano = epiyear(DT_SIN_PRI)) %>% 
  summarise(
    srag = n(),
    covid = sum(COVID, na.rm = T),
    obitosrag = sum(EVOLUCAO == 2, na.rm = T),
    obitocovid = sum(EVOLUCAO == 2 & COVID == 1, na.rm = T),
  )


tbl.all <- srag.kids %>%  
  filter(DT_SIN_PRI < "2022-06-12") %>% 
  group_by(IDADE_cat) %>% 
  summarise(
    srag = n(),
    covid = sum(COVID, na.rm = T),
    obitosrag = sum(EVOLUCAO == 2, na.rm = T),
    obitocovid = sum(EVOLUCAO == 2 & COVID == 1, na.rm = T),
  )




tbl.all %>% 
  # Gambiarra com aaa necessaria pois IDADE_cat é um fator ordenado
  mutate(aaa = as.character(IDADE_cat)) %>% 
  left_join(pop %>% 
              mutate(aaa = as.character(IDADE_cat)) %>% 
              select(-IDADE_cat), 
            by = "aaa") %>% 
  select(-aaa) %>% 
  mutate(
    srag = srag / pop * 100000,
    covid = covid  / pop * 100000,
    obitosrag = obitosrag / pop * 100000,
    obitocovid = obitocovid / pop * 100000
  ) #%>% select(-pop)


covid.kids.epiweek <- srag.kids %>% 
  filter(DT_SIN_PRI < "2022-06-12") %>% 
  mutate(
    #DT_SIN_PRI.m = my(format(DT_SIN_PRI,"%m-%Y"))
    DT_SIN_PRI.m = DT_SIN_PRI - as.numeric(format(DT_SIN_PRI,"%w"))
  ) %>% 
  group_by(DT_SIN_PRI.m, IDADE_cat) %>% 
  summarise(
    srag = n(),
    covid = sum(COVID, na.rm = T),
    obitocovid = sum(EVOLUCAO == 2 & COVID == 1, na.rm = T)
  ) %>% ungroup() %>% 
  # Controlando pela populacao de 2020
  mutate(aaa = as.character(IDADE_cat)) %>% 
  left_join(pop %>% 
              mutate(aaa = as.character(IDADE_cat)) %>% 
              select(-IDADE_cat), 
            by = "aaa") %>% 
  select(-aaa) %>% 
  mutate(
    srag.prop = srag  / pop * 100000,
    covid.prop = covid  / pop * 100000,
    obitocovid.prop = obitocovid / pop * 100000
  )

# covid.kids.epiweek %>% write_csv(file = "covid.kids.epiweek.csv")

covid.kids.epiweek %>% 
  filter(
    IDADE_cat != "[0 - 3)",
    # epiyear(DT_SIN_PRI.m) > 2019
    ) %>%
  ggplot(aes(x = DT_SIN_PRI.m)) +
  geom_line(aes(y = srag.prop, color = IDADE_cat)) + 
  theme_bw(base_size = 16) +
  labs(
    x = "Epiweek",
    y = "SARI hospitalizations (per 100k people)",
    color = "Age group (in years)"
  ) + 
  theme(legend.position = c(0.1, 0.8)) + 
  geom_vline(xintercept = ymd(c("2021-01-17","2021-06-11", "2022-01-20")), 
             linetype = "dashed") + 
  scale_color_colorblind()

fig1.a <- covid.kids.epiweek %>% 
  filter(epiyear(DT_SIN_PRI.m) > 2019) %>% 
  ggplot(aes(x = DT_SIN_PRI.m)) +
  geom_line(aes(y = covid.prop, color = IDADE_cat)) + 
  theme_bw(base_size = 16) +
  labs(
    x = "Epiweek",
    y = "COVID-19 hospitalizations (per 100k people)",
    color = "Age group (in years)"
  ) + 
  theme(legend.position = c(0.175, 0.8)) + 
  geom_vline(xintercept = ymd(c("2021-01-17","2021-06-11", "2022-01-20")), 
             linetype = "dashed") + 
  scale_color_colorblind()
fig1.a

fig1.b <- covid.kids.epiweek %>% 
  filter(epiyear(DT_SIN_PRI.m) > 2019) %>% 
  ggplot(aes(x = DT_SIN_PRI.m)) +
  geom_line(aes(y = obitocovid.prop, color = IDADE_cat)) + 
  theme_bw(base_size = 16) +
  labs(
    x = "Epiweek",
    y = "COVID-19 deaths (per 100k people)",
    color = "Age group (in years)"
  ) + 
  theme(legend.position = c(0.175, 0.8)) + 
  geom_vline(xintercept = ymd(c("2021-01-17","2021-06-11", "2022-01-20")), 
             linetype = "dashed") + 
  scale_color_colorblind()

fig1.b

 
 # test <- grid.arrange(
 #   fig1.a, # + ggtitle("Hospitalizações por SRAG", subtitle = NULL),
 #   fig1.b, # + ggtitle("Hospitalizações por SRAG", subtitle = NULL),
 #   nrow = 1) 
 # 


fig1 <- marrangeGrob(grobs = list(a = fig1.a + ggtitle("A. Hospitalizations"), 
                                  b = fig1.b+ ggtitle("B. Deaths")), 
                     nrow=1, ncol=2, top = "")

png(filename = "fig1.png", res = 300, width = 48, height = 18, units = "cm")
fig1
dev.off()

png(filename = "fig1.a.png", res = 300, width = 24, height = 18, units = "cm")
fig1.a
dev.off()

png(filename = "fig1.b.png", res = 300, width = 24, height = 18, units = "cm")
fig1.b
dev.off()


tbl.all.ano <- srag.kids %>% 
  #filter(epiweek(DT_SIN_PRI)<29-5 ) %>% 
  group_by(Ano = epiyear(DT_SIN_PRI), IDADE_cat) %>% 
  summarise(
    srag = n(),
    covid = sum(COVID, na.rm = T),
    obitosrag = sum(EVOLUCAO == 2, na.rm = T),
    obitocovid = sum(EVOLUCAO == 2 & COVID == 1, na.rm = T),
  ) %>% 
  mutate(aaa = as.character(IDADE_cat)) %>% 
  left_join(pop %>% 
              mutate(aaa = as.character(IDADE_cat)) %>% 
              select(-IDADE_cat), 
            by = "aaa") %>% 
  select(-aaa) %>% 
  mutate(
    srag.prop = srag / pop * 100000,
    covid.prop = covid  / pop * 100000,
    obitosrag.prop = obitosrag / pop * 100000,
    obitocovid.prop = obitocovid / pop * 100000
  ) 

# tbl.all.ano %>% write_csv(file = "covid.kids.year.until_week_26.csv")


tbl.all.ano %>% 
  ggplot(aes(x = Ano, y = srag.prop, fill = IDADE_cat)) +
  geom_col(position = "dodge") + 
  theme_classic() + 
  labs(
    x = "Year",
    y = "SARI hospitalizations (per 100k people)",
    fill = "Age group (in years)"
  ) + theme(legend.position = c(.1,.8)) +
  scale_fill_colorblind()


fig3.a <- tbl.all.ano %>% 
  filter(Ano > 2019) %>% 
  ggplot(aes(x = Ano, y = covid.prop, fill = IDADE_cat)) +
  geom_col(position = "dodge") + 
  theme_classic(base_size = 16) + 
  labs(
    x = "Year",
    y = "COVID-19 hospitalizations (per 100k people)",
    fill = "Age group (in years)"
  ) + theme(legend.position = c(.2,.85)) + 
  scale_fill_colorblind()
fig3.a

fig3.b <- tbl.all.ano %>% 
  filter(Ano > 2019) %>% 
  ggplot(aes(x = Ano, y = obitocovid.prop, fill = IDADE_cat)) +
  geom_col(position = "dodge") + 
  theme_classic(base_size = 16) + 
  labs(
    x = "Year",
    y = "COVID-19 deaths (per 100k people)",
    fill = "Age group (in years)"
  ) + theme(legend.position = c(.8,.85)) +
  scale_fill_colorblind()
fig3.b

fig3 <- marrangeGrob(grobs = list(a = fig3.a + ggtitle("A."), 
                                  b = fig3.b+ ggtitle("B.")), 
                     nrow=1, ncol=2, top = "")

png(filename = "fig3.png", res = 300, width = 48, height = 18, units = "cm")
fig3
dev.off()

png(filename = "fig3.a.png", res = 300, width = 24, height = 18, units = "cm")
fig3.a
dev.off()

png(filename = "fig3.b.png", res = 300, width = 24, height = 18, units = "cm")
fig3.b
dev.off()



srag.kids %>% 
  filter(
      TP_IDADE == 3, NU_IDADE_N > 5, NU_IDADE_N < 12, 
      DT_SIN_PRI >= "2021-06-11", DT_SIN_PRI <= "2022-01-20"
    ) %>%   summarise(
      srag = n(),
      covid = sum(COVID, na.rm = T),
      obitosrag = sum(EVOLUCAO == 2, na.rm = T),
      obitocovid = sum(EVOLUCAO == 2 & COVID == 1, na.rm = T),
    )
