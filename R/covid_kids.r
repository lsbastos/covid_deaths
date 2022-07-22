# Rode as primeiras 60 linhas de preparing_data

srag.kids <- srag.all %>% 
  mutate(
    IDADE_cat = case_when(
      TP_IDADE != 3 | NU_IDADE_N < 3 ~ 1,
      TP_IDADE == 3 & NU_IDADE_N >= 3 & NU_IDADE_N < 5 ~ 2,
      TP_IDADE == 3 & NU_IDADE_N >= 5 & NU_IDADE_N < 12 ~ 3,
      TP_IDADE == 3 & NU_IDADE_N >= 12 & NU_IDADE_N < 18 ~ 4,
      TP_IDADE == 3 & NU_IDADE_N >= 18 ~ 99,
      TRUE ~ 99
    ),
    IDADE_cat = factor(IDADE_cat, 
                       levels = c(1:4,99), 
                       labels = c("[0 - 3)", "[3 - 5)",
                                  "[5 - 12)", "[12 - 17)",
                                  "99"), 
                       ordered = T)
  ) %>% 
  filter(IDADE_cat != "99" ) 


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
  theme_bw() +
  labs(
    x = "Epiweek",
    y = "SARI hospitalizations (per 100k people)",
    color = "Age group (in years)"
  ) + 
  theme(legend.position = c(0.1, 0.8)) + 
  geom_vline(xintercept = ymd(c("2021-01-17","2021-06-11", "2022-01-20")), 
             linetype = "dashed")

covid.kids.epiweek %>% 
  filter(epiyear(DT_SIN_PRI.m) > 2019) %>% 
  ggplot(aes(x = DT_SIN_PRI.m)) +
  geom_line(aes(y = covid.prop, color = IDADE_cat)) + 
  theme_bw() +
  labs(
    x = "Epiweek",
    y = "COVID-19 hospitalizations (per 100k people)",
    color = "Age group (in years)"
  ) + 
  theme(legend.position = c(0.1, 0.8)) + 
  geom_vline(xintercept = ymd(c("2021-01-17","2021-06-11", "2022-01-20")), 
             linetype = "dashed")


 covid.kids.epiweek %>% 
   filter(epiyear(DT_SIN_PRI.m) > 2019) %>% 
   ggplot(aes(x = DT_SIN_PRI.m)) +
  geom_line(aes(y = obitocovid.prop, color = IDADE_cat)) + 
  theme_bw() +
  labs(
    x = "Epiweek",
    y = "COVID-19 deaths (per 100k people)",
    color = "Age group (in years)"
  ) + 
  theme(legend.position = c(0.1, 0.8)) + 
  geom_vline(xintercept = ymd(c("2021-01-17","2021-06-11", "2022-01-20")), 
             linetype = "dashed")



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
  ) + theme(legend.position = c(.1,.8))


tbl.all.ano %>% 
  filter(Ano > 2019) %>% 
  ggplot(aes(x = Ano, y = covid.prop, fill = IDADE_cat)) +
  geom_col(position = "dodge") + 
  theme_classic() + 
  labs(
    x = "Year",
    y = "COVID-19 hospitalizations (per 100k people)",
    fill = "Age group (in years)"
  ) + theme(legend.position = c(.1,.8))

tbl.all.ano %>% 
  filter(Ano > 2019) %>% 
  ggplot(aes(x = Ano, y = obitocovid.prop, fill = IDADE_cat)) +
  geom_col(position = "dodge") + 
  theme_classic() + 
  labs(
    x = "Year",
    y = "COVID-19 deaths (per 100k people)",
    fill = "Age group (in years)"
  ) + theme(legend.position = c(.1,.8))



srag.all %>% 
  filter(
      TP_IDADE == 3, NU_IDADE_N > 5, NU_IDADE_N < 12, 
      DT_SIN_PRI >= "2021-06-11", DT_SIN_PRI <= "2022-01-20"
    ) %>%   summarise(
      srag = n(),
      covid = sum(COVID, na.rm = T),
      obitosrag = sum(EVOLUCAO == 2, na.rm = T),
      obitocovid = sum(EVOLUCAO == 2 & COVID == 1, na.rm = T),
    )
