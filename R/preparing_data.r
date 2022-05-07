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
  select(SG_UF, CO_MUN_RES, DT_SIN_PRI, DT_NOTIFIC, 
         EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI) %>% 
  bind_rows(
    srag2021 %>% 
      select(SG_UF, CO_MUN_RES, DT_SIN_PRI, DT_NOTIFIC, 
             EVOLUCAO, DT_EVOLUCA, CLASSI_FIN, HOSPITAL, UTI) 
  ) %>% 
  bind_rows(
    srag2022 %>% 
      select(SG_UF, CO_MUN_RES, DT_SIN_PRI, DT_NOTIFIC, 
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
