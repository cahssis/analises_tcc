rm(list = ls())
setwd("~/Suicidio no Brasil-20230703T132807Z-001/Suicidio no Brasil")

library(data.table)
library(lubridate)
library(dplyr)
library(rio)
library(survival)
library(muhaz)
library(flexsurv)
library(ranger)
library(ggplot2)
library(ggfortify)
library(tidycmprsk)
library(ggsurvfit)
library(microdatasus)
library(magrittr)


base_bruta = fetch_datasus(year_start = 2014, year_end = 2021,
                           uf = "all", information_system = "SIM-DO")
base_bruta %<>% process_sim()
# base_bruta %>% export("./base_obito_bruta.xlsx")

base_bruta %>% names()

base = base_bruta %>% 
  select(DTOBITO, TIPOBITO, SEXO, RACACOR, ESTCIV, OCUP, LOCOCOR, ASSISTMED, IDADEanos,
         munResUf, munResNome, CODMUNNATU, CODMUNRES, CODMUNOCOR, ESC2010,
         NECROPSIA, CIRCOBITO, FONTE, CAUSABAS, CAUSABAS_O) %>% 
  mutate(IDADEanos = IDADEanos %>% as.numeric())


teste = base %>%
  filter(TIPOBITO == "Não Fetal" & CAUSABAS %in% c(paste0("X", 600:849)) &
           CAUSABAS_O %in% c(paste0("X", 600:849)) & IDADEanos > 4) %>% 
  mutate(morteoco = ifelse(CODMUNRES == CODMUNOCOR, 1, 0),
         mortenatu = ifelse(CODMUNRES == CODMUNNATU, 1, 0),
         regiao = case_when(munResUf %in% c("São Paulo", 'Espírito Santo', 'Minas Gerais', 'Rio de Janeiro') ~ 'Sudeste',
                            munResUf %in% c('Rio Grande do Sul', 'Paraná', 'Santa Catarina') ~ 'Sul',
                            munResUf %in% c('Mato Grosso', 'Goiás', 'Mato Grosso do Sul', 'Distrito Federal') ~ 'Centro-Oeste',
                            munResUf %in% c('Maranhão', 'Piauí', 'Ceará', 'Rio Grande do Norte', 'Paraíba', 'Pernambuco', 'Alagoas', 'Sergipe', 'Bahia') ~ 'Nordeste',
                            munResUf %in% c('Acre', 'Rondônia', 'Amazonas', 'Pará', 'Amapá', 'Roraima', 'Tocantins') ~ 'Norte'),
         mes = DTOBITO %>% as.Date() %>% format(., '%b'),
         ano = DTOBITO %>% as.Date() %>% format(., '%Y'),
         flag_sp = ifelse(munResUf == 'São Paulo', 1, 0),
         flag_pb = ifelse(munResUf == 'Paraíba', 1, 0),
         flag_aposentado = ifelse(OCUP == 'Aposentado/Pensionista', 1, 0),
         flag_estudante = ifelse(OCUP == "Estudante", 1, 0),
         faixa_etaria = case_when(IDADEanos >= 5 & IDADEanos < 15 ~ '5 a 14 anos',
                                  IDADEanos >= 15 & IDADEanos < 20 ~ '15 a 19 anos',
                                  IDADEanos >= 20 & IDADEanos < 40 ~ '20 a 39 anos',
                                  IDADEanos >= 40 & IDADEanos < 60 ~ '40 a 59 anos',
                                  IDADEanos >= 60 ~ '60 anos ou mais'),
         tri = case_when(mes %in% c("jan", 'fev', 'mar') ~ "1º Tri",
                         mes %in% c('abr', 'mai', 'jun') ~ '2º Tri',
                         mes %in% c('jul', 'ago', 'set') ~ '3º Tri',
                         mes %in% c('out', 'nov', 'dez') ~ '4º Tri'),
         pandemia = ifelse(ano > 2019, 1, 0),
         LOCOCOR = ifelse(LOCOCOR == "6", "Aldeia Indígena", LOCOCOR),
         censura_rc = case_when(CIRCOBITO == "Suicídio" ~ 1,
                                CIRCOBITO == "Homicídio" ~ 2,
                                CIRCOBITO == "Acidente" ~ 3,
                                CIRCOBITO == "Outro" ~ 0),
         censura_suicidio = ifelse(CIRCOBITO == 'Suicídio', 1, 0)) %>% 
  select(-DTOBITO, -munResUf, -munResNome, -mes, -CODMUNNATU, -CODMUNRES, -CODMUNOCOR,
         -TIPOBITO, -OCUP, -CAUSABAS, -CAUSABAS_O)

teste = teste[complete.cases(teste),]


teste %>% export("./base_obito_ajustada.xlsx")

base %>% export("./base_obito_bruta.xlsx")