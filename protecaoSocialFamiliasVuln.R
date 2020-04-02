library(tidyverse)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)
library(knitr)


options(scipen = 999999999) # desativa notação científica


setwd("C:/Users/Claudio/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/ProteçãoSocial/BDs")
pessoas <- read.xlsx("BDPessoas2020.xlsx", cols = c(1:6, 11, 22, 44, 124, 128, 132, 136, 140, 144, 161, 163, 165, 168))
pessoas[pessoas=="[NA]"] <- NA


# Recofica variáveis de renda para numéricas (substitui pontos por vazio e vírgulas por pontos)
pessoas$`2.2.79` <- as.numeric(str_replace_all(pessoas$`2.2.79`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.83` <- as.numeric(str_replace_all(pessoas$`2.2.83`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.87` <- as.numeric(str_replace_all(pessoas$`2.2.87`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.91` <- as.numeric(str_replace_all(pessoas$`2.2.91`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.95` <- as.numeric(str_replace_all(pessoas$`2.2.95`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.99` <- as.numeric(str_replace_all(pessoas$`2.2.99`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.109` <- as.numeric(str_replace_all(pessoas$`2.2.109`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.111` <- as.numeric(str_replace_all(pessoas$`2.2.111`, c("\\." = "",
                                                                     "," = "\\.")))

pessoas$`2.2.113` <- as.numeric(str_replace_all(pessoas$`2.2.113`, c("\\." = "",
                                                                     "," = "\\.")))

pessoas$`2.2.116` <- as.numeric(str_replace_all(pessoas$`2.2.116`, c("\\." = "",
                                                                     "," = "\\.")))


# Variáveis de data
pessoas$`2.2.10.1.a` <- as.Date(pessoas$`2.2.10.1.a`, tryFormats = "%d/%m/%Y")
pessoas$C3 <- as.Date(pessoas$C3, origin = "1899-12-30")


# Cria variável idade (considerar a data da entrevista (C3) ou uma data mais atual)
pessoas <- add_column(pessoas, "idade" = as.numeric(""), .after = "2.2.10.1.a")
pessoas$idade <- trunc(time_length(interval(ymd(pessoas$`2.2.10.1.a`), ymd("2020-03-31")), "year"))


# Cria variável de público vulnerável (crinças, adolescentes ou idosos)
pessoas <- add_column(pessoas, "publicoVulneravel" = as.numeric(""), .after = "idade")
pessoas$publicoVulneravel <- ifelse(pessoas$idade < 12, 1,
                                       ifelse(pessoas$idade >= 12 & pessoas$idade < 18, 1,
                                              ifelse(pessoas$idade >=60, 1, 0)))


# Cria variável de salário minimo (com valores de cada ano)
pessoas <- add_column(pessoas, "SM" = as.numeric(""), .after = "2.2.32")
pessoas$SM <- ifelse(pessoas$C3 < "2016-01-01", 788,
                  ifelse(pessoas$C3 >= "2016-01-01" &  pessoas$C3 < "2017-01-01", 880,
                         ifelse(pessoas$C3 >= "2017-01-01" & pessoas$C3 < "2018-01-01", 937,
                                ifelse(pessoas$C3 >= "2018-01-01" & pessoas$C3 < "2019-01-01", 954, 998))))


# Deficiência
pessoas$`2.2.32` <- ifelse(pessoas$`2.2.32` == "Sim", 1, 0)


# Cria base de famílias com as variáveis
familias <- 
     pessoas %>% 
     mutate(SM = SM/2) %>% 
     group_by(ID_SGC) %>% 
     summarise(RendaTotal = sum(`2.2.79`, `2.2.83`,`2.2.87`, `2.2.91`, `2.2.95`, `2.2.99`, na.rm = TRUE),
               Npessoas = n(),
               PerCapita = RendaTotal/Npessoas,
               SM_Entrevista = max(SM),
               RendaSM = round(PerCapita/SM_Entrevista, digits = 1),
               PessoasVulneraveis = sum(publicoVulneravel, na.rm = TRUE),
               PessoasDeficiencia = sum(`2.2.32`, na.rm = TRUE)) %>% 
     mutate(BaixaRenda = ifelse(RendaSM <= 0.5, 1, 0)) %>% 
     mutate(FamiliaVulneravel = ifelse(BaixaRenda == 1 & (PessoasVulneraveis > 0 | PessoasDeficiencia > 0), 1, 0))
     

familias %>% summarise(N = sum(FamiliaVulneravel)) # conta o número de famílias vulneráveis



setwd("C:/Users/MAGNA TI/OneDrive - HERKENHOFF & PRATES/FundRenova/ProteçãoSocial")
write.xlsx(list(familias = familias,
                pessoas = base), "base.xlsx")