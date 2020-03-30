library(tidyverse)
library(stringr)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)


setwd("C:/Users/MAGNA TI/OneDrive - HERKENHOFF & PRATES")

base <- read.xlsx("ExtracaoPessoas20201103.xlsx")

base[base=="[NA]"] <- NA
base[base=="NULL"] <- NA

base <- base[, c(1, 4:15, 18, 20, 95)]

base$DATA_ENTREVISTA <- as.Date(base$DATA_ENTREVISTA, origin = "1899-12-30")

base$Vulnerável_Considerado[base$Vulnerável_Considerado==0] <- "Não"

base$SM <- ifelse(base$DATA_ENTREVISTA < "2016-01-01", 788,
                  ifelse(base$DATA_ENTREVISTA >= "2016-01-01" &  base$DATA_ENTREVISTA < "2017-01-01", 880,
                         ifelse(base$DATA_ENTREVISTA >= "2017-01-01" & base$DATA_ENTREVISTA < "2018-01-01", 937,
                                ifelse(base$DATA_ENTREVISTA >= "2018-01-01" & base$DATA_ENTREVISTA < "2019-01-01", 954, 998))))

base$vulnSM <- ifelse(base$FAM_Renda_PerCapita_Entrevista <= base$SM/2, "Sim", "Não")


base$vulneravel <- ifelse(((base$IDADE_ENTREVISTA >= 60 | base$IDADE_ENTREVISTA < 18) |
                                base$`POSSUI.ALGUMA.DEFICIÊNCIA?` == "Sim" |
                                base$vulnSM == "Sim"), "Sim", "Não")

base$grupoEtario <- ifelse(base$IDADE_ENTREVISTA < 12, "Criança",
                           ifelse(base$IDADE_ENTREVISTA >= 12 & base$IDADE_ENTREVISTA <18, "Adolescente",
                                  ifelse(base$IDADE_ENTREVISTA >= 18 & base$IDADE_ENTREVISTA < 60, "Adulto", "Idoso")))

base$jovem <- ifelse(base$IDADE_ENTREVISTA >= 18 & base$IDADE_ENTREVISTA <= 29 , "Jovem", "Não jovem")


familias <- base %>% 
     group_by(ID_SGC, vulneravel) %>% 
     summarise(n = n()) %>% 
     pivot_wider(names_from = vulneravel, values_from = n)

familias[is.na(familias)] <- 0

familias$Não <- NULL
familias$Sim[familias$Sim >0] <- 1
familias <- rename(familias, vulneravel = Sim)

familiasRenda <- base %>% 
     group_by(ID_SGC, vulnSM) %>% 
     summarise(n = n()) %>% 
     pivot_wider(names_from = vulnSM, values_from = n)

familiasRenda[is.na(familiasRenda)] <- 0

colnames(familiasRenda) = c("ID_SGC",
                            "Vulneravel",
                            "NaoVulneravel")


familiasDef <- base %>% 
     group_by(ID_SGC, `POSSUI.ALGUMA.DEFICIÊNCIA?`) %>%
     summarise(n = n()) %>% 
     pivot_wider(names_from = `POSSUI.ALGUMA.DEFICIÊNCIA?`, values_from = n)

familiasDef[is.na(familiasDef)] <- 0

colnames(familiasDef) = c("ID_SGC",
                          "PessoasDeficienca",
                          "PessoasNaoDeficienca",
                          "PessoasNaoRespondeu",
                          "PessoasNaoSabe")


familias <- merge(familias, familiasDef, by = "ID_SGC")
familias <- merge(familias, familiasRenda, by = "ID_SGC")

familias <- familias[c("ID_SGC",
                       "Adulto",
                       "Adolescente",
                       "Criança",
                       "Idoso",
                       "PessoasDeficienca",
                       "Vulneravel")]

setwd("C:/Users/MAGNA TI/OneDrive - HERKENHOFF & PRATES/FundRenova/ProteçãoSocial")
write.xlsx(list(familias = familias,
                pessoas = base), "base.xlsx")