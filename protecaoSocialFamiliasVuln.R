library(tidyverse)
library(reshape2)
library(openxlsx)
library(lubridate)
library(RColorBrewer)
library(knitr)



options(scipen = 999999999) # desativa notação científica


setwd(paste0("C:/Users/",Sys.info()[[7]],"/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/ProteçãoSocial/BDs"))
list.files()
pessoas <- read.xlsx("BDPessoas2020.xlsx",
                     cols = c(1:6, 11, 20, 22, 44, 124, 128, 132, 136, 140, 144, 161, 163, 165, 168))
pessoas[pessoas=="[NA]"] <- NA



# Recofica variáveis de renda para numéricas (substitui pontos por vazio e vírgulas por pontos)
pessoas$`2.2.79` <- as.double(str_replace_all(pessoas$`2.2.79`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.83` <- as.double(str_replace_all(pessoas$`2.2.83`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.87` <- as.double(str_replace_all(pessoas$`2.2.87`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.91` <- as.double(str_replace_all(pessoas$`2.2.91`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.95` <- as.double(str_replace_all(pessoas$`2.2.95`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.99` <- as.double(str_replace_all(pessoas$`2.2.99`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.109` <- as.double(str_replace_all(pessoas$`2.2.109`, c("\\." = "",
                                                                   "," = "\\.")))

pessoas$`2.2.111` <- as.double(str_replace_all(pessoas$`2.2.111`, c("\\." = "",
                                                                     "," = "\\.")))

pessoas$`2.2.113` <- as.double(str_replace_all(pessoas$`2.2.113`, c("\\." = "",
                                                                     "," = "\\.")))

pessoas$`2.2.116` <- as.double(str_replace_all(pessoas$`2.2.116`, c("\\." = "",
                                                                     "," = "\\.")))


# Variáveis de data
pessoas$`2.2.10.1.a` <- as.Date(pessoas$`2.2.10.1.a`, tryFormats = "%d/%m/%Y")
pessoas$C3 <- as.Date(pessoas$C3, origin = "1899-12-30")


# Cria variável idade (considerar a data da entrevista (C3) ou uma data mais atual)
# pessoas <- add_column(pessoas, "idade" = as.numeric(""), .after = "2.2.10.1.a")
# pessoas$idade <- trunc(time_length(interval(ymd(pessoas$`2.2.10.1.a`), ymd(pessoas$C3)), "year"))



# Cria base de famílias com as variáveis
familias <- 
     pessoas %>% 
        mutate(idade = `2.2.9`,
               crianca = ifelse(idade < 12, 1, 0),
               adolescente = ifelse(idade %in% c(12:17), 1, 0),
               adulto = ifelse(idade %in% c(18:59), 1,0),
               idoso = ifelse(idade >= 60, 1, 0),
               jovem = ifelse(idade %in% c(15:29), 1, 0),
               pessoaDeficiencia = ifelse(pessoas$`2.2.32` == "Sim", 1, 0),
               SM = ifelse(pessoas$C3 < "2016-01-01", 788,
                           ifelse(pessoas$C3 >= "2016-01-01" &  pessoas$C3 < "2017-01-01", 880,
                                  ifelse(pessoas$C3 >= "2017-01-01" & pessoas$C3 < "2018-01-01", 937,
                                         ifelse(pessoas$C3 >= "2018-01-01" & pessoas$C3 < "2019-01-01", 954, 998)))),
               masculino = ifelse(`2.2.4` == "Masculino", 1, 0),
               feminino = ifelse(`2.2.4` == "Feminino", 1, 0)) %>% 
        group_by(ID_SGC) %>% 
        summarise(RendaTotal = sum(`2.2.79`, `2.2.83`,`2.2.87`, `2.2.91`, `2.2.95`, `2.2.99`, `2.2.116`, na.rm = TRUE),
                  Membros = n(),
                  PerCapita = RendaTotal/Membros,
                  SM = mean(SM),
                  RendaSM = PerCapita/SM,
                  Homens = sum(masculino),
                  Mulheres = sum(feminino),
                  Criancas = sum(crianca),
                  Adolescentes = sum(adolescente),
                  Idosos = sum(idoso),
                  PessoasDeficiencia = sum(pessoaDeficiencia),
                  PublicoVulneravel = sum(crianca, adolescente, idoso, pessoaDeficiencia)) %>% 
        mutate(BaixaRenda = ifelse(RendaSM <= 0.5, 1, 0),
               FamiliaVulneravel = ifelse(BaixaRenda == 1 & (PublicoVulneravel > 0), 1, 0),
               RendaSM = NULL,
               PerCapita = round(PerCapita, digits = 2),
               SM = NULL)


familias %>% summarise(TotalPessoas = sum(Membros),
                       PublicoVulneravel = sum(PublicoVulneravel),
                       PessoasDeficiencia = sum(PessoasDeficiencia),
                       Familias = n(),
                       FamiliasVulneraveis = sum(FamiliaVulneravel)) # conta o número de famílias vulneráveis



# Define diretório e salva as duas vases
setwd(paste0("C:/Users/",Sys.info()[[7]],"/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/ProteçãoSocial/BDs"))
write.xlsx(list(familias = familias,
                pessoas = pessoas), "ProtecaoSocialBaseCalculoV3.xlsx")



