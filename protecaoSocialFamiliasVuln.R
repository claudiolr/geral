library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(knitr)



options(scipen = 99) # desativa notação científica


# [1] ID                                                [2] ID_SGC                                              [3] C1
# [4] C3                                                [5] codpessoa                                           [6] P
# [7] 2.2.1                                             [8] 2.2.1.1.a                                           [9] 2.2.2
# [10] 2.2.3                                            [11] 2.2.4                                              [12] 2.2.5
# [13] 2.2.5.28.a                                       [14] 2.2.6                                              [15] 2.2.6.35.a
# [16] 2.2.7                                            [17] 2.2.7.6.a                                          [18] 2.2.8
# [19] 2.2.8.1.a                                        [20] 2.2.9                                              [21] 2.2.10
# [22] 2.2.10.1.a                                       [23] 2.2.11                                             [24] 2.2.11.1.a
# [25] 2.2.13                                           [26] 2.2.14                                             [27] 2.2.15
# [28] 2.2.16                                           [29] 2.2.17                                             [30] 2.2.17.1.a
# [31] 2.2.19                                           [32] 2.2.19.1.a                                         [33] 2.2.21
# [34] 2.2.21.1.a                                       [35] 2.2.22                                             [36] 2.2.22.1.a
# [37] 2.2.24                                           [38] 2.2.24.1.a                                         [39] 2.2.26
# [40] 2.2.26.1.a                                       [41] 2.2.29                                             [42] 2.2.30
# [43] 2.2.30.1.a                                       [44] 2.2.32                                             [45] 2.2.33.1
# [46] 2.2.33.2                                         [47] 2.2.33.3                                           [48] 2.2.33.4
# [49] 2.2.33.5                                         [50] 2.2.33.98                                          [51] 2.2.33.99
# [52] 2.2.34                                           [53] 2.2.35                                             [54] 2.2.36
# [55] 2.2.37                                           [56] 2.2.38.1                                           [57] 2.2.38.2
# [58] 2.2.38.3                                         [59] 2.2.38.4                                           [60] 2.2.38.5
# [61] 2.2.38.6                                         [62] 2.2.38.7                                           [63] 2.2.38.8
# [64] 2.2.38.9                                         [65] 2.2.38.10                                          [66] 2.2.38.11
# [67] 2.2.38.12                                        [68] 2.2.38.13                                          [69] 2.2.38.98
# [70] 2.2.38.99                                        [71] 2.2.39                                             [72] 2.2.40
# [73] 2.2.41                                           [74] 2.2.41.2.a                                         [75] 2.2.41.3.a
# [76] 2.2.41.4.a                                       [77] 2.2.42                                             [78] 2.2.43
# [79] 2.2.44                                           [80] 2.2.46                                             [81] 2.2.47
# [82] 2.2.48                                           [83] 2.2.49                                             [84] 2.2.50
# [85] 2.2.51                                           [86] 2.2.52                                             [87] 2.2.53
# [88] 2.2.54                                           [89] 2.2.54.2.a                                         [90] 2.2.54.3.a
# [91] 2.2.54.4.a                                       [92] 2.2.55                                             [93] 2.2.55.1.a
# [94] 2.2.56                                           [95] 2.2.57                                             [96] 2.2.57/1
# [97] 2.2.58                                           [98] 2.2.59                                             [99] 2.2.59.1.a
# [100] 2.2.61                                          [101] 2.2.61.12.a                                       [102] 2.2.62
# [103] 2.2.63                                          [104] 2.2.64                                            [105] 2.2.65
# [106] 2.2.66                                          [107] 2.2.66.12.a                                       [108] 2.2.67
# [109] 2.2.68                                          [110] 2.2.69                                            [111] 2.2.70
# [112] 2.2.70/1                                        [113] 2.2.71                                            [114] 2.2.72
# [115] 2.2.73                                          [116] 2.2.74                                            [117] 2.2.75
# [118] 2.2.76                                          [119] 2.2.76.2.a                                        [120] 2.2.77
# [121] 2.2.77.5.a                                      [122] 2.2.77.7.a                                        [123] 2.2.78
# [124] 2.2.79                                          [125] 2.2.80                                            [126] 2.2.81
# [127] 2.2.82                                          [128] 2.2.83                                            [129] 2.2.84
# [130] 2.2.85                                          [131] 2.2.86                                            [132] 2.2.87
# [133] 2.2.88                                          [134] 2.2.89                                            [135] 2.2.90
# [136] 2.2.91                                          [137] 2.2.92                                            [138] 2.2.93
# [139] 2.2.94                                          [140] 2.2.95                                            [141] 2.2.96
# [142] 2.2.97                                          [143] 2.2.98                                            [144] 2.2.99
# [145] 2.2.100                                         [146] 2.2.101                                           [147] 2.2.102
# [148] 2.2.103                                         [149] 2.2.103.1.a                                       [150] 2.2.104
# [151] 2.2.105                                         [152] 2.2.106.1                                         [153] 2.2.106.2
# [154] 2.2.106.3                                       [155] 2.2.106.3.a                                       [156] 2.2.107
# [157] 2.2.108.1                                       [158] 2.2.108.2                                         [159] 2.2.108.3
# [160] 2.2.108.3.a                                     [161] 2.2.109                                           [162] 2.2.110
# [163] 2.2.111                                         [164] 2.2.112                                           [165] 2.2.113
# [166] 2.2.114                                         [167] 2.2.115                                           [168] 2.2.116
# [169] 2.2.117                                         [170] 2.2.118                                           [171] 2.2.119
# [172] ID_SGC2                                         [173] CODPESSOA3                                        [174] NOME
# [175] CPF                                             [176] VINCULO                                           [177] PES_Pesca
# [178] PES_Pesca_ProfissionalReg                       [179] PES_Pesca_NaoRegulamentada                        [180] PES_Pesca_Subsistencia
# [181] PES_Pesca_Camaroeiro                            [182] PES_Pesca_Marisqueiro                             [183] PES_Pesca_Caranguejeiro
# [184] PES_Pesca_Mergulhador                           [185] PES_Agropecuaria                                  [186] PES_AgroPecuaria_Minifundio
# [187] PES_AgroPecuaria_PequenoPorte                   [188] PES_AgroPecuaria_MedioPorte                       [189] PES_AgroPecuaria_GrandePorte
# [190] PES_AgroPecuaria_Arrendatario                   [191] PES_AgroPecuaria_Meeiro                           [192] PES_AgroPecuaria_QuintalRural
# [193] PES_AgroPecuaria_QuintalUrbano                  [194] PES_ComercioServ                                  [195] PES_ComercioServ_Turismo
# [196] PES_ComercioServ_Alimentacao                    [197] PES_ComercioServ_Ambulante                        [198] PES_ComercioServ_Artesao
# [199] PES_ComercioServ_Aluguel                        [200] PES_ComercioServ_Mergulhador                      [201] PES_ComercioServ_Diverso
# [202] PES_ExtracaoMineral                             [203] PES_ExtracaoMineral_Areeiro                       [204] PES_ExtracaoMineral_Garimpeiro
# [205] PES_ExtracaoMineral_Faiscador                   [206] PES_ExtracaoMineral_Carroceiro                    [207] PES_ExtracaoMineral_Mergulhador
# [208] PES_ExtracaoMineral_Diverso                     [209] PES_Lavadeira                                     [210] PES_Lavadeira_ComRenda
# [211] PES_Lavadeira_SemRenda                          [212] PES_Piscicultura                                  [213] PES_Piscicultura_TanqueRede
# [214] PES_Piscicultura_TanqueEscavado                 [215] PES_IndAgroind                                    [216] PES_IndAgroind_Industria
# [217] PES_IndAgroind_Agroindustria                    [218] PES_TranspFluvial                                 [219] PES_TranspFluvial_Balseiro
# [220] PES_TranspFluvial_Boteiro                       [221] PES_EmpregOcup                                    [222] PES_CadeiaPesca
# [223] PES_CadeiaPesca_Beneficiamento                  [224] PES_CadeiaPesca_Comercializacao                   [225] PES_CadeiaPesca_InsumoServico
# [226] PES_PatrimonioMaterial                          [227] PES_PatrimonioMaterial_Terreno                    [228] PES_PatrimonioMaterial_BenfNaoRep
# [229] PES_PatrimonioMaterial_EletroMobiVest           [230] PES_PatrimonioMaterial_MaquinasEquipamentos       [231] PES_PatrimonioMaterial_Veiculos
# [232] PES_PerdaHumana                                 [233] PES_AnimaisDomesticos                             [234] PES_Documentos
# [235] PES_Acesso                                      [236] PES_Acesso_PropPrivada                            [237] PES_Acesso_EquipPublico
# [238] PES_ExtracaoVegetal


setwd(paste0("C:/Users/",Sys.info()[[7]],"/HERKENHOFF & PRATES/OneDrive - HERKENHOFF & PRATES/ProteçãoSocial/BDs"))
list.files()
pessoas <- openxlsx::read.xlsx("BDPessoas2020.xlsx", cols = c(1:6, 11, 20, 44, 22, 25, 124, 128, 132, 136, 140, 144, 161, 163, 165, 168))
pessoas[pessoas=="[NA]"] <- NA


# Recofica variáveis de renda para numéricas (substitui pontos por vazio e vírgulas por pontos)
pessoas <- pessoas %>% 
        mutate(`2.2.79` = as.double(str_replace_all(`2.2.79`, c("\\." = "", "," = "\\."))),
               `2.2.83` = as.double(str_replace_all(`2.2.83`, c("\\." = "", "," = "\\."))),
               `2.2.87` = as.double(str_replace_all(`2.2.87`, c("\\." = "", "," = "\\."))),
               `2.2.91` = as.double(str_replace_all(`2.2.91`, c("\\." = "", "," = "\\."))),
               `2.2.95` = as.double(str_replace_all(`2.2.95`, c("\\." = "", "," = "\\."))),
               `2.2.99` = as.double(str_replace_all(`2.2.99`, c("\\." = "", "," = "\\."))),
               `2.2.109` = as.double(str_replace_all(`2.2.109`, c("\\." = "", "," = "\\."))),
               `2.2.111` = as.double(str_replace_all(`2.2.111`, c("\\." = "", "," = "\\."))),
               `2.2.113` = as.double(str_replace_all(`2.2.113`, c("\\." = "", "," = "\\."))),
               `2.2.116` = as.double(str_replace_all(`2.2.116`, c("\\." = "", "," = "\\."))))

# Variáveis de data
pessoas$`2.2.10.1.a` <- as.Date(pessoas$`2.2.10.1.a`, tryFormats = "%d/%m/%Y")
pessoas$C3 <- as.Date(pessoas$C3, origin = "1899-12-30")


# Cria variável idade (considerar a data da entrevista (C3) ou uma data mais atual)
# pessoas <- add_column(pessoas, "idade" = as.numeric(""), .after = "2.2.10.1.a")
# pessoas$idade <- trunc(time_length(interval(ymd(pessoas$`2.2.10.1.a`), ymd(pessoas$C3)), "year"))


# Variável de responsável por renda
pessoas <- add_column(pessoas, "ResponsavelAlternativo" = "", .after = "2.2.13")
pessoas <- add_column(pessoas, "RendaTotal" = as.numeric(""), .after = "2.2.32")
pessoas <- add_column(pessoas, "MaiorRenda" = as.numeric(""), .after = "RendaTotal")
pessoas <- add_column(pessoas, "MaisVelho" = as.numeric(""), .after = "MaiorRenda")


pessoas <- pessoas %>% 
        group_by(codpessoa) %>% 
        mutate(RendaTotal = sum(`2.2.79`, `2.2.83`,`2.2.87`, `2.2.91`, `2.2.95`, `2.2.99`, `2.2.116`, na.rm = TRUE)) %>% 
        group_by(ID_SGC) %>% 
        mutate(MaiorRenda = ifelse(RendaTotal == max(RendaTotal) & RendaTotal != 0, 1, 0),
               MaisVelho = ifelse(`2.2.9` == max(`2.2.9`), 1, 0)) %>% 
        group_by(ID_SGC) %>% 
        mutate(ResponsavelAlternativo = ifelse(`2.2.9` < 18, 0,
                                       ifelse(MaiorRenda != 1, 0,
                                              ifelse(MaiorRenda == 1 & MaisVelho == 1, 1, 0))))


idsConflito <- pessoas %>% summarise(Total = n(),
                    Responsáveis = sum(ResponsavelAlternativo)) %>% 
        arrange(desc(Responsáveis)) %>% 
        filter(Responsáveis > 1)


# Cria base de famílias com as variáveis
familias <- 
     pessoas %>% 
        mutate(idade = `2.2.9`,
               crianca = ifelse(idade < 12, 1, 0),
               adolescente = ifelse(idade %in% c(12:17), 1, 0),
               adulto = ifelse(idade %in% c(18:59), 1,0),
               idoso = ifelse(idade >= 60, 1, 0),
               jovem = ifelse(idade %in% c(15:29), 1, 0),
               pessoaDeficiencia = ifelse(`2.2.32` == "Sim", 1, 0),
               SM = ifelse(C3 < "2016-01-01", 788,
                           ifelse(C3 >= "2016-01-01" &  C3 < "2017-01-01", 880,
                                  ifelse(C3 >= "2017-01-01" & C3 < "2018-01-01", 937,
                                         ifelse(C3 >= "2018-01-01" & C3 < "2019-01-01", 954, 998)))),
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
openxlsx::write.xlsx(list(familias = familias,
                          pessoas = pessoas,
                          idsConflito = idsConflito), "ProtecaoSocialBaseCalculoV4.xlsx")



