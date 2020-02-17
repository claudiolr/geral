library(readxl)
library(stringr)
library(reshape2)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(tibble)


setwd("C:/Users/Claudio/OneDrive/CienciaDados/R/ValeTetraTech/FiltrosR")

# Filtro 3: identificação -------------------------------------------------
Filtro3 <- read_excel("Filtro_3.xlsx")
colnames(Filtro3) = c("idSincronismo",
                      "id",
                      "data",
                      "cidade",
                      "pontoEncontro",
                      "passivelCadastro",
                      "motivoNaoCadastro",
                      "usosPropriedade",
                      "atividadeEconomicaTipos",
                      "entidadesTipos",
                      "capacidadeMaxima",
                      "coordenadasCompletas",
                      "latitude",
                      "longitude",
                      "quantidadeAnimais")

Filtro3[c("cidade","coordenadasCompletas","latitude","longitude","quantidadeAnimais")] <- NULL

Filtro3$data <- as.Date(Filtro3$data, format = "%d/%m/%Y")
Filtro3$data  <- format(Filtro3$data, "%d/%m/%Y")

Filtro3$pontoEncontro[Filtro3$pontoEncontro=="0" | Filtro3$pontoEncontro=="00" | Filtro3$pontoEncontro=="000"] <- NA


Filtro3$passivelCadastro <- ifelse(Filtro3$passivelCadastro == "0", "Não",
                                    ifelse(Filtro3$passivelCadastro == "1", "Sim", NA))
Filtro3$passivelCadastro <- as.factor(Filtro3$passivelCadastro)


Filtro3$motivoNaoCadastro <- ifelse(Filtro3$motivoNaoCadastro == "1", "Atividade econômica sazonal",
                                        ifelse(Filtro3$motivoNaoCadastro == "2", "Ausência temporária de longa duração",
                                               ifelse(Filtro3$motivoNaoCadastro == "3", "Edificação desocupada",
                                                      ifelse(Filtro3$motivoNaoCadastro == "4", "Extensão de instalação",
                                                             ifelse(Filtro3$motivoNaoCadastro == "5", "Imóvel em construção",
                                                                    ifelse(Filtro3$motivoNaoCadastro == "6", "Lote vago",
                                                                           ifelse(Filtro3$motivoNaoCadastro == "7", "Inexistente/ demolido",
                                                                                  ifelse(Filtro3$motivoNaoCadastro == "66", "Outro", NA))))))))
Filtro3$motivoNaoCadastro <- as.factor(Filtro3$motivoNaoCadastro)


Filtro3$motivoNaoCadastro[Filtro3$passivelCadastro=="Sim"] <- NA
Filtro3$usosPropriedade[Filtro3$passivelCadastro=="Não"] <- NA
Filtro3$atividadeEconomicaTipos[Filtro3$passivelCadastro=="Não"] <- NA
Filtro3$entidadesTipos[Filtro3$passivelCadastro=="Não"] <- NA
Filtro3$capacidadeMaxima[Filtro3$passivelCadastro=="Não"] <- NA

Filtro3 <- add_column(Filtro3, "residencia" = "", .after = "usosPropriedade")
Filtro3 <- mutate(Filtro3, "residencia" = ifelse(grepl("1;", usosPropriedade), "Sim",
                                                  ifelse(Filtro3$usosPropriedade == "", NA, "Não")))

Filtro3 <- add_column(Filtro3, "atividadeEconomica"="", .after = "residencia")
Filtro3 <- add_column(Filtro3, "industria"="", .after = "atividadeEconomicaTipos")
Filtro3 <- add_column(Filtro3, "comercio"="", .after = "industria")
Filtro3 <- add_column(Filtro3, "servico"="", .after = "comercio")
Filtro3 <- add_column(Filtro3, "agropecuaria"="", .after = "servico")
Filtro3 <- add_column(Filtro3, "extrativismo"="", .after = "agropecuaria")

Filtro3 <- mutate(Filtro3, "atividadeEconomica" = ifelse(grepl("2", usosPropriedade), "Sim",
                                                         ifelse(Filtro3$usosPropriedade == "", NA, "Não")))
Filtro3 <- mutate(Filtro3, "industria" = ifelse(grepl("1", atividadeEconomicaTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "comercio" = ifelse(grepl("2", atividadeEconomicaTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "servico" = ifelse(grepl("3", atividadeEconomicaTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "agropecuaria" = ifelse(grepl("4", atividadeEconomicaTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "extrativismo" = ifelse(grepl("5", atividadeEconomicaTipos), "Sim", NA))

Filtro3 <- add_column(Filtro3, "entidade" = "", .after = "extrativismo")
Filtro3 <- mutate(Filtro3, "entidade" = ifelse(grepl("3", usosPropriedade), "Sim",
                                               ifelse(Filtro3$usosPropriedade == "", NA, "Não")))

Filtro3 <- add_column(Filtro3, "temploReligioso"="", .after = "entidadesTipos")
Filtro3 <- add_column(Filtro3, "ong"="", .after = "temploReligioso")
Filtro3 <- add_column(Filtro3, "instEducacional"="", .after = "ong")
Filtro3 <- add_column(Filtro3, "instSaude"="", .after = "instEducacional")
Filtro3 <- add_column(Filtro3, "instPublica"="", .after = "instSaude")
Filtro3 <- add_column(Filtro3, "autarquia"="", .after = "instPublica")
Filtro3 <- add_column(Filtro3, "associacao"="", .after = "autarquia")
Filtro3 <- add_column(Filtro3, "entidadesGeral"="", .after = "associacao")

Filtro3 <- mutate(Filtro3, "temploReligioso" = ifelse(grepl("1",entidadesTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "ong" = ifelse(grepl("2",entidadesTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "instEducacional" = ifelse(grepl("3",entidadesTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "instSaude" = ifelse(grepl("4",entidadesTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "instPublica" = ifelse(grepl("5",entidadesTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "autarquia" = ifelse(grepl("6",entidadesTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "associacao" = ifelse(grepl("7",entidadesTipos), "Sim", NA))
Filtro3 <- mutate(Filtro3, "entidadesGeral" = ifelse(grepl("8",entidadesTipos), "Sim", NA))

Filtro3$usosPropriedade <- str_replace_all(Filtro3$usosPropriedade, c("1"="Residência", "2"="Atividade econômica", "3"="Entidade"))
Filtro3$atividadeEconomicaTipos <- str_replace_all(Filtro3$atividadeEconomicaTipos, c("1"="Indústria",
                                                                                      "2"="Comércio",
                                                                                      "3"="Serviço",
                                                                                      "4"="Agropecuária",
                                                                                      "5"="Extrativismo"))
Filtro3$entidadesTipos <- str_replace_all(Filtro3$entidadesTipos, c("1"="Templo religioso",
                                                                    "2"="Organização não-governamental",
                                                                    "3"="Instituição educacional",
                                                                    "4"="Hospital/Unidade de Saúde",
                                                                    "5"="Instituição Pública",
                                                                    "6"="Autarquia",
                                                                    "7"="Associação",
                                                                    "8"="Entidades em geral"))

Filtro3$capacidadeMaxima[Filtro3$capacidadeMaxima==0] <- "Não sabe/ não respondeu"

#Filtro 4: Ocorrências
Filtro4 <- read_excel("Filtro_4.xlsx")
colnames(Filtro4) = c("idSincronismo",
                      "id",
                      "data",
                      "dataSincronismo",
                      "cidade",
                      "possivelEntrevista",
                      "numeroTentativa",
                      "tipoOcorrencia",
                      "ocorrenciaOutro",
                      "incapacidadeResponder")
Filtro4$ocorrenciaOutro <- toupper(Filtro4$ocorrenciaOutro)
Filtro4$ocorrenciaOutro[Filtro4$ocorrenciaOutro == "000"] <- NA

Filtro4[c("idSincronismo","data","cidade")] <- NULL

Filtro4$dataSincronismo <- as.Date(Filtro4$dataSincronismo)
Filtro4$possivelEntrevista <- ifelse(Filtro4$possivelEntrevista=="1", "Sim",
                                      ifelse(Filtro4$possivelEntrevista=="0", "Não", NA))
Filtro4$numeroTentativa <- as.numeric(Filtro4$numeroTentativa)
Filtro4$tipoOcorrencia <- ifelse(Filtro4$tipoOcorrencia=="1", "Morador ou funcionário não estava presente",
                                      ifelse(Filtro4$tipoOcorrencia=="2", "Morador ou funcionário recusou responder",
                                              ifelse(Filtro4$tipoOcorrencia=="3", "Morador ou funcionário incapacitado para responder",
                                                      ifelse(Filtro4$tipoOcorrencia=="4", "Outro", NA))))
Filtro4$tipoOcorrencia[Filtro4$possivelEntrevista=="Sim"] <- NA
Filtro4$ocorrenciaOutro[Filtro4$possivelEntrevista=="Sim"] <- NA
Filtro4$incapacidadeResponder[Filtro4$possivelEntrevista=="Sim"] <- NA

#Filtro 5
Filtro5 <- read_excel("filtro_5.xlsx")
colnames(Filtro5) = c("idSincronismo",
                      "id",
                      "cidade",
                      "moradiaIsolamentoAcustico",
                      "moradiaAtivSinaisSonoros",
                      "moradiaAtivSinaisSonorosQuais",
                      "familiaPossuiTransporte",
                      "familiaTransporte",
                      "abrigoEmergencia",
                      "abrigoEmergenciaOnde",
                      "abrigoEmergenciaTempo")

Filtro5[c("idSincronismo","cidade")] <- NULL

Filtro5$moradiaIsolamentoAcustico <- ifelse(Filtro5$moradiaIsolamentoAcustico == "0", "Não",
                                             ifelse(Filtro5$moradiaIsolamentoAcustico == "1", "Sim", 
                                                     ifelse(Filtro5$moradiaIsolamentoAcustico == "88", "Não sabe",
                                                             ifelse(Filtro5$moradiaIsolamentoAcustico == "99", "Recusou informar", NA))))

Filtro5$moradiaAtivSinaisSonoros <- ifelse(Filtro5$moradiaAtivSinaisSonoros == "0", "Não",
                                            ifelse(Filtro5$moradiaAtivSinaisSonoros == "1", "Sim",
                                                   ifelse(Filtro5$moradiaAtivSinaisSonoros == "88", "Não sabe",
                                                          ifelse(Filtro5$moradiaAtivSinaisSonoros == "99", "Recusou informar", NA))))
Filtro5$moradiaAtivSinaisSonorosQuais[Filtro5$moradiaAtivSinaisSonoros=="Não"] <- NA
Filtro5$moradiaAtivSinaisSonorosQuais <- toupper(Filtro5$moradiaAtivSinaisSonorosQuais)

Filtro5$familiaPossuiTransporte <- ifelse(Filtro5$familiaPossuiTransporte=="0", "Não",
                                               ifelse(Filtro5$familiaPossuiTransporte=="1", "Sim",
                                                       ifelse(Filtro5$familiaPossuiTransporte=="88", "Não sabe",
                                                               ifelse(Filtro5$familiaPossuiTransporte=="99", "Recusou informar", NA))))
Filtro5$familiaTransporte[Filtro5$familiaPossuiTransporte=="Não"] <- NA

Filtro5 <- add_column(Filtro5, "carroPequenoFam"="", .after = "familiaTransporte")
Filtro5 <- add_column(Filtro5, "caminhoneteFam"="", .after = "carroPequenoFam")
Filtro5 <- add_column(Filtro5, "motoFam"="", .after = "caminhoneteFam")
Filtro5 <- add_column(Filtro5, "caminhaoFam"="", .after = "motoFam")
Filtro5 <- add_column(Filtro5, "bicicletaFam"="", .after = "caminhaoFam")
Filtro5 <- add_column(Filtro5, "cavaloCarrocaFam"="", .after = "bicicletaFam")
Filtro5 <- add_column(Filtro5, "barcoFam"="", .after = "cavaloCarrocaFam")
Filtro5 <- add_column(Filtro5, "outroTranspFam"="", .after = "barcoFam")

Filtro5 <- mutate(Filtro5, "outroTranspFam" = ifelse(grepl("66", familiaTransporte), "Sim", NA))
Filtro5$familiaTransporte <- str_replace(Filtro5$familiaTransporte, "66", "")
Filtro5 <- mutate(Filtro5, "carroPequenoFam" = ifelse(grepl("1", familiaTransporte), "Sim", NA))
Filtro5 <- mutate(Filtro5, "caminhoneteFam" = ifelse(grepl("2", familiaTransporte), "Sim", NA))
Filtro5 <- mutate(Filtro5, "motoFam" = ifelse(grepl("3", familiaTransporte), "Sim", NA))
Filtro5 <- mutate(Filtro5, "caminhaoFam" = ifelse(grepl("4", familiaTransporte), "Sim", NA))
Filtro5 <- mutate(Filtro5, "bicicletaFam" = ifelse(grepl("5", familiaTransporte), "Sim", NA))
Filtro5 <- mutate(Filtro5, "cavaloCarrocaFam" = ifelse(grepl("6", familiaTransporte), "Sim", NA))
Filtro5 <- mutate(Filtro5, "barcoFam" = ifelse(grepl("7", familiaTransporte), "Sim", NA))
Filtro5$familiaTransporte <- NULL

Filtro5$abrigoEmergencia <- ifelse(Filtro5$abrigoEmergencia == "0", "Não",
                                   ifelse(Filtro5$abrigoEmergencia == "1", "Sim",
                                          ifelse(Filtro5$abrigoEmergencia == "88", "Não sabe",
                                                 ifelse(Filtro5$abrigoEmergencia == "99", "Recusou informar", NA))))
Filtro5$abrigoEmergenciaOnde[Filtro5$abrigoEmergencia=="Não"] <- NA
Filtro5$abrigoEmergenciaTempo[Filtro5$abrigoEmergencia=="Não"] <- NA

Filtro5 <- add_column(Filtro5, "abrigoOutroImovel"="", .after = "abrigoEmergencia")
Filtro5 <- add_column(Filtro5, "abrigoAmigos"="", .after = "abrigoOutroImovel")
Filtro5 <- add_column(Filtro5, "abrigoParentes"="", .after = "abrigoAmigos")

Filtro5 <- mutate(Filtro5, "abrigoOutroImovel" = ifelse(grepl("1", abrigoEmergenciaOnde), "Sim", NA))
Filtro5 <- mutate(Filtro5, "abrigoAmigos" = ifelse(grepl("2", abrigoEmergenciaOnde), "Sim", NA))
Filtro5 <- mutate(Filtro5, "abrigoParentes" = ifelse(grepl("3", abrigoEmergenciaOnde), "Sim", NA))
Filtro5$abrigoEmergenciaOnde <- NULL

Filtro5$abrigoEmergenciaTempo <- ifelse(Filtro5$abrigoEmergenciaTempo=="1", "Por poucos dias",
                                             ifelse(Filtro5$abrigoEmergenciaTempo=="2", "Por até uma semana",
                                                     ifelse(Filtro5$abrigoEmergenciaTempo=="3","Por até um mês",
                                                             ifelse(Filtro5$abrigoEmergenciaTempo=="4", "Por mais de um mês",
                                                                     ifelse(Filtro5$abrigoEmergenciaTempo=="5", "Indeterminado",
                                                                             ifelse(Filtro5$abrigoEmergenciaTempo=="88", "Não sabe/ não respondeu", NA))))))


#Filtro 6
Filtro6 <- read_excel("Filtro_6.xlsx")

colnames(Filtro6) = c("idSincronismo",
                      "id",
                      "cidade",
                      "nomeEmpresa",
                      "informaTelEmpresa",
                      "telEmpresa",
                      "atividadeEmpresa",
                      "informaCNPJ",
                      "cnpj",
                      "diasFunciona",
                      "horaFuncionaSegundaEmpresa",
                      "horaFuncionaTercaEmpresa",
                      "horaFuncionaQuartaEmpresa",
                      "horaFuncionaQuintaEmpresa",
                      "horaFuncionaSextaEmpresa",
                      "horaFuncionaSabadoEmpresa",
                      "horaFuncionaDomingoEmpresa",
                      "pessoasAtendidas",
                      "pessoasAtendidasManha",
                      "pessoasAtendidasTarde",
                      "pessoasAtendidasNoite",
                      "pessoasAtendidasMadrugada",
                      "pessoasAtendidasDeficiencia",
                      "possuiProdQuimRadioativo",
                      "volumeProdQuimRadioativo",
                      "armazenamentoProdQuimRadioativo",
                      "acessoEdificacao",
                      "possuiSaidaEmerg",
                      "numSaidasEmerg",
                      "possuiCIPA",
                      "sabeVencCIPA",
                      "mesVencCIPA",
                      "possuiBrigIncendio",
                      "isolamentoAcustico",
                      "ativSinaisSonoros",
                      "ativSinaisSonorosQuais",
                      "ativSinaisSonorosQuaisOutros",
                      "contatoEmerg",
                      "contatoEmergTel")

Filtro6[c("idSincronismo","cidade")] <-NULL

Filtro6 <- add_column(Filtro6, "anoVencCIPA"="", .after = "sabeVencCIPA")

Filtro6$nomeEmpresa <- toupper(Filtro6$nomeEmpresa)

Filtro6$atividadeEmpresa <- toupper(Filtro6$atividadeEmpresa)

Filtro6$informaTelEmpresa <- ifelse(Filtro6$informaTelEmpresa == "0", "Não",
                                         ifelse(Filtro6$informaTelEmpresa == "1", "Sim", NA))

Filtro6$informaTelEmpresa[Filtro6$telEmpresa=="(00) 000000000" |
                               Filtro6$telEmpresa=="(00) 00000000" |
                               Filtro6$telEmpresa=="(00) 999999999" |
                               Filtro6$telEmpresa=="(00) 99999999" |
                               Filtro6$telEmpresa=="(99) 999999999" |
                               Filtro6$telEmpresa=="(99) 99999999" |
                               Filtro6$telEmpresa=="(31) 000000000" |
                               Filtro6$telEmpresa=="(31) 00000000" |
                               Filtro6$telEmpresa=="(31) 999999999" |
                               Filtro6$telEmpresa=="(31) 99999999" |
                               Filtro6$telEmpresa=="(94) 000000000" |
                               Filtro6$telEmpresa=="(94) 00000000" |
                               Filtro6$telEmpresa=="(94) 999999999" |
                               Filtro6$telEmpresa=="(94) 99999999"] <- "Não"

Filtro6$telEmpresa[Filtro6$informaTelEmpresa=="Não"] <- NA

Filtro6$informaCNPJ <- ifelse(Filtro6$informaCNPJ=="0", "Não",
                                   ifelse(Filtro6$informaCNPJ=="1", "Sim",
                                           ifelse(Filtro6$informaCNPJ=="77","Não possui CNPJ",
                                                   ifelse(Filtro6$informaCNPJ=="88", "Não sabe",
                                                           ifelse(Filtro6$informaCNPJ=="99", "Recusou informar CNPJ", NA)))))

Filtro6$cnpj[Filtro6$informaCNPJ=="Não" | Filtro6$informaCNPJ=="Não possui CNPJ" | Filtro6$informaCNPJ=="Recusou informar CNPJ"] <- NA

Filtro6 <- add_column(Filtro6, "funcionaSegundaEmpresa"="", .after = "diasFunciona")
Filtro6 <- add_column(Filtro6, "funcionaTercaEmpresa"="", .after = "funcionaSegundaEmpresa")
Filtro6 <- add_column(Filtro6, "funcionaQuartaEmpresa"="", .after = "funcionaTercaEmpresa")
Filtro6 <- add_column(Filtro6, "funcionaQuintaEmpresa"="", .after = "funcionaQuartaEmpresa")
Filtro6 <- add_column(Filtro6, "funcionaSextaEmpresa"="", .after = "funcionaQuintaEmpresa")
Filtro6 <- add_column(Filtro6, "funcionaSabadoEmpresa"="", .after = "funcionaSextaEmpresa")
Filtro6 <- add_column(Filtro6, "funcionaDomingoEmpresa"="", .after = "funcionaSabadoEmpresa")

                      Filtro6 <- mutate(Filtro6, "funcionaSegundaEmpresa" = ifelse(grepl("1", diasFunciona), "Sim", NA))
                      Filtro6 <- mutate(Filtro6, "funcionaTercaEmpresa" = ifelse(grepl("2", diasFunciona), "Sim", NA))
                      Filtro6 <- mutate(Filtro6, "funcionaQuartaEmpresa" = ifelse(grepl("3", diasFunciona), "Sim", NA))
                      Filtro6 <- mutate(Filtro6, "funcionaQuintaEmpresa" = ifelse(grepl("4", diasFunciona), "Sim", NA))
                      Filtro6 <- mutate(Filtro6, "funcionaSextaEmpresa" = ifelse(grepl("5", diasFunciona), "Sim", NA))
                      Filtro6 <- mutate(Filtro6, "funcionaSabadoEmpresa" = ifelse(grepl("6", diasFunciona), "Sim", NA))
                      Filtro6 <- mutate(Filtro6, "funcionaDomingoEmpresa" = ifelse(grepl("7", diasFunciona), "Sim", NA))

Filtro6$diasFunciona <- NULL

Filtro6$possuiProdQuimRadioativo <- ifelse(Filtro6$possuiProdQuimRadioativo=="0", "Não",
                                                ifelse(Filtro6$possuiProdQuimRadioativo=="1", "Sim",
                                                        ifelse(Filtro6$possuiProdQuimRadioativo=="88", "Não sabe",
                                                                ifelse(Filtro6$possuiProdQuimRadioativo=="99", "Recusou responder", NA))))

Filtro6$volumeProdQuimRadioativo[Filtro6$possuiProdQuimRadioativo=="Não" |
                                      Filtro6$possuiProdQuimRadioativo=="Não sabe" |
                                      Filtro6$possuiProdQuimRadioativo=="Recusou responder"] <- NA

Filtro6$volumeProdQuimRadioativo <- toupper(Filtro6$volumeProdQuimRadioativo)

Filtro6$armazenamentoProdQuimRadioativo[Filtro6$possuiProdQuimRadioativo=="Não" |
                                      Filtro6$possuiProdQuimRadioativo=="Não sabe" |
                                      Filtro6$possuiProdQuimRadioativo=="Recusou responder"] <- NA

Filtro6$armazenamentoProdQuimRadioativo <- toupper(Filtro6$armazenamentoProdQuimRadioativo)

Filtro6 <- add_column(Filtro6, "rampasEmpresa"="", .after = "armazenamentoProdQuimRadioativo")
Filtro6 <- add_column(Filtro6, "escadasEmpresa"="", .after = "rampasEmpresa")
Filtro6 <- add_column(Filtro6, "elevadoresEmpresa"="", .after = "escadasEmpresa")
Filtro6 <- add_column(Filtro6, "edificNivelRuaEmpresa"="", .after = "elevadoresEmpresa")
Filtro6 <- add_column(Filtro6, "outroAcessoEmpresa"="", .after = "edificNivelRuaEmpresa")
              Filtro6 <- mutate(Filtro6, "rampasEmpresa" = ifelse(grepl("1", acessoEdificacao), "Sim", NA))
              Filtro6 <- mutate(Filtro6, "escadasEmpresa" = ifelse(grepl("2", acessoEdificacao), "Sim", NA))
              Filtro6 <- mutate(Filtro6, "elevadoresEmpresa" = ifelse(grepl("3", acessoEdificacao), "Sim", NA))
              Filtro6 <- mutate(Filtro6, "edificNivelRuaEmpresa" = ifelse(grepl("4", acessoEdificacao), "Sim", NA))
              Filtro6 <- mutate(Filtro6, "outroAcessoEmpresa" = ifelse(grepl("66", acessoEdificacao), "Sim", NA))

Filtro6$acessoEdificacao <- NULL

Filtro6$possuiCIPA <- ifelse(Filtro6$possuiCIPA == "0", "Não",
                                  ifelse(Filtro6$possuiCIPA == "1", "Sim",
                                          ifelse(Filtro6$possuiCIPA == "88", "Não sabe",
                                                  ifelse(Filtro6$possuiCIPA == "99", "Recusou responder", NA))))

Filtro6$sabeVencCIPA <- ifelse(Filtro6$sabeVencCIPA=="0", "Não",
                                    ifelse(Filtro6$sabeVencCIPA=="1", "Sim", NA))

Filtro6$mesVencCIPA[Filtro6$sabeVencCIPA=="Não"] <- NA
Filtro6$mesVencCIPA[Filtro6$sabeVencCIPA=="Sim"] <- "Não sabe"
Filtro6$anoVencCIPA[Filtro6$sabeVencCIPA=="Não"] <- NA
Filtro6$anoVencCIPA[Filtro6$sabeVencCIPA=="Sim"] <- "Não sabe"

Filtro6$possuiSaidaEmerg <- ifelse(Filtro6$possuiSaidaEmerg=="0", "Não",
                                    ifelse(Filtro6$possuiSaidaEmerg=="1", "Sim", NA))

Filtro6$numSaidasEmerg[Filtro6$possuiSaidaEmerg=="Não"] <- NA

Filtro6$possuiBrigIncendio <- ifelse(Filtro6$possuiBrigIncendio=="0", "Não",
                                          ifelse(Filtro6$possuiBrigIncendio=="1", "Sim",
                                                  ifelse(Filtro6$possuiBrigIncendio=="88", "Não sabe", "Não sabe")))

Filtro6$isolamentoAcustico <- ifelse(Filtro6$isolamentoAcustico == "0", "Não",
                                             ifelse(Filtro6$isolamentoAcustico == "1", "Sim",
                                                     ifelse(Filtro6$isolamentoAcustico == "88", "Não sabe",
                                                             ifelse(Filtro6$isolamentoAcustico == "99", "Recusou informar", "Não sabe"))))

Filtro6$ativSinaisSonoros <- ifelse(Filtro6$ativSinaisSonoros == "0", "Não",
                                    ifelse(Filtro6$ativSinaisSonoros == "1", "Sim",
                                           ifelse(Filtro6$ativSinaisSonoros == "88", "Não sabe",
                                                  ifelse(Filtro6$ativSinaisSonoros == "99", "Recusou informar", "Não sabe"))))

Filtro6$ativSinaisSonorosQuais[Filtro6$ativSinaisSonoros!="Sim"] <- NA

Filtro6$ativSinaisSonorosQuaisOutros[Filtro6$ativSinaisSonoros!="Sim"] <- NA


Filtro6 <- add_column(Filtro6, "consultOdontologicoEmpresa"="", .after = "ativSinaisSonoros")
Filtro6 <- add_column(Filtro6, "marcenariaEmpresa"="", .after = "consultOdontologicoEmpresa")
Filtro6 <- add_column(Filtro6, "constrCivilEmpresa"="", .after = "marcenariaEmpresa")
Filtro6 <- add_column(Filtro6, "serralheriaEmpresa"="", .after = "constrCivilEmpresa")
Filtro6 <- add_column(Filtro6, "estudioMusicalEmpresa"="", .after = "serralheriaEmpresa")
Filtro6 <- add_column(Filtro6, "outrasAtivSonorasEmpresa"="", .after = "estudioMusicalEmpresa")

Filtro6 <- mutate(Filtro6, "consultOdontologicoEmpresa" = ifelse(grepl("1", ativSinaisSonorosQuais), "Sim", NA))
Filtro6 <- mutate(Filtro6, "marcenariaEmpresa" = ifelse(grepl("2", ativSinaisSonorosQuais), "Sim", NA))
Filtro6 <- mutate(Filtro6, "constrCivilEmpresa" = ifelse(grepl("3", ativSinaisSonorosQuais), "Sim", NA))
Filtro6 <- mutate(Filtro6, "serralheriaEmpresa" = ifelse(grepl("4", ativSinaisSonorosQuais), "Sim", NA))
Filtro6 <- mutate(Filtro6, "estudioMusicalEmpresa" = ifelse(grepl("5", ativSinaisSonorosQuais), "Sim", NA))
Filtro6 <- mutate(Filtro6, "outrasAtivSonorasEmpresa" = ifelse(grepl("6", ativSinaisSonorosQuais), "Sim", NA))

Filtro6$ativSinaisSonorosQuaisOutros <- toupper(Filtro6$ativSinaisSonorosQuaisOutros)

Filtro6$ativSinaisSonorosQuais <- NULL

Filtro6$contatoEmerg <- toupper(Filtro6$contatoEmerg)
Filtro6$contatoEmergTel[Filtro6$contatoEmergTel=="(00) 000000000" |
                               Filtro6$contatoEmergTel=="(00) 00000000" |
                               Filtro6$contatoEmergTel=="(00) 999999999" |
                               Filtro6$contatoEmergTel=="(00) 99999999" |
                               Filtro6$contatoEmergTel=="(99) 999999999" |
                               Filtro6$contatoEmergTel=="(99) 99999999" |
                               Filtro6$contatoEmergTel=="(31) 000000000" |
                               Filtro6$contatoEmergTel=="(31) 00000000" |
                               Filtro6$contatoEmergTel=="(31) 999999999" |
                               Filtro6$contatoEmergTel=="(31) 99999999" |
                               Filtro6$contatoEmergTel=="(94) 000000000" |
                               Filtro6$contatoEmergTel=="(94) 00000000" |
                               Filtro6$contatoEmergTel=="(94) 999999999" |
                               Filtro6$contatoEmergTel=="(94) 99999999"] <- NA


#Filtro 7
Filtro7 <- read_excel("Filtro_7.xlsx")
colnames(Filtro7) = c("idSincronismo",
                      "id",
                      "cidade",
                      "logradouro",
                      "numero",
                      "complemento",
                      "bairroDistrito",
                      "localidade",
                      "observacoes",
                      "coordenadasCompletas",
                      "latitude",
                      "longitude",
                      "quantidadeAnimais")

Filtro7[c("idSincronismo","latitude","longitude","quantidadeAnimais","coordenadasCompletas")] <- NULL

Filtro7$cidade <- ifelse(Filtro7$cidade=="1", "Canaã dos Carajás",
                             ifelse(Filtro7$cidade=="2", "Parauapebas",
                                    ifelse(Filtro7$cidade=="3","Marabá",
                                           ifelse(Filtro7$cidade=="4","Barão de Cocais",
                                                  ifelse(Filtro7$cidade=="5","São Gonçalo do Rio Abaixo",
                                                         ifelse(Filtro7$cidade=="6","Belo Vale",
                                                                ifelse(Filtro7$cidade=="7","Catas Altas",
                                                                       ifelse(Filtro7$cidade=="8","Santa Bárbara",
                                                                              ifelse(Filtro7$cidade=="9","Ouro Preto",
                                                                                     ifelse(Filtro7$cidade=="10", "Itabirito",
                                                                                            ifelse(Filtro7$cidade=="11", "Rio Piracicaba", "")))))))))))
Filtro7$numero <- as.numeric(Filtro7$numero)

Filtro7$logradouro[Filtro7$logradouro=="0" | Filtro7$logradouro=="00" | Filtro7$logradouro=="000"] <- "(RUA SEM NOME)"
Filtro7$numero[Filtro7$numero=="000" | Filtro7$numero=="00" | Filtro7$numero=="0"] <- ""
Filtro7$numero <- as.numeric(Filtro7$numero)
Filtro7$complemento[Filtro7$complemento=="000" | Filtro7$complemento=="00" | Filtro7$complemento=="0"] <- ""
Filtro7$bairroDistrito[Filtro7$bairroDistrito=="000" | Filtro7$bairroDistrito=="00" | Filtro7$bairroDistrito=="0"] <- ""
Filtro7$localidade[Filtro7$localidade=="000" | Filtro7$localidade=="00" | Filtro7$localidade=="0"] <- ""
Filtro7$observacoes[Filtro7$observacoes=="000" | Filtro7$observacoes=="00" | Filtro7$observacoes=="0"] <- ""

Filtro7$logradouro <- toupper(Filtro7$logradouro)
Filtro7$bairroDistrito <- toupper(Filtro7$bairroDistrito)
Filtro7$localidade <- toupper(Filtro7$localidade)
Filtro7$observacoes <- toupper(Filtro7$observacoes)

#Filtro 8: animais
Filtro8 <- read_excel("Filtro_8.xlsx")
colnames(Filtro8) = c("idSincronismo",
                      "id",
                      "data",
                      "dataSincronismo",
                      "especieAnimal",
                      "nomeAnimal",
                      "porteAnimal",
                      "racaAnimal",
                      "corAnimal",
                      "sexoAnimal",
                      "donoAnimal")
Filtro8[c("idSincronismo","data","dataSincronismo")] <- NULL

Filtro8$especieAnimal <- if_else(Filtro8$especieAnimal == "1", "Cachorro",
                                 if_else(Filtro8$especieAnimal == "2", "Gato",
                                         if_else(Filtro8$especieAnimal == "3", "Cavalo",
                                                 if_else(Filtro8$especieAnimal == "4", "Mula",
                                                         if_else(Filtro8$especieAnimal == "5", "Burro",
                                                                 if_else(Filtro8$especieAnimal == "6", "Porco",
                                                                         if_else(Filtro8$especieAnimal == "7", "Boi/vaca",
                                                                                 if_else(Filtro8$especieAnimal == "8", "Coelho",
                                                                                         if_else(Filtro8$especieAnimal == "9", "Porquinho da índia",
                                                                                                 if_else(Filtro8$especieAnimal == "10", "Galinha",
                                                                                                         if_else(Filtro8$especieAnimal == "11", "Pato",
                                                                                                                 if_else(Filtro8$especieAnimal == "12", "Pássaro",
                                                                                                                         if_else(Filtro8$especieAnimal == "13", "Peixe",
                                                                                                                                 if_else(Filtro8$especieAnimal == "14", "Paca",
                                                                                                                                         if_else(Filtro8$especieAnimal == "15", "Catitu",
                                                                                                                                                 if_else(Filtro8$especieAnimal == "16", "Cotia",
                                                                                                                                                         if_else(Filtro8$especieAnimal == "17", "Jabuti",
                                                                                                                                                                 if_else(Filtro8$especieAnimal == "18", "Outra ave", "Outro animal", missing = NULL))))))))))))))))))

Filtro8$nomeAnimal <- toupper(Filtro8$nomeAnimal)

Filtro8$porteAnimal <- ifelse(Filtro8$porteAnimal=="1", "Pequeno",
                               ifelse(Filtro8$porteAnimal=="2", "Médio",
                                       ifelse(Filtro8$porteAnimal=="3", "Grande", "Não sabe/não respondeu")))

Filtro8$racaAnimal <- toupper(Filtro8$racaAnimal)

Filtro8$sexoAnimal <- ifelse(Filtro8$sexoAnimal=="1", "Macho",
                              ifelse(Filtro8$sexoAnimal == "2", "Fêmea",
                                      ifelse(Filtro8$sexoAnimal == "88", "Não sabe/ não respondeu", NA)))


#Filtro 10
Filtro10 <- read_excel("filtro_10.xlsx")
colnames(Filtro10) = c("idSincronismo",
                       "id",
                       "data",
                       "cidade",
                       "entidadesTipos",
                       "nomeInstituicao",
                       "informaTelInstituicao",
                       "telInstituicao",
                       "atividadeInstituicao",
                       "informaCnpjInstituicao",
                       "cnpjInstituicao",
                       "diasFuncionaInstituicao",
                       "horaFuncionaInstituicaoSegunda",
                       "horaFuncionaInstituicaoTerca",
                       "horaFuncionaInstituicaoQuarta",
                       "horaFuncionaInstituicaoQuinta",
                       "horaFuncionaInstituicaoSexta",
                       "horaFuncionaInstituicaoSabado",
                       "horaFuncionaInstituicaoDomingo",
                       "alunosFreqEscolaManha",
                       "alunosFreqEscolaTarde",
                       "alunosFreqEscolaNoite",
                       "alunosDeficiencia",
                       "alunosDeficienciaQuais",
                       "alunosFreqInstituicaoDeficienciaFisicaManha",
                       "alunosFreqInstituicaoDeficienciaFisicaTarde",
                       "alunosFreqInstituicaoDeficienciaFisicaNoite",
                       "alunosFreqInstituicaoDeficienciaAuditivaManha",
                       "alunosFreqInstituicaoDeficienciaAuditivaTarde",
                       "alunosFreqInstituicaoDeficienciaAuditivaNoite",
                       "alunosFreqInstituicaoDeficienciaVisualManha",
                       "alunosFreqInstituicaoDeficienciaVisualTarde",
                       "alunosFreqInstituicaoDeficienciaVisualNoite",
                       "alunosFreqInstituicaoDeficienciaMentalManha",
                       "alunosFreqInstituicaoDeficienciaMentalTarde",
                       "alunosFreqInstituicaoDeficienciaMentalNoite",
                       "alunosFreqInstituicaoDeficienciaFalaManha",
                       "alunosFreqInstituicaoDeficienciaFalaTarde",
                       "alunosFreqInstituicaoDeficienciaFalaNoite",
                       "alunosFreqInstituicaoDificuldadeLocomocao",
                       "alunosFreqInstituicaoDificuldadeLocomocaoTipo",
                       "alunosFreqInstituicaoDificuldadeCaminharManha",
                       "alunosFreqInstituicaoDificuldadeCaminharTarde",
                       "alunosFreqInstituicaoDificuldadeCaminharNoite",
                       "alunosFreqInstituicaoAcamadasManha",
                       "alunosFreqInstituicaoAcamadasTarde",
                       "alunosFreqInstituicaoAcamadasNoite",
                       "alunosFreqInstituicaoCadeirantesManha",
                       "alunosFreqInstituicaoCadeirantesTarde",
                       "alunosFreqInstituicaoCadeirantesNoite",
                       "alunosFreqInstituicaoBengalaManha",
                       "alunosFreqInstituicaoBengalaTarde",
                       "alunosFreqInstituicaoBengalaNoite",
                       "alunosFreqInstituicaoMembroAmputadoManha",
                       "alunosFreqInstituicaoMembroAmputadoTarde",
                       "alunosFreqInstituicaoMembroAmputadoNoite",
                       "alunosFreqInstituicaoProblRespitarioLocomManha",
                       "alunosFreqInstituicaoProblRespitarioLocomTarde",
                       "alunosFreqInstituicaoProblRespitarioLocomNoite",
                       "alunosFreqInstituicaoPosOperatorioManha",
                       "alunosFreqInstituicaoPosOperatorioTarde",
                       "alunosFreqInstituicaoPosOperatorioNoite",
                       "alunosFreqInstituicaoProteseManha",
                       "alunosFreqInstituicaoProteseTarde",
                       "alunosFreqInstituicaoProteseNoite",
                       "alunosFreqInstituicaoProblColunaColomManha",
                       "alunosFreqInstituicaoProblColunaColomTarde",
                       "alunosFreqInstituicaoProblColunaColomNoite",
                       "alunosFreqInstituicaoObesasManha",
                       "alunosFreqInstituicaoObesasTarde",
                       "alunosFreqInstituicaoObesasNoite",
                       "alunosFreqInstituicaoSeqAcidenteManha",
                       "alunosFreqInstituicaoSeqAcidenteTarde",
                       "alunosFreqInstituicaoSeqAcidenteNoite",
                       "alunosFreqInstituicaoIdadeAvancadaManha",
                       "alunosFreqInstituicaoIdadeAvancadaTarde",
                       "alunosFreqInstituicaoIdadeAvancadaNoite",
                       "alunosFreqInstituicaoCriancaColoManha",
                       "alunosFreqInstituicaoCriancaColoTarde",
                       "alunosFreqInstituicaoCriancaColoNoite",
                       "alunosFreqInstituicaoOutraDificLocomManha",
                       "alunosFreqInstituicaoOutraDificLocomTarde",
                       "alunosFreqInstituicaoOutraDificLocomNoite",
                       "pessoasFreqInstituicaoSegundaManha",
                       "pessoasFreqInstituicaoSegundaTarde",
                       "pessoasFreqInstituicaoSegundaNoite",
                       "pessoasFreqInstituicaoTercaManha",
                       "pessoasFreqInstituicaoTercaTarde",
                       "pessoasFreqInstituicaoTercaNoite",
                       "pessoasFreqInstituicaoQuartaManha",
                       "pessoasFreqInstituicaoQuartaTarde",
                       "pessoasFreqInstituicaoQuartaNoite",
                       "pessoasFreqInstituicaoQuintaManha",
                       "pessoasFreqInstituicaoQuintaTarde",
                       "pessoasFreqInstituicaoQuintaNoite",
                       "pessoasFreqInstituicaoSextaManha",
                       "pessoasFreqInstituicaoSextaTarde",
                       "pessoasFreqInstituicaoSextaNoite",
                       "pessoasFreqInstituicaoSabadoManha",
                       "pessoasFreqInstituicaoSabadoTarde",
                       "pessoasFreqInstituicaoSabadoNoite",
                       "pessoasFreqInstituicaoDomingoManha",
                       "pessoasFreqInstituicaoDomingoTarde",
                       "pessoasFreqInstituicaoDomingoNoite",
                       "pessoasFreqInstituicaoDeficiencia",
                       "pessoasFreqInstituicaoDeficienciaTipo",
                       "pessoasFreqInstituicaoDeficienciaFisicaManha",
                       "pessoasFreqInstituicaoDeficienciaFisicaTarde",
                       "pessoasFreqInstituicaoDeficienciaFisicaNoite",
                       "pessoasFreqInstituicaoDeficienciaAuditivaManha",
                       "pessoasFreqInstituicaoDeficienciaAuditivaTarde",
                       "pessoasFreqInstituicaoDeficienciaAuditivaNoite",
                       "pessoasFreqInstituicaoDeficienciaVisualManha",
                       "pessoasFreqInstituicaoDeficienciaVisualTarde",
                       "pessoasFreqInstituicaoDeficienciaVisualNoite",
                       "pessoasFreqInstituicaoDeficienciaMentalManha",
                       "pessoasFreqInstituicaoDeficienciaMentalTarde",
                       "pessoasFreqInstituicaoDeficienciaMentalNoite",
                       "pessoasFreqInstituicaoDeficienciaFalaManha",
                       "pessoasFreqInstituicaoDeficienciaFalaTarde",
                       "pessoasFreqInstituicaoDeficienciaFalaNoite",
                       "pessoasFreqInstituicaoDificuldadeLocomocao",
                       "pessoasFreqInstituicaoDificuldadeLocomocaoTipo",
                       "pessoasFreqInstituicaoDificuldadeCaminharManha",
                       "pessoasFreqInstituicaoDificuldadeCaminharTarde",
                       "pessoasFreqInstituicaoDificuldadeCaminharNoite",
                       "pessoasFreqInstituicaoAcamadasManha",
                       "pessoasFreqInstituicaoAcamadasTarde",
                       "pessoasFreqInstituicaoAcamadasNoite",
                       "pessoasFreqInstituicaoCadeirantesManha",
                       "pessoasFreqInstituicaoCadeirantesTarde",
                       "pessoasFreqInstituicaoCadeirantesNoite",
                       "pessoasFreqInstituicaoBengalaManha",
                       "pessoasFreqInstituicaoBengalaTarde",
                       "pessoasFreqInstituicaoBengalaNoite",
                       "pessoasFreqInstituicaoMembroAmputadoManha",
                       "pessoasFreqInstituicaoMembroAmputadoTarde",
                       "pessoasFreqInstituicaoMembroAmputadoNoite",
                       "pessoasFreqInstituicaoProblRespitarioLocomManha",
                       "pessoasFreqInstituicaoProblRespitarioLocomTarde",
                       "pessoasFreqInstituicaoProblRespitarioLocomNoite",
                       "pessoasFreqInstituicaoPosOperatorioManha",
                       "pessoasFreqInstituicaoPosOperatorioTarde",
                       "pessoasFreqInstituicaoPosOperatorioNoite",
                       "pessoasFreqInstituicaoProteseManha",
                       "pessoasFreqInstituicaoProteseTarde",
                       "pessoasFreqInstituicaoProteseNoite",
                       "pessoasFreqInstituicaoProblColunaColomManha",
                       "pessoasFreqInstituicaoProblColunaColomTarde",
                       "pessoasFreqInstituicaoProblColunaColomNoite",
                       "pessoasFreqInstituicaoObesasManha",
                       "pessoasFreqInstituicaoObesasNoite",
                       "pessoasFreqInstituicaoSeqAcidenteManha",
                       "pessoasFreqInstituicaoSeqAcidenteTarde",
                       "pessoasFreqInstituicaoSeqAcidenteNoite",
                       "pessoasFreqInstituicaoIdadeAvancadaManha",
                       "pessoasFreqInstituicaoIdadeAvancadaTarde",
                       "pessoasFreqInstituicaoIdadeAvancadaNoite",
                       "pessoasFreqInstituicaoCriancaColoManha",
                       "pessoasFreqInstituicaoCriancaColoTarde",
                       "pessoasFreqInstituicaoCriancaColoNoite",
                       "pessoasFreqInstituicaoOutraDificLocomManha",
                       "pessoasFreqInstituicaoOutraDificLocomTarde",
                       "pessoasFreqInstituicaoOutraDificLocomNoite",
                       "pessoasFreqInstituicaoSemanaTotal",
                       "publicoExternoAtendidoManha",
                       "publicoExternoAtendidoTarde",
                       "publicoExternoAtendidoNoite",
                       "publicoExternoAtendidoMadrugada",
                       "pessoasAtendDeficienciaDificLocomocaoSemana",
                       "possuiProdQuimRadioativoInstituicao",
                       "possuiProdQuimRadioativoInstituicaoQuais",
                       "volumeProdQuimRadioativoInstituicao",
                       "armazenamentoProdQuimRadioativoInstituicao",
                       "acessoEdificacaoInstituicao",
                       "possuiSaidaEmergInstituicao",
                       "numSaidasEmergInstituicao",
                       "possuiBrigIncendioInstituicao",
                       "isolamentoAcusticoInstituicao",
                       "ativSinaisSonorosQuaisInstituicao",
                       "ativSinaisSonorosQuaisOutrosInstituicao",
                       "contatoEmergInstituicao",
                       "contatoEmergTelInstituicao")

Filtro10[c("idSincronismo","data","cidade","logradouro","numero","complemento","localidade","observacoes","bairroDistrito")] <- NULL

Filtro10$informaTelInstituicao <- ifelse(Filtro10$informaTelInstituicao == "0", "Não",
                                              ifelse(Filtro10$informaTelInstituicao == "1", "Sim", NA))

Filtro10$telInstituicao[Filtro10$informaTelInstituicao=="Não"] <- NA

Filtro10$telInstituicao[Filtro10$telInstituicao=="(00) 000000000" |
                               Filtro10$telInstituicao=="(00) 00000000" |
                               Filtro10$telInstituicao=="(00) 999999999" |
                               Filtro10$telInstituicao=="(00) 99999999" |
                               Filtro10$telInstituicao=="(99) 999999999" |
                               Filtro10$telInstituicao=="(99) 99999999" |
                               Filtro10$telInstituicao=="(31) 000000000" |
                               Filtro10$telInstituicao=="(31) 00000000" |
                               Filtro10$telInstituicao=="(31) 999999999" |
                               Filtro10$telInstituicao=="(31) 99999999" |
                               Filtro10$telInstituicao=="(94) 000000000" |
                               Filtro10$telInstituicao=="(94) 00000000" |
                               Filtro10$telInstituicao=="(94) 999999999" |
                               Filtro10$telInstituicao=="(94) 99999999"] <- NA
Filtro10$informaTelInstituicao[is.na(Filtro10$telInstituicao)] <- "Não"

Filtro10$informaCnpjInstituicao <- ifelse(Filtro10$informaCnpjInstituicao == "0", "Não",
                                               ifelse(Filtro10$informaCnpjInstituicao == "1", "Sim",
                                                       ifelse(Filtro10$informaCnpjInstituicao=="77", "Não possui CNPJ",
                                                               ifelse(Filtro10$informaCnpjInstituicao=="88", "Não sabe",
                                                                       ifelse(Filtro10$informaCnpjInstituicao=="99", "Recusou informar CNPJ", "Não sabe")))))
Filtro10$cnpjInstituicao[Filtro10$informaCnpjInstituicao!="Sim"] <- NA
Filtro10$informaCnpjInstituicao[is.na(Filtro10$cnpjInstituicao)] <- "Não"

Filtro10 <- add_column(Filtro10, "funcionaSegundaInst"="", .after = "diasFuncionaInstituicao")
Filtro10 <- add_column(Filtro10, "funcionaTercaInst"="", .after = "funcionaSegundaInst")
Filtro10 <- add_column(Filtro10, "funcionaQuartaInst"="", .after = "funcionaTercaInst")
Filtro10 <- add_column(Filtro10, "funcionaQuintaInst"="", .after = "funcionaQuartaInst")
Filtro10 <- add_column(Filtro10, "funcionaSextaInst"="", .after = "funcionaQuintaInst")
Filtro10 <- add_column(Filtro10, "funcionaSabadoInst"="", .after = "funcionaSextaInst")
Filtro10 <- add_column(Filtro10, "funcionaDomingoInst"="", .after = "funcionaSabadoInst")

Filtro10 <- mutate(Filtro10, "funcionaSegundaInst" = ifelse(grepl("1", diasFuncionaInstituicao), "Sim", NA))
Filtro10 <- mutate(Filtro10, "funcionaTercaInst" = ifelse(grepl("2", diasFuncionaInstituicao), "Sim", NA))
Filtro10 <- mutate(Filtro10, "funcionaQuartaInst" = ifelse(grepl("3", diasFuncionaInstituicao), "Sim", NA))
Filtro10 <- mutate(Filtro10, "funcionaQuintaInst" = ifelse(grepl("4", diasFuncionaInstituicao), "Sim", NA))
Filtro10 <- mutate(Filtro10, "funcionaSextaInst" = ifelse(grepl("5", diasFuncionaInstituicao), "Sim", NA))
Filtro10 <- mutate(Filtro10, "funcionaSabadoInst" = ifelse(grepl("6", diasFuncionaInstituicao), "Sim", NA))
Filtro10 <- mutate(Filtro10, "funcionaDomingoInst" = ifelse(grepl("7", diasFuncionaInstituicao), "Sim", NA))

Filtro10$diasFuncionaInstituicao <- str_replace_all(Filtro10$diasFuncionaInstituicao, c("1"="Segunda",
                                                                                        "2"="Terça",
                                                                                        "3"="Quarta",
                                                                                        "4"="Quinta",
                                                                                        "5"="Sexta",
                                                                                        "6"="Sábado",
                                                                                        "7"="Domingo",
                                                                                        "8"="Não sabe/ não respondeu"))

Filtro10$alunosFreqEscolaManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqEscolaTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqEscolaNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosDeficiencia[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosDeficienciaQuais[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaFisicaManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaFisicaTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaFisicaNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaAuditivaManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaAuditivaTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaAuditivaNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaVisualManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaVisualTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaVisualNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaMentalManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaMentalTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaMentalNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaFalaManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaFalaTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDeficienciaFalaNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDificuldadeLocomocao[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDificuldadeLocomocaoTipo[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDificuldadeCaminharManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDificuldadeCaminharTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoDificuldadeCaminharNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoAcamadasManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoAcamadasTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoAcamadasNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoCadeirantesManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoCadeirantesTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoCadeirantesNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoBengalaManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoBengalaTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoBengalaNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoMembroAmputadoManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoMembroAmputadoTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoMembroAmputadoNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoProblRespitarioLocomManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoProblRespitarioLocomTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoProblRespitarioLocomNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoPosOperatorioManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoPosOperatorioTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoPosOperatorioNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoProteseManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoProteseTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoProteseNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoProblColunaColomManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoProblColunaColomTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoProblColunaColomNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoObesasManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoObesasTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoObesasNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoSeqAcidenteManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoSeqAcidenteTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoSeqAcidenteNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoIdadeAvancadaManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoIdadeAvancadaTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoIdadeAvancadaNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoCriancaColoManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoCriancaColoTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoCriancaColoNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoOutraDificLocomManha[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoOutraDificLocomTarde[Filtro10$entidadesTipos!="Instituição educacional"] <- NA
Filtro10$alunosFreqInstituicaoOutraDificLocomNoite[Filtro10$entidadesTipos!="Instituição educacional"] <- NA

Filtro10$pessoasFreqInstituicaoSegundaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSegundaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSegundaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSegundaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoTercaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoTercaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoTercaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoQuartaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoQuartaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoQuartaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoQuintaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoQuintaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoQuintaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSextaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSextaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSextaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSabadoManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSabadoTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSabadoNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDomingoManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDomingoTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDomingoNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficiencia[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaTipo[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaFisicaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaFisicaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaFisicaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaAuditivaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaAuditivaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaAuditivaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaVisualManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaVisualTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaVisualNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaMentalManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaMentalTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaMentalNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaFalaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaFalaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDeficienciaFalaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDificuldadeLocomocao[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDificuldadeLocomocaoTipo[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDificuldadeCaminharManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDificuldadeCaminharTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoDificuldadeCaminharNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoAcamadasManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoAcamadasTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoAcamadasNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoCadeirantesManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoCadeirantesTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoCadeirantesNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoBengalaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoBengalaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoBengalaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoMembroAmputadoManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoMembroAmputadoTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoMembroAmputadoNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoProblRespitarioLocomManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoProblRespitarioLocomTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoProblRespitarioLocomNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoPosOperatorioManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoPosOperatorioTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoPosOperatorioNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoProteseManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoProteseTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoProteseNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoProblColunaColomManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoProblColunaColomTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoProblColunaColomNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoObesasManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoObesasNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSeqAcidenteManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSeqAcidenteTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSeqAcidenteNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoIdadeAvancadaManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoIdadeAvancadaTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoIdadeAvancadaNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoCriancaColoManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoCriancaColoTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoCriancaColoNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoOutraDificLocomManha[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoOutraDificLocomTarde[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoOutraDificLocomNoite[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA
Filtro10$pessoasFreqInstituicaoSemanaTotal[Filtro10$entidadesTipos!="Templo religioso" | Filtro10$entidadesTipos!="Organização não-governamental"] <- NA

Filtro10$pessoasFreqInstituicaoDeficiencia[Filtro10$entidadesTipos=="Templo religioso" | Filtro10$entidadesTipos=="Organização não-governamental"] <- "Não sabe/ não respondeu"
Filtro10$pessoasFreqInstituicaoDeficienciaTipo[Filtro10$entidadesTipos=="Templo religioso" | Filtro10$entidadesTipos=="Organização não-governamental"] <- "Não sabe/ não respondeu"

Filtro10$publicoExternoAtendidoManha[Filtro10$entidadesTipos== "Hospital/Unidade de Saúde" |
                                          Filtro10$entidadesTipos==  "Instituição Pública" |
                                          Filtro10$entidadesTipos!= "Autarquia" |
                                          Filtro10$entidadesTipos!= "Associação" |
                                          Filtro10$entidadesTipos!= "Entidades em geral"] <- NA
Filtro10$publicoExternoAtendidoTarde[Filtro10$entidadesTipos== "Hospital/Unidade de Saúde" |
                                          Filtro10$entidadesTipos==  "Instituição Pública" |
                                          Filtro10$entidadesTipos!= "Autarquia" |
                                          Filtro10$entidadesTipos!= "Associação" |
                                          Filtro10$entidadesTipos!= "Entidades em geral"] <- NA
Filtro10$publicoExternoAtendidoNoite[Filtro10$entidadesTipos== "Hospital/Unidade de Saúde" |
                                          Filtro10$entidadesTipos==  "Instituição Pública" |
                                          Filtro10$entidadesTipos!= "Autarquia" |
                                          Filtro10$entidadesTipos!= "Associação" |
                                          Filtro10$entidadesTipos!= "Entidades em geral"] <- NA

Filtro10$possuiProdQuimRadioativoInstituicao <- ifelse(Filtro10$possuiProdQuimRadioativoInstituicao == "0", "Não",
                                                            ifelse(Filtro10$possuiProdQuimRadioativoInstituicao == "1", "Sim",
                                                                   ifelse(Filtro10$possuiProdQuimRadioativoInstituicao == "88", "Não sabe",
                                                                          ifelse(Filtro10$possuiProdQuimRadioativoInstituicao == "99", "Recusou responder", "Não sabe"))))

Filtro10$possuiProdQuimRadioativoInstituicaoQuais[Filtro10$possuiProdQuimRadioativoInstituicao!="Sim"] <- NA

Filtro10$volumeProdQuimRadioativoInstituicao[Filtro10$possuiProdQuimRadioativoInstituicao!="Sim"] <- NA

Filtro10 <- add_column(Filtro10, "rampasInst"="", .after = "acessoEdificacaoInstituicao")
Filtro10 <- add_column(Filtro10, "escadasInst"="", .after = "rampasInst")
Filtro10 <- add_column(Filtro10, "elevadoresInst"="", .after = "escadasInst")
Filtro10 <- add_column(Filtro10, "edificNivelRuaInst"="", .after = "elevadoresInst")
Filtro10 <- add_column(Filtro10, "outroAcessoInst"="", .after = "edificNivelRuaInst")

Filtro10 <- mutate(Filtro10, "rampasInst" = ifelse(grepl("1", acessoEdificacaoInstituicao), "Sim", ""))
Filtro10 <- mutate(Filtro10, "escadasInst" = ifelse(grepl("2", acessoEdificacaoInstituicao), "Sim", ""))
Filtro10 <- mutate(Filtro10, "elevadoresInst" = ifelse(grepl("3", acessoEdificacaoInstituicao), "Sim", ""))
Filtro10 <- mutate(Filtro10, "edificNivelRuaInst" = ifelse(grepl("4", acessoEdificacaoInstituicao), "Sim", ""))
Filtro10 <- mutate(Filtro10, "outroAcessoInst" = ifelse(grepl("66", acessoEdificacaoInstituicao), "Sim", ""))

Filtro10$possuiSaidaEmergInstituicao <- ifelse(Filtro10$possuiSaidaEmergInstituicao == "0", "Não",
                                                    ifelse(Filtro10$possuiSaidaEmergInstituicao == "1", "Sim", "Não sabe"))

Filtro10$numSaidasEmergInstituicao[Filtro10$possuiSaidaEmergInstituicao!="Sim"] <- NA

Filtro10$possuiSaidaEmergInstituicao[Filtro10$numSaidasEmergInstituicao=="0"] <- "Não"

Filtro10$possuiBrigIncendioInstituicao <- ifelse(Filtro10$possuiBrigIncendioInstituicao == "0", "Não",
                                                      ifelse(Filtro10$possuiBrigIncendioInstituicao == "1", "Sim", "Não sabe"))

Filtro10$isolamentoAcusticoInstituicao <- ifelse(Filtro10$isolamentoAcusticoInstituicao == "0", "Não",
                                             ifelse(Filtro10$isolamentoAcusticoInstituicao == "1", "Sim",
                                                     ifelse(Filtro10$isolamentoAcusticoInstituicao == "88", "Não sabe",
                                                             ifelse(Filtro10$isolamentoAcusticoInstituicao == "99", "Recusou informar", "Não sabe"))))

Filtro10$ativSinaisSonorosQuaisInstituicao <- ifelse(Filtro10$ativSinaisSonorosQuaisInstituicao=="0", "Não",
                                                      ifelse(Filtro10$ativSinaisSonorosQuaisInstituicao=="1", "Sim",
                                                              ifelse(Filtro10$ativSinaisSonorosQuaisInstituicao=="88", "Não sabe",
                                                                      ifelse(Filtro10$ativSinaisSonorosQuaisInstituicao=="99", "Recusou informar", "Não sabe"))))
Filtro10$ativSinaisSonorosQuaisOutrosInstituicao[Filtro10$ativSinaisSonorosQuaisInstituicao!="Sim"] <- NA

Filtro10$contatoEmergTelInstituicao[Filtro10$contatoEmergTelInstituicao=="(00) 000000000" |
                               Filtro10$contatoEmergTelInstituicao=="(00) 00000000" |
                               Filtro10$contatoEmergTelInstituicao=="(00) 999999999" |
                               Filtro10$contatoEmergTelInstituicao=="(00) 99999999" |
                               Filtro10$contatoEmergTelInstituicao=="(99) 999999999" |
                               Filtro10$contatoEmergTelInstituicao=="(99) 99999999" |
                               Filtro10$contatoEmergTelInstituicao=="(31) 000000000" |
                               Filtro10$contatoEmergTelInstituicao=="(31) 00000000" |
                               Filtro10$contatoEmergTelInstituicao=="(31) 999999999" |
                               Filtro10$contatoEmergTelInstituicao=="(31) 99999999" |
                               Filtro10$contatoEmergTelInstituicao=="(94) 000000000" |
                               Filtro10$contatoEmergTelInstituicao=="(94) 00000000" |
                               Filtro10$contatoEmergTelInstituicao=="(94) 999999999" |
                               Filtro10$contatoEmergTelInstituicao=="(94) 99999999"] <- NA

Filtro10$entidadesTipos <- NULL


#Filtro 11
Filtro11 <- read_excel("Filtro_11.xlsx")
colnames(Filtro11) = c("idSincronismo",
                       "id",
                       "cidade",
                       "nome",
                       "mora",
                       "trabalha",
                       "cargoInstituicao",
                       "tempoTrabalho",
                       "informaEmail",
                       "email",
                       "informaCPF",
                       "cpf",
                       "informaRG",
                       "rg",
                       "sexo",
                       "informaDataNascimento",
                       "dataNascimento",
                       "informaIdade",
                       "idade",
                       "estadoCivil",
                       "informaNomeMae",
                       "nomeMae",
                       "estudando",
                       "serieEstudando",
                       "informaNomeInstEnsino",
                       "instEnsino",
                       "informaEnderecoInstEnsino",
                       "enderecoInstEnsino",
                       "diasFreqInstEnsino",
                       "turnoFreqInstEnsino",
                       "transpInstEnsino",
                       "localTrabalho",
                       "deficiencias",
                       "deficienciaFisicaEspecif",
                       "doencas",
                       "dificuldadesLocomocao",
                       "gravidez",
                       "usaMedicamentos",
                       "medicamentos",
                       "informaTelFixo",
                       "telFixo",
                       "informaTelCel",
                       "telCel",
                       "informaTelCom",
                       "telCom",
                       "responsavelDomicilio",
                       "relacaoResponsavelDomicilio",
                       "segundaTurnoCasa",
                       "tercaTurnoCasa",
                       "quartaTurnoCasa",
                       "quintaTurnoCasa",
                       "sextaTurnoCasa",
                       "sabadoTurnoCasa",
                       "domingoTurnoCasa",
                       "diasTrabalho",
                       "segundaHoraTrabalho",
                       "tercaHoraTrabalho",
                       "quartaHoraTrabalho",
                       "quintaHoraTrabalho",
                       "sextaHoraTrabalho",
                       "sabadoHoraTrabalho",
                       "domingoHoraTrabalho",
                       "responsavelInstituicao",
                       "conheceZAS_ZSS",
                       "conheceRotasFugaPE",
                       "conheceRotasFugaPEQuais")
Filtro11$cidade <- NULL

Filtro11$informaEnderecoInstEnsino <- ifelse(Filtro11$informaEnderecoInstEnsino=="1", "Sim",
                                             ifelse(Filtro11$informaEnderecoInstEnsino=="2", "Não possui aulas presenciais (EAD)",
                                                     ifelse(Filtro11$informaEnderecoInstEnsino=="3", "Não sabe/ não respondeu", NA)))

Filtro11 <- add_column(Filtro11, "respondente"="Sim", .after = "id")

Filtro11$localTrabalho <- ifelse(Filtro11$localTrabalho == "2", "No bairro/distrito que mora",
                                 ifelse(Filtro11$localTrabalho == "3", "Outro bairro/distrito ou na sede do Município",
                                         ifelse(Filtro11$localTrabalho == "4", "Outro Município",
                                                 ifelse(Filtro11$localTrabalho == "5", "Não tem local fixo",
                                                         ifelse(Filtro11$localTrabalho == "6", "Não trabalha", NA)))))

Filtro11$estadoCivil <- ifelse(Filtro11$estadoCivil=="1", "Solteiro",
                               ifelse(Filtro11$estadoCivil=="2", "Casado",
                                       ifelse(Filtro11$estadoCivil=="3", "União estável",
                                               ifelse(Filtro11$estadoCivil=="4", "Separado não oficialmente",
                                                       ifelse(Filtro11$estadoCivil=="5", "Divorciado",
                                                               ifelse(Filtro11$estadoCivil=="6", "Viúvo", NA))))))

Filtro11$tempoTrabalho <- ifelse(Filtro11$tempoTrabalho=="1", "1 ano ou menos",
                                 ifelse(Filtro11$tempoTrabalho=="2", "De 2 a 5 anos",
                                        ifelse(Filtro11$tempoTrabalho=="3", "De 5 a 10 anos",
                                               ifelse(Filtro11$tempoTrabalho=="4", "De 10 a 15 anos",
                                                      ifelse(Filtro11$tempoTrabalho=="5", "Mais de 15 anos", NA)))))


#Filtro 13
Filtro13 <- read_excel("Filtro_13.xlsx")
colnames(Filtro13) = c("idSincronismo",
                       "id",
                       "data",
                       "dataSincronismo",
                       "cidade",
                       "nome",
                       "mora",
                       "trabalha",
                       "cargoInstituicao",
                       "localTrabalho",
                       "sexo",
                       "informaDataNascimento",
                       "dataNascimento",
                       "informaIdade",
                       "idade",
                       "informaEmail",
                       "email",
                       "informaCPF",
                       "cpf",
                       "responsavelDomicilio",
                       "relacaoResponsavelDomicilio",
                       "segundaTurnoCasa",
                       "tercaTurnoCasa",
                       "quartaTurnoCasa",
                       "quintaTurnoCasa",
                       "sextaTurnoCasa",
                       "sabadoTurnoCasa",
                       "domingoTurnoCasa",
                       "diasTrabalho",
                       "segundaHoraTrabalho",
                       "tercaHoraTrabalho",
                       "quartaHoraTrabalho",
                       "quintaHoraTrabalho",
                       "sextaHoraTrabalho",
                       "sabadoHoraTrabalho",
                       "domingoHoraTrabalho",
                       "deficiencias",
                       "deficienciaFisicaEspecif",
                       "dificuldadesLocomocao",
                       "doencas",
                       "usaMedicamentos",
                       "medicamentos",
                       "gravidez",
                       "estadoCivil",
                       "estudando",
                       "serieEstudando",
                       "informaNomeInstEnsino",
                       "instEnsino",
                       "informaEnderecoInstEnsino",
                       "enderecoInstEnsino",
                       "turnoFreqInstEnsino",
                       "diasFreqInstEnsino",
                       "transpInstEnsino",
                       "brigadaIncendio",
                       "CIPA",
                       "informaTelFixo",
                       "telFixo",
                       "informaTelCel",
                       "telCel",
                       "informaTelCom",
                       "telCom")
Filtro13 <- Filtro13 %>% select(-c("data","dataSincronismo","cidade"))

Filtro13 <- add_column(Filtro13, "respondente"="Não", .after = "id")

Filtro13$sexo <- ifelse(Filtro13$sexo == "1", "Feminino",
                        ifelse(Filtro13$sexo == "2", "Masculino",
                               ifelse(Filtro13$sexo == "66", "Outro", NA)))

Filtro13$informaEnderecoInstEnsino <- ifelse(Filtro13$informaEnderecoInstEnsino == "1", "Sim",
                                              ifelse(Filtro13$informaEnderecoInstEnsino == "3", "Não possui aulas presenciais (EAD)",
                                                      ifelse(Filtro13$informaEnderecoInstEnsino == "4", "Não sabe/ não respondeu", NA)))

Filtro13$localTrabalho <- ifelse(Filtro13$localTrabalho == "1", "No bairro/distrito que mora",
                                  ifelse(Filtro13$localTrabalho == "2", "Outro bairro/distrito ou na sede do Município",
                                          ifelse(Filtro13$localTrabalho == "3", "Outro Município",
                                                  ifelse(Filtro13$localTrabalho == "4", "Não tem local fixo",
                                                          ifelse(Filtro13$localTrabalho == "5", "Não trabalha",
                                                                  ifelse(Filtro13$localTrabalho == "6", "Não sabe/ não respondeu", NA))))))

Filtro13$estadoCivil <- ifelse(Filtro13$estadoCivil == "1", "Solteiro",
                               ifelse(Filtro13$estadoCivil == "2", "Casado",
                                       ifelse(Filtro13$estadoCivil == "3", "União estável",
                                               ifelse(Filtro13$estadoCivil == "4", "Divorciado",
                                                       ifelse(Filtro13$estadoCivil == "5", "Viúvo",
                                                               ifelse(Filtro13$estadoCivil == "6", "Separado não oficialmente",
                                                                       ifelse(Filtro13$estadoCivil == "99", "Não sabe/ não respondeu", NA)))))))


#Monta base de pessoas
pessoas <- merge(Filtro11, Filtro13, all = TRUE)
pessoas <- pessoas %>% filter(nome != "")

pessoas <- pessoas[c("idSincronismo",
                     "id",
                     "codPessoa",
                     "nome",
                     "respondente",
                     "mora",
                     "trabalha",
                     "cargoInstituicao",
                     "informaEmail",
                     "email",
                     "informaCPF",
                     "cpf",
                     "informaRG",
                     "rg",
                     "sexo",
                     "informaDataNascimento",
                     "dataNascimento",
                     "informaIdade",
                     "idade",
                     "estadoCivil",
                     "informaNomeMae",
                     "nomeMae",
                     "estudando",
                     "serieEstudando",
                     "informaNomeInstEnsino",
                     "instEnsino",
                     "informaEnderecoInstEnsino",
                     "enderecoInstEnsino",
                     "diasFreqInstEnsino",
                     "turnoFreqInstEnsino",
                     "transpInstEnsino",
                     "localTrabalho",
                     "deficiencias",
                     "deficienciaFisicaEspecif",
                     "doencas",
                     "dificuldadesLocomocao",
                     "gravidez",
                     "usaMedicamentos",
                     "medicamentos",
                     "informaTelFixo",
                     "telFixo",
                     "informaTelCel",
                     "telCel",
                     "informaTelCom",
                     "telCom",
                     "responsavelDomicilio",
                     "relacaoResponsavelDomicilio",
                     "segundaTurnoCasa",
                     "tercaTurnoCasa",
                     "quartaTurnoCasa",
                     "quintaTurnoCasa",
                     "sextaTurnoCasa",
                     "sabadoTurnoCasa",
                     "domingoTurnoCasa",
                     "diasTrabalho",
                     "segundaHoraTrabalho",
                     "tercaHoraTrabalho",
                     "quartaHoraTrabalho",
                     "quintaHoraTrabalho",
                     "sextaHoraTrabalho",
                     "sabadoHoraTrabalho",
                     "domingoHoraTrabalho",
                     "tempoTrabalho",
                     "responsavelInstituicao",
                     "conheceZAS_ZSS",
                     "conheceRotasFugaPE",
                     "conheceRotasFugaPEQuais",
                     "brigadaIncendio",
                     "CIPA")]

pessoas$nome <- toupper(gsub(pattern = "  ", replacement = " ", x = pessoas$nome))
# pessoas$nome <- str_replace(pessoas$nome, "  ", " ")

pessoas$mora <- ifelse(pessoas$mora == "1", "Sim", "Não")

pessoas$trabalha <- ifelse(pessoas$trabalha == "0", "Não",
                           ifelse(pessoas$trabalha == "1", "Sim",
                                  ifelse(pessoas$trabalha == "88", "Não sabe",
                                         ifelse(pessoas$trabalha == "99", "Recusou informar", NA))))

pessoas$cargoInstituicao[pessoas$trabalha=="Não"] <- NA

pessoas$cargoInstituicao <- ifelse(pessoas$cargoInstituicao == "1", "Proprietário/Sócio",
                                   ifelse(pessoas$cargoInstituicao == "2", "Gerente/Diretor de área",
                                          ifelse(pessoas$cargoInstituicao == "3", "Funcionário",
                                                 ifelse(pessoas$cargoInstituicao == "4", "Voluntário",
                                                        ifelse(pessoas$cargoInstituicao == "5", "Estagiário/ aprendiz",
                                                               ifelse(pessoas$cargoInstituicao == "66", "Outro", NA))))))

pessoas$informaEmail[pessoas$trabalha=="Não"] <- NA
pessoas$informaEmail <- ifelse(pessoas$informaEmail=="1", "Sim",
                               ifelse(pessoas$informaEmail=="0", "Não", NA))
pessoas$email[pessoas$informaEmail=="Não"] <- NA
pessoas$email[pessoas$trabalha=="Não"] <- NA

pessoas$informaCPF <- ifelse(pessoas$informaCPF=="1", "Sim", "Não")
pessoas$cpf[pessoas$informaCPF=="Não"] <- NA

pessoas$informaRG <- ifelse(pessoas$informaRG=="1", "Sim", "Não")
pessoas$rg[pessoas$informaRG=="Não"] <- NA

pessoas$informaDataNascimento <- ifelse(pessoas$informaDataNascimento=="1", "Sim", "Não")
pessoas$dataNascimento[pessoas$informaDataNascimento=="Não"] <- NA
pessoas$dataNascimento <- as.Date(pessoas$dataNascimento)

pessoas$informaIdade <- ifelse(pessoas$informaIdade=="1", "Sim", "Não")
pessoas$idade[pessoas$informaIdade=="Não"] <- NA
pessoas$informaIdade[pessoas$informaIdade=="Sim" & is.na(pessoas$idade)] <- "Não"
pessoas$idade <- as.numeric(pessoas$idade)

pessoas$informaNomeMae <- ifelse(pessoas$informaNomeMae=="1", "Sim", "Não")
pessoas$nomeMae[pessoas$informaNomeMae=="Não"] <- NA

pessoas$estudando <- ifelse(pessoas$estudando == "0", "Não",
                             ifelse(pessoas$estudando == "1", "Sim",
                                     ifelse(pessoas$estudando == "99", "Não sabe/não respondeu", NA)))

pessoas$serieEstudando[pessoas$estudando=="Não"] <- NA
pessoas$informaNomeInstEnsino[pessoas$estudando=="Não"] <- NA
pessoas$instEnsino[pessoas$estudando=="Não"] <- NA
pessoas$informaEnderecoInstEnsino[pessoas$estudando=="Não"] <- NA
pessoas$enderecoInstEnsino[pessoas$estudando=="Não"] <- NA
pessoas$enderecoInstEnsino[pessoas$estudando=="Não"] <- NA
pessoas$diasFreqInstEnsino[pessoas$estudando=="Não"] <- NA
pessoas$turnoFreqInstEnsino[pessoas$estudando=="Não"] <- NA
pessoas$transpInstEnsino[pessoas$estudando=="Não"] <- NA

pessoas$serieEstudando <- ifelse(pessoas$serieEstudando=="1","1ª série ensino fundamental",
                                 ifelse(pessoas$serieEstudando=="2", "2ª série ensino fundamental",
                                        ifelse(pessoas$serieEstudando=="3", "3ª série ensino fundamental",
                                               ifelse(pessoas$serieEstudando=="4", "4ª série ensino fundamental",
                                                      ifelse(pessoas$serieEstudando=="5", "5ª série ensino fundamental",
                                                             ifelse(pessoas$serieEstudando=="6", "6ª série ensino fundamental",
                                                                    ifelse(pessoas$serieEstudando=="7", "7ª série ensino fundamental",
                                                                           ifelse(pessoas$serieEstudando=="8", "8ª série ensino fundamental",
                                                                                  ifelse(pessoas$serieEstudando=="9", "9ª série ensino fundamental",
                                                                                         ifelse(pessoas$serieEstudando=="10", "1º ano ensino médio",
                                                                                                ifelse(pessoas$serieEstudando=="11", "2º ano ensino médio",
                                                                                                       ifelse(pessoas$serieEstudando=="12", "3º ano ensino médio",
                                                                                                              ifelse(pessoas$serieEstudando=="13", "Formação técnica",
                                                                                                                     ifelse(pessoas$serieEstudando=="14", "Pré-vestibular",
                                                                                                                            ifelse(pessoas$serieEstudando=="15", "Superior",
                                                                                                                                   ifelse(pessoas$serieEstudando=="16","Pós-Graduação",
                                                                                                                                          ifelse(pessoas$serieEstudando=="17","Educação infantil","")))))))))))))))))

pessoas$informaNomeInstEnsino <- ifelse(pessoas$informaNomeInstEnsino == "0", "Não",
                                        ifelse(pessoas$informaNomeInstEnsino == "1", "Sim",
                                               ifelse(pessoas$informaNomeInstEnsino == "88", "Não sabe",
                                                      ifelse(pessoas$informaNomeInstEnsino == "99", "Recusou informar", NA))))

pessoas$instEnsino[pessoas$informaNomeInstEnsino=="Não"] <- NA
pessoas$enderecoInstEnsino[pessoas$informaEnderecoInstEnsino!="Sim"] <- NA

pessoas <- add_column(pessoas, "freqInstEnsinoSegunda"="", .after = "diasFreqInstEnsino")
pessoas <- add_column(pessoas, "freqInstEnsinoTerca"="", .after = "freqInstEnsinoSegunda")
pessoas <- add_column(pessoas, "freqInstEnsinoQuarta"="", .after = "freqInstEnsinoTerca")
pessoas <- add_column(pessoas, "freqInstEnsinoQuinta"="", .after = "freqInstEnsinoQuarta")
pessoas <- add_column(pessoas, "freqInstEnsinoSexta"="", .after = "freqInstEnsinoQuinta")
pessoas <- add_column(pessoas, "freqInstEnsinoSabado"="", .after = "freqInstEnsinoSexta")
pessoas <- add_column(pessoas, "freqInstEnsinoDomingo"="", .after = "freqInstEnsinoSabado")

pessoas <- mutate(pessoas, "freqInstEnsinoSegunda"= ifelse(grepl("1", diasFreqInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "freqInstEnsinoTerca"= ifelse(grepl("2", diasFreqInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "freqInstEnsinoQuarta"= ifelse(grepl("3", diasFreqInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "freqInstEnsinoQuinta"= ifelse(grepl("4", diasFreqInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "freqInstEnsinoSexta"= ifelse(grepl("5", diasFreqInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "freqInstEnsinoSabado"= ifelse(grepl("6", diasFreqInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "freqInstEnsinoDomingo"= ifelse(grepl("7", diasFreqInstEnsino), "Sim", NA))

pessoas$diasFreqInstEnsino <- str_replace_all(pessoas$diasFreqInstEnsino, c("1"="Segunda",
                                                                            "2"="Terça",
                                                                            "3"="Quarta",
                                                                            "4"="Quinta",
                                                                            "5"="Sexta",
                                                                            "6"="Sábado",
                                                                            "7"="Domingo",
                                                                            "8"="Não sabe/ não respondeu"))

pessoas <- add_column(pessoas, "freqInstEnsinoManha"="", .after = "turnoFreqInstEnsino")
pessoas <- add_column(pessoas, "freqInstEnsinoTarde"="", .after = "freqInstEnsinoManha")
pessoas <- add_column(pessoas, "freqInstEnsinoNoite"="", .after = "freqInstEnsinoTarde")

pessoas <- mutate(pessoas, "freqInstEnsinoManha" = ifelse(grepl("1", turnoFreqInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "freqInstEnsinoTarde" = ifelse(grepl("2", turnoFreqInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "freqInstEnsinoNoite" = ifelse(grepl("3", turnoFreqInstEnsino), "Sim", NA))

pessoas$turnoFreqInstEnsino <- str_replace_all(pessoas$turnoFreqInstEnsino, c("1"="Manhã",
                                                                              "2"="Tarde",
                                                                              "3"="Noite",
                                                                              "4"="Não sabe/ não respondeu"))

pessoas <- add_column(pessoas, "carroPequeno"="", .after = "transpInstEnsino")
pessoas <- add_column(pessoas, "caminhonete"="", .after = "carroPequeno")
pessoas <- add_column(pessoas, "moto"="", .after = "caminhonete")
pessoas <- add_column(pessoas, "caminhao"="", .after = "moto")
pessoas <- add_column(pessoas, "bicicleta"="", .after = "caminhao")
pessoas <- add_column(pessoas, "cavaloCarroca"="", .after = "bicicleta")
pessoas <- add_column(pessoas, "barco"="", .after = "cavaloCarroca")
pessoas <- add_column(pessoas, "peAndando"="", .after = "barco")
pessoas <- add_column(pessoas, "carona"="", .after = "peAndando")
pessoas <- add_column(pessoas, "onibusColetivo"="", .after = "carona")
pessoas <- add_column(pessoas, "transpPrefeitura"="", .after = "onibusColetivo")
pessoas <- add_column(pessoas, "taxi"="", .after = "transpPrefeitura")
pessoas <- add_column(pessoas, "outroTransp"="", .after = "taxi")

pessoas <- mutate(pessoas, "outroTransp" = ifelse(grepl("13", transpInstEnsino), "Sim", NA))
pessoas$transpInstEnsino <- str_replace(pessoas$transpInstEnsino, "13;", "")
pessoas <- mutate(pessoas, "taxi" = ifelse(grepl("12", transpInstEnsino), "Sim", NA))
pessoas$transpInstEnsino <- str_replace(pessoas$transpInstEnsino, "12;", "")
pessoas <- mutate(pessoas, "transpPrefeitura" = ifelse(grepl("11", transpInstEnsino), "Sim", NA))
pessoas$transpInstEnsino <- str_replace(pessoas$transpInstEnsino, "11;", "")
pessoas <- mutate(pessoas, "onibusColetivo" = ifelse(grepl("10", transpInstEnsino), "Sim", NA))
pessoas$transpInstEnsino <- str_replace(pessoas$transpInstEnsino, "10;", "")

pessoas <- mutate(pessoas, "carona" = ifelse(grepl("9", transpInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "peAndando" = ifelse(grepl("8", transpInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "barco" = ifelse(grepl("7", transpInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "cavaloCarroca" = ifelse(grepl("6", transpInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "bicicleta" = ifelse(grepl("5", transpInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "caminhao" = ifelse(grepl("4", transpInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "moto" = ifelse(grepl("3", transpInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "caminhonete" = ifelse(grepl("2", transpInstEnsino), "Sim", NA))
pessoas <- mutate(pessoas, "carroPequeno" = ifelse(grepl("1", transpInstEnsino), "Sim", NA))

pessoas$transpInstEnsino <- NULL

pessoas$deficiencias[is.na(pessoas$deficiencias)] <- "NÃO COLETADO"
pessoas$deficiencias <- str_replace(pessoas$deficiencias,"77;", "Não possui deficiência")

pessoas <- add_column(pessoas, "deficienciaFisica"="", .after = "deficiencias")
pessoas <- mutate(pessoas, "deficienciaFisica" = ifelse(grepl("1", deficiencias), "Sim", NA))
pessoas <- add_column(pessoas, "deficienciaAuditiva"="", .after = "deficienciaFisicaEspecif")
pessoas <- mutate(pessoas, "deficienciaAuditiva" = ifelse(grepl("2", deficiencias), "Sim", NA))
pessoas <- add_column(pessoas, "deficienciaVisual"="", .after = "deficienciaAuditiva")
pessoas <- mutate(pessoas, "deficienciaVisual" = ifelse(grepl("3", deficiencias), "Sim", NA))
pessoas <- add_column(pessoas, "deficienciaMental" ="", .after = "deficienciaVisual")
pessoas <- mutate(pessoas, "deficienciaMental" = ifelse(grepl("4", deficiencias), "Sim", NA))
pessoas <- add_column(pessoas, "deficienciaFala" ="", .after = "deficienciaMental")
pessoas <- mutate(pessoas, "deficienciaFala" = ifelse(grepl("5", deficiencias), "Sim", NA))

pessoas <- add_column(pessoas, "possuiDeficiencia" = "", .after = "deficienciaFala")
pessoas$possuiDeficiencia <- ifelse(pessoas$deficiencias=="NÃO COLETADO", "NÃO COLETADO",
                                    ifelse(pessoas$deficiencias=="Não possui deficiência", "Não possui deficiência",
                                           ifelse((pessoas$deficienciaFisica=="Sim") | 
                                                     (pessoas$deficienciaAuditiva=="Sim") |
                                                     (pessoas$deficienciaVisual=="Sim") |
                                                     (pessoas$deficienciaMental=="Sim") |
                                                     (pessoas$deficienciaFala=="Sim"), "Sim", "")))

pessoas$deficienciaFisicaEspecif[pessoas$deficienciaFisica=="Não"] <- NA
pessoas$deficiencias <- NULL


pessoas$doencas[is.na(pessoas$doencas)] <- "NÃO COLETADO"
pessoas$doencas <- str_replace(pessoas$doencas,"77;", "Não possui doença")


pessoas <- add_column(pessoas, "pressaoAlta"="", .after = "doencas")
pessoas <- add_column(pessoas, "doencasCoracao"="", .after = "pressaoAlta")
pessoas <- add_column(pessoas, "diabetes"="", .after = "doencasCoracao")
pessoas <- add_column(pessoas, "cancer"="", .after = "diabetes")
pessoas <- add_column(pessoas, "doencaMental"="", .after = "cancer")
pessoas <- add_column(pessoas, "colesterol"="", .after = "doencaMental")
pessoas <- add_column(pessoas, "doencaRenal"="", .after = "colesterol")
pessoas <- add_column(pessoas, "depressaoAnsiedade"="", .after = "doencaRenal")
pessoas <- add_column(pessoas, "problemaRespiratorio"="", .after = "depressaoAnsiedade")
pessoas <- add_column(pessoas, "artriteArtrose"="", .after = "problemaRespiratorio")
pessoas <- add_column(pessoas, "tireoide"="", .after = "artriteArtrose")
pessoas <- add_column(pessoas, "problemaColuna"="", .after = "tireoide")
pessoas <- add_column(pessoas, "malaria"="", .after = "problemaColuna")
pessoas <- add_column(pessoas, "hepatite"="", .after = "malaria")
pessoas <- add_column(pessoas, "doencaChagas"="", .after = "hepatite")
pessoas <- add_column(pessoas, "esquistossomose"="", .after = "doencaChagas")
pessoas <- add_column(pessoas, "obsesidade"="", .after = "esquistossomose")
pessoas <- add_column(pessoas, "doencaNeurologica"="", .after = "obsesidade")
pessoas <- add_column(pessoas, "atrasoDesenvolvimento"="", .after = "doencaNeurologica")
pessoas <- add_column(pessoas, "outraDoenca"="", .after = "atrasoDesenvolvimento")

pessoas <- mutate(pessoas, "outraDoenca" = ifelse(grepl("66;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"66;", "")
pessoas <- mutate(pessoas, "atrasoDesenvolvimento" = ifelse(grepl("19;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"19;", "")
pessoas <- mutate(pessoas, "doencaNeurologica" = ifelse(grepl("18;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"18;", "")
pessoas <- mutate(pessoas, "obsesidade" = ifelse(grepl("17;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"17;", "")
pessoas <- mutate(pessoas, "esquistossomose" = ifelse(grepl("16;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"16;", "")
pessoas <- mutate(pessoas, "doencaChagas" = ifelse(grepl("15;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"15;", "")
pessoas <- mutate(pessoas, "hepatite" = ifelse(grepl("14;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"14;", "")
pessoas <- mutate(pessoas, "malaria" = ifelse(grepl("13;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"13;", "")
pessoas <- mutate(pessoas, "problemaColuna" = ifelse(grepl("12;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"12;", "")
pessoas <- mutate(pessoas, "tireoide" = ifelse(grepl("11;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"11;", "")
pessoas <- mutate(pessoas, "artriteArtrose" = ifelse(grepl("10;", doencas), "Sim", NA))
pessoas$doencas <- str_replace(pessoas$doencas,"10;", "")
pessoas <- mutate(pessoas, "problemaRespiratorio" = ifelse(grepl("9;", doencas), "Sim", NA))
pessoas <- mutate(pessoas, "depressaoAnsiedade" = ifelse(grepl("8;", doencas), "Sim", NA))
pessoas <- mutate(pessoas, "doencaRenal" = ifelse(grepl("7;", doencas), "Sim", NA))
pessoas <- mutate(pessoas, "colesterol" = ifelse(grepl("6;", doencas), "Sim", NA))
pessoas <- mutate(pessoas, "doencaMental" = ifelse(grepl("5;", doencas), "Sim", NA))
pessoas <- mutate(pessoas, "cancer" = ifelse(grepl("4;", doencas), "Sim", NA))
pessoas <- mutate(pessoas, "diabetes" = ifelse(grepl("3", doencas), "Sim", NA))
pessoas <- mutate(pessoas, "doencasCoracao" = ifelse(grepl("2", doencas), "Sim", NA))
pessoas <- mutate(pessoas, "pressaoAlta" = ifelse(grepl("1;", doencas), "Sim", NA))

pessoas <- add_column(pessoas, "possuiDoenca" = "", .after = "outraDoenca")

pessoas$possuiDoenca <- ifelse(pessoas$doencas=="NÃO COLETADO", "NÃO COLETADO",
                               ifelse(pessoas$doencas=="Não possui doença", "Não possui doença",
                                      ifelse(((pessoas$pressaoAlta=="Sim") | 
                                                (pessoas$doencasCoracao=="Sim") |
                                                (pessoas$diabetes=="Sim") |
                                                (pessoas$cancer=="Sim") |
                                                (pessoas$doencaMental=="Sim") |
                                                (pessoas$colesterol=="Sim") |
                                                (pessoas$doencaRenal=="Sim") |
                                                (pessoas$depressaoAnsiedade=="Sim") |
                                                (pessoas$problemaRespiratorio=="Sim") |
                                                (pessoas$artriteArtrose=="Sim") |
                                                (pessoas$tireoide=="Sim") |
                                                (pessoas$problemaColuna=="Sim") |
                                                (pessoas$malaria=="Sim") |
                                                (pessoas$hepatite=="Sim") |
                                                (pessoas$doencaChagas=="Sim") |
                                                (pessoas$esquistossomose=="Sim") |
                                                (pessoas$obsesidade=="Sim") |
                                                (pessoas$doencaNeurologica=="Sim") |
                                                (pessoas$atrasoDesenvolvimento=="Sim") |
                                                (pessoas$outraDoenca=="Sim")), "Sim", "")))
pessoas$doencas <- NULL

pessoas$dificuldadesLocomocao[is.na(pessoas$dificuldadesLocomocao)] <- "NÃO COLETADO"
pessoas$dificuldadesLocomocao <- str_replace(pessoas$dificuldadesLocomocao,"77;", "Não possui dificuldade de locomoção")

pessoas <- add_column(pessoas, "dificuldadeCaminhar"="", .after = "dificuldadesLocomocao")
pessoas <- add_column(pessoas, "pacienteAcamado"="", .after = "dificuldadeCaminhar")
pessoas <- add_column(pessoas, "cadeirante"="", .after = "pacienteAcamado")
pessoas <- add_column(pessoas, "bengalaAndador"="", .after = "cadeirante")
pessoas <- add_column(pessoas, "membroAmputado"="", .after = "bengalaAndador")
pessoas <- add_column(pessoas, "problemaRespiratorioDifLocom"="", .after = "membroAmputado")
pessoas <- add_column(pessoas, "posOperatorio"="", .after = "problemaRespiratorioDifLocom")
pessoas <- add_column(pessoas, "protesePerna"="", .after = "posOperatorio")
pessoas <- add_column(pessoas, "problemaColunaDifLocom"="", .after = "protesePerna")
pessoas <- add_column(pessoas, "obesidadeDifLocom"="", .after = "problemaColunaDifLocom")
pessoas <- add_column(pessoas, "sequelaAcidente"="", .after = "obesidadeDifLocom")
pessoas <- add_column(pessoas, "idadeAvancada"="", .after = "sequelaAcidente")
pessoas <- add_column(pessoas, "criancaColo"="", .after = "idadeAvancada")
pessoas <- add_column(pessoas, "outroDifLocom"="", .after = "criancaColo")

pessoas <- mutate(pessoas, "outroDifLocom" = ifelse(grepl("66;", dificuldadesLocomocao), "Sim", NA))
pessoas$dificuldadesLocomocao <- str_replace(pessoas$dificuldadesLocomocao,"66;", "")
pessoas <- mutate(pessoas, "criancaColo" = ifelse(grepl("13;", dificuldadesLocomocao), "Sim", NA))
pessoas$dificuldadesLocomocao <- str_replace(pessoas$dificuldadesLocomocao,"13;", "")
pessoas <- mutate(pessoas, "idadeAvancada" = ifelse(grepl("12;", dificuldadesLocomocao), "Sim", NA))
pessoas$dificuldadesLocomocao <- str_replace(pessoas$dificuldadesLocomocao,"12;", "")
pessoas <- mutate(pessoas, "sequelaAcidente" = ifelse(grepl("11;", dificuldadesLocomocao), "Sim", NA))
pessoas$dificuldadesLocomocao <- str_replace(pessoas$dificuldadesLocomocao,"11;", "")
pessoas <- mutate(pessoas, "obesidadeDifLocom" = ifelse(grepl("10;", dificuldadesLocomocao), "Sim", NA))
pessoas$dificuldadesLocomocao <- str_replace(pessoas$dificuldadesLocomocao,"10;", "")
pessoas <- mutate(pessoas, "problemaColunaDifLocom" = ifelse(grepl("9;", dificuldadesLocomocao), "Sim", NA))
pessoas <- mutate(pessoas, "protesePerna" = ifelse(grepl("8;", dificuldadesLocomocao), "Sim", NA))
pessoas <- mutate(pessoas, "posOperatorio" = ifelse(grepl("7;", dificuldadesLocomocao), "Sim", NA))
pessoas <- mutate(pessoas, "problemaRespiratorioDifLocom" = ifelse(grepl("6;", dificuldadesLocomocao), "Sim", NA))
pessoas <- mutate(pessoas, "membroAmputado" = ifelse(grepl("5;", dificuldadesLocomocao), "Sim", NA))
pessoas <- mutate(pessoas, "bengalaAndador" = ifelse(grepl("4;", dificuldadesLocomocao), "Sim", NA))
pessoas <- mutate(pessoas, "cadeirante" = ifelse(grepl("3;", dificuldadesLocomocao), "Sim", NA))
pessoas <- mutate(pessoas, "pacienteAcamado" = ifelse(grepl("2;", dificuldadesLocomocao), "Sim", NA))
pessoas <- mutate(pessoas, "dificuldadeCaminhar" = ifelse(grepl("1;", dificuldadesLocomocao), "Sim", NA))

pessoas <- add_column(pessoas, "possuiDificLocomocao" = "", .after = "outroDifLocom")
pessoas$possuiDificLocomocao <- ifelse(pessoas$dificuldadesLocomocao=="Não possui dificuldade de locomoção", "Não possui dificuldade de locomoção",
                                       ifelse(pessoas$dificuldadesLocomocao=="NÃO COLETADO", "NÃO COLETADO",
                                              ifelse((pessoas$dificuldadeCaminhar=="Sim") | 
                                                       (pessoas$pacienteAcamado=="Sim") |
                                                       (pessoas$cadeirante=="Sim") |
                                                       (pessoas$bengalaAndador=="Sim") |
                                                       (pessoas$membroAmputado=="Sim") |
                                                       (pessoas$problemaRespiratorioDifLocom=="Sim") |
                                                       (pessoas$posOperatorio=="Sim") |
                                                       (pessoas$protesePerna=="Sim") |
                                                       (pessoas$problemaColunaDifLocom=="Sim") |
                                                       (pessoas$obesidadeDifLocom=="Sim") |
                                                       (pessoas$sequelaAcidente=="Sim") |
                                                       (pessoas$idadeAvancada=="Sim") |
                                                       (pessoas$criancaColo=="Sim") |
                                                       (pessoas$outroDifLocom=="Sim"), "Sim", "")))
pessoas$dificuldadesLocomocao <- NULL

pessoas$gravidez <- ifelse(pessoas$gravidez=="0", "Não",
                            ifelse(pessoas$gravidez=="1", "Sim",
                                    ifelse(pessoas$gravidez=="88", "Não sabe",
                                            ifelse(pessoas$gravidez=="99", "Não respondeu", NA))))

pessoas$usaMedicamentos <- ifelse(pessoas$usaMedicamentos=="0", "Não",
                                   ifelse(pessoas$usaMedicamentos=="1", "Sim",
                                           ifelse(pessoas$usaMedicamentos=="88", "Não sabe",
                                                   ifelse(pessoas$usaMedicamentos=="99", "Não respondeu", NA))))
pessoas$medicamentos[pessoas$usaMedicamentos!="Sim"] <- NA

pessoas$informaTelCel <- ifelse(pessoas$informaTelCel=="1", "Sim", "Não")
pessoas$telCel[pessoas$informaTelCel=="Não"] <- NA
pessoas$telCel[pessoas$telCel=="(00) 000000000" |
                 pessoas$telCel=="(00) 00000000" |
                 pessoas$telCel=="(00) 999999999" |
                 pessoas$telCel=="(00) 99999999" |
                 pessoas$telCel=="(99) 999999999" |
                 pessoas$telCel=="(99) 99999999" |
                 pessoas$telCel=="(31) 000000000" |
                 pessoas$telCel=="(31) 00000000" |
                 pessoas$telCel=="(31) 999999999" |
                 pessoas$telCel=="(31) 99999999" |
                 pessoas$telCel=="(94) 000000000" |
                 pessoas$telCel=="(94) 00000000" |
                 pessoas$telCel=="(94) 999999999" |
                 pessoas$telCel=="(94) 99999999"] <- NA

pessoas$informaTelFixo <- ifelse(pessoas$informaTelFixo=="1", "Sim", "Não")
pessoas$telFixo[pessoas$informaTelFixo=="Não"] <- NA
pessoas$telFixo[pessoas$telFixo=="(00) 000000000" |
                 pessoas$telFixo=="(00) 00000000" |
                 pessoas$telFixo=="(00) 999999999" |
                 pessoas$telFixo=="(00) 99999999" |
                 pessoas$telFixo=="(99) 999999999" |
                 pessoas$telFixo=="(99) 99999999" |
                 pessoas$telFixo=="(31) 000000000" |
                 pessoas$telFixo=="(31) 00000000" |
                 pessoas$telFixo=="(31) 999999999" |
                 pessoas$telFixo=="(31) 99999999" |
                 pessoas$telFixo=="(94) 000000000" |
                 pessoas$telFixo=="(94) 00000000" |
                 pessoas$telFixo=="(94) 999999999" |
                 pessoas$telFixo=="(94) 99999999"] <- NA

pessoas$informaTelCom <- if_else(pessoas$informaTelCom=="1", "Sim", "Não", missing = NULL)
pessoas$telCom[pessoas$informaTelCom!="Sim"] <- NA
pessoas$telCom[pessoas$telCom=="(00) 000000000" |
                 pessoas$telCom=="(00) 00000000" |
                 pessoas$telCom=="(00) 999999999" |
                 pessoas$telCom=="(00) 99999999" |
                 pessoas$telCom=="(99) 999999999" |
                 pessoas$telCom=="(99) 99999999" |
                 pessoas$telCom=="(31) 000000000" |
                 pessoas$telCom=="(31) 00000000" |
                 pessoas$telCom=="(31) 999999999" |
                 pessoas$telCom=="(31) 99999999" |
                 pessoas$telCom=="(94) 000000000" |
                 pessoas$telCom=="(94) 00000000" |
                 pessoas$telCom=="(94) 999999999" |
                 pessoas$telCom=="(94) 99999999"] <- NA

pessoas$responsavelDomicilio <- ifelse(pessoas$responsavelDomicilio == "0", "Não",
                                       ifelse(pessoas$responsavelDomicilio == "1", "Sim",
                                              ifelse(pessoas$responsavelDomicilio == "2", "Não há uma pessoa responsável pelo domicílio",
                                                     ifelse(pessoas$responsavelDomicilio == "88", "Não sabe", NA))))

pessoas$relacaoResponsavelDomicilio <- ifelse(pessoas$relacaoResponsavelDomicilio == "1", "Cônjuge",
                                              ifelse(pessoas$relacaoResponsavelDomicilio == "2", "Filho(a)/ enteado(a)",
                                                     ifelse(pessoas$relacaoResponsavelDomicilio == "3", "Genro/ nora",
                                                            ifelse(pessoas$relacaoResponsavelDomicilio == "4", "Neto(a)",
                                                                   ifelse(pessoas$relacaoResponsavelDomicilio == "5", "Pai/ mãe",
                                                                          ifelse(pessoas$relacaoResponsavelDomicilio == "6", "Sogro(a)",
                                                                                 ifelse(pessoas$relacaoResponsavelDomicilio == "7", "Irmão(ã)",
                                                                                        ifelse(pessoas$relacaoResponsavelDomicilio == "8", "Cunhado(a)",
                                                                                               ifelse(pessoas$relacaoResponsavelDomicilio == "9", "Tio(a)",
                                                                                                      ifelse(pessoas$relacaoResponsavelDomicilio == "10", "Sobrinho(a)",
                                                                                                             ifelse(pessoas$relacaoResponsavelDomicilio == "11", "Avô(ó)",
                                                                                                                    ifelse(pessoas$relacaoResponsavelDomicilio == "12", "Primo(a)",
                                                                                                                           ifelse(pessoas$relacaoResponsavelDomicilio == "13", "Não parente",
                                                                                                                                  ifelse(pessoas$relacaoResponsavelDomicilio == "14", "Empregado(a)", "Outro"))))))))))))))

pessoas$diasTrabalho[pessoas$trabalha=="Não"] <- NA

pessoas <- add_column(pessoas, "segundaCasaManha"="", .after = "segundaTurnoCasa")
pessoas <- add_column(pessoas, "segundaCasaTarde"="", .after = "segundaCasaManha")
pessoas <- add_column(pessoas, "segundaCasaNoite"="", .after = "segundaCasaTarde")
pessoas <- add_column(pessoas, "segundaCasaMadrugada"="", .after = "segundaCasaNoite")
pessoas <- add_column(pessoas, "segundaCasaNaoFica"="", .after = "segundaCasaMadrugada")

pessoas <- add_column(pessoas, "tercaCasaManha"="", .after = "tercaTurnoCasa")
pessoas <- add_column(pessoas, "tercaCasaTarde"="", .after = "tercaCasaManha")
pessoas <- add_column(pessoas, "tercaCasaNoite"="", .after = "tercaCasaTarde")
pessoas <- add_column(pessoas, "tercaCasaMadrugada"="", .after = "tercaCasaNoite")
pessoas <- add_column(pessoas, "tercaCasaNaoFica"="", .after = "tercaCasaMadrugada")

pessoas <- add_column(pessoas, "quartaCasaManha"="", .after = "quartaTurnoCasa")
pessoas <- add_column(pessoas, "quartaCasaTarde"="", .after = "quartaCasaManha")
pessoas <- add_column(pessoas, "quartaCasaNoite"="", .after = "quartaCasaTarde")
pessoas <- add_column(pessoas, "quartaCasaMadrugada"="", .after = "quartaCasaNoite")
pessoas <- add_column(pessoas, "quartaCasaNaoFica"="", .after = "quartaCasaMadrugada")

pessoas <- add_column(pessoas, "quintaCasaManha"="", .after = "quintaTurnoCasa")
pessoas <- add_column(pessoas, "quintaCasaTarde"="", .after = "quintaCasaManha")
pessoas <- add_column(pessoas, "quintaCasaNoite"="", .after = "quintaCasaTarde")
pessoas <- add_column(pessoas, "quintaCasaMadrugada"="", .after = "quintaCasaNoite")
pessoas <- add_column(pessoas, "quintaCasaNaoFica"="", .after = "quintaCasaMadrugada")

pessoas <- add_column(pessoas, "sextaCasaManha"="", .after = "sextaTurnoCasa")
pessoas <- add_column(pessoas, "sextaCasaTarde"="", .after = "sextaCasaManha")
pessoas <- add_column(pessoas, "sextaCasaNoite"="", .after = "sextaCasaTarde")
pessoas <- add_column(pessoas, "sextaCasaMadrugada"="", .after = "sextaCasaNoite")
pessoas <- add_column(pessoas, "sextaCasaNaoFica"="", .after = "sextaCasaMadrugada")

pessoas <- add_column(pessoas, "sabadoCasaManha"="", .after = "sabadoTurnoCasa")
pessoas <- add_column(pessoas, "sabadoCasaTarde"="", .after = "sabadoCasaManha")
pessoas <- add_column(pessoas, "sabadoCasaNoite"="", .after = "sabadoCasaTarde")
pessoas <- add_column(pessoas, "sabadoCasaMadrugada"="", .after = "sabadoCasaNoite")
pessoas <- add_column(pessoas, "sabadoCasaNaoFica"="", .after = "sabadoCasaMadrugada")

pessoas <- add_column(pessoas, "domingoCasaManha"="", .after = "domingoTurnoCasa")
pessoas <- add_column(pessoas, "domingoCasaTarde"="", .after = "domingoCasaManha")
pessoas <- add_column(pessoas, "domingoCasaNoite"="", .after = "domingoCasaTarde")
pessoas <- add_column(pessoas, "domingoCasaMadrugada"="", .after = "domingoCasaNoite")
pessoas <- add_column(pessoas, "domingoCasaNaoFica"="", .after = "domingoCasaMadrugada")

pessoas <- mutate(pessoas, "segundaCasaManha"= ifelse(grepl("1", segundaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "segundaCasaTarde"= ifelse(grepl("2", segundaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "segundaCasaNoite"= ifelse(grepl("3", segundaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "segundaCasaMadrugada"= ifelse(grepl("4", segundaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "segundaCasaNaoFica"= ifelse(grepl("5", segundaTurnoCasa), "Sim",
                                                        ifelse(grepl("77", segundaTurnoCasa), "Sim", NA)))

pessoas$segundaTurnoCasa <- str_replace_all(pessoas$segundaTurnoCasa, c("1"="Manhã",
                                                                        "2"="Tarde",
                                                                        "3"="Noite",
                                                                        "4"="Madrugada",
                                                                        "5"="Não fica em casa nesse dia",
                                                                        "77"="Não fica em casa nesse dia",
                                                                        "88"="Não sabe",
                                                                        "99"="Não sabe/ não respondeu"))

pessoas <- mutate(pessoas, "tercaCasaManha"= ifelse(grepl("1", tercaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "tercaCasaTarde"= ifelse(grepl("2", tercaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "tercaCasaNoite"= ifelse(grepl("3", tercaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "tercaCasaMadrugada"= ifelse(grepl("4", tercaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "tercaCasaNaoFica"= ifelse(grepl("5", tercaTurnoCasa), "Sim",
                                                      ifelse(grepl("77", tercaTurnoCasa), "Sim", NA)))
pessoas$tercaTurnoCasa <- str_replace_all(pessoas$tercaTurnoCasa, c("1"="Manhã",
                                                                    "2"="Tarde",
                                                                    "3"="Noite",
                                                                    "4"="Madrugada",
                                                                    "5"="Não fica em casa nesse dia",
                                                                    "77"="Não fica em casa nesse dia",
                                                                    "99"="Não sabe/ não respondeu"))

pessoas <- mutate(pessoas, "quartaCasaManha"= ifelse(grepl("1", quartaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "quartaCasaTarde"= ifelse(grepl("2", quartaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "quartaCasaNoite"= ifelse(grepl("3", quartaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "quartaCasaMadrugada"= ifelse(grepl("4", quartaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "quartaCasaNaoFica"= ifelse(grepl("5", quartaTurnoCasa), "Sim",
                                                       ifelse(grepl("77", quartaTurnoCasa), "Sim", NA)))
pessoas$quartaTurnoCasa <- str_replace_all(pessoas$quartaTurnoCasa, c("1"="Manhã",
                                                                      "2"="Tarde",
                                                                      "3"="Noite",
                                                                      "4"="Madrugada",
                                                                      "5"="Não fica em casa nesse dia",
                                                                      "77"="Não fica em casa nesse dia",
                                                                      "88"="Não sabe",
                                                                      "99"="Não sabe/ não respondeu"))

pessoas <- mutate(pessoas, "quintaCasaManha"= ifelse(grepl("1", quintaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "quintaCasaTarde"= ifelse(grepl("2", quintaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "quintaCasaNoite"= ifelse(grepl("3", quintaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "quintaCasaMadrugada"= ifelse(grepl("4", quintaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "quintaCasaNaoFica"= ifelse(grepl("5", quintaTurnoCasa), "Sim",
                                                       ifelse(grepl("77", quintaTurnoCasa), "Sim", NA)))
pessoas$quintaTurnoCasa <- str_replace_all(pessoas$quintaTurnoCasa, c("1"="Manhã",
                                                                      "2"="Tarde",
                                                                      "3"="Noite",
                                                                      "4"="Madrugada",
                                                                      "5"="Não fica em casa nesse dia",
                                                                      "77"="Não fica em casa nesse dia",
                                                                      "88"="Não sabe",
                                                                      "99"="Não sabe/ não respondeu"))

pessoas <- mutate(pessoas, "sextaCasaManha"= ifelse(grepl("1", sextaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "sextaCasaTarde"= ifelse(grepl("2", sextaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "sextaCasaNoite"= ifelse(grepl("3", sextaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "sextaCasaMadrugada"= ifelse(grepl("4", sextaTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "sextaCasaNaoFica"= ifelse(grepl("5", sextaTurnoCasa), "Sim",
                                                      ifelse(grepl("77", sextaTurnoCasa), "Sim", NA)))
pessoas$sextaTurnoCasa <- str_replace_all(pessoas$sextaTurnoCasa, c("1"="Manhã",
                                                                    "2"="Tarde",
                                                                    "3"="Noite",
                                                                    "4"="Madrugada",
                                                                    "5"="Não fica em casa nesse dia",
                                                                    "77"="Não fica em casa nesse dia",
                                                                    "88"="Não sabe",
                                                                    "99"="Não sabe/ não respondeu"))

pessoas <- mutate(pessoas, "sabadoCasaManha"= ifelse(grepl("1", sabadoTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "sabadoCasaTarde"= ifelse(grepl("2", sabadoTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "sabadoCasaNoite"= ifelse(grepl("3", sabadoTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "sabadoCasaMadrugada"= ifelse(grepl("4", sabadoTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "sabadoCasaNaoFica"= ifelse(grepl("5", sabadoTurnoCasa), "Sim",
                                                       ifelse(grepl("77", sabadoTurnoCasa), "Sim", NA)))
pessoas$sabadoTurnoCasa <- str_replace_all(pessoas$sabadoTurnoCasa, c("1"="Manhã",
                                                                      "2"="Tarde",
                                                                      "3"="Noite",
                                                                      "4"="Madrugada",
                                                                      "5"="Não fica em casa nesse dia",
                                                                      "77"="Não fica em casa nesse dia",
                                                                      "88"="Não sabe",
                                                                      "99"="Não sabe/ não respondeu"))

pessoas <- mutate(pessoas, "domingoCasaManha"= ifelse(grepl("1", domingoTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "domingoCasaTarde"= ifelse(grepl("2", domingoTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "domingoCasaNoite"= ifelse(grepl("3", domingoTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "domingoCasaMadrugada"= ifelse(grepl("4", domingoTurnoCasa), "Sim", NA))
pessoas <- mutate(pessoas, "domingoCasaNaoFica"= ifelse(grepl("5", domingoTurnoCasa), "Sim",
                                                        ifelse(grepl("77", domingoTurnoCasa), "Sim",NA)))
pessoas$domingoTurnoCasa <- str_replace_all(pessoas$domingoTurnoCasa, c("1"="Manhã",
                                                                        "2"="Tarde",
                                                                        "3"="Noite",
                                                                        "4"="Madrugada",
                                                                        "5"="Não fica em casa nesse dia",
                                                                        "77"="Não fica em casa nesse dia",
                                                                        "88"="Não sabe",
                                                                        "99"="Não sabe/ não respondeu"))

pessoas <- add_column(pessoas, "trabalhaSegunda"="", .after = "diasTrabalho")
pessoas <- add_column(pessoas, "trabalhaTerca"="", .after = "trabalhaSegunda")
pessoas <- add_column(pessoas, "trabalhaQuarta"="", .after = "trabalhaTerca")
pessoas <- add_column(pessoas, "trabalhaQuinta"="", .after = "trabalhaQuarta")
pessoas <- add_column(pessoas, "trabalhaSexta"="", .after = "trabalhaQuinta")
pessoas <- add_column(pessoas, "trabalhaSabado"="", .after = "trabalhaSexta")
pessoas <- add_column(pessoas, "trabalhaDomingo"="", .after = "trabalhaSabado")

pessoas <- mutate(pessoas, "trabalhaSegunda" = if_else(grepl("1", diasTrabalho), "Sim", ""))
pessoas <- mutate(pessoas, "trabalhaTerca" = if_else(grepl("2", diasTrabalho), "Sim", ""))
pessoas <- mutate(pessoas, "trabalhaQuarta" = if_else(grepl("3", diasTrabalho), "Sim", ""))
pessoas <- mutate(pessoas, "trabalhaQuinta" = if_else(grepl("4", diasTrabalho), "Sim", ""))
pessoas <- mutate(pessoas, "trabalhaSexta" = if_else(grepl("5", diasTrabalho), "Sim", ""))
pessoas <- mutate(pessoas, "trabalhaSabado" = if_else(grepl("6", diasTrabalho), "Sim", ""))
pessoas <- mutate(pessoas, "trabalhaDomingo" = if_else(grepl("7", diasTrabalho), "Sim", ""))

pessoas$diasTrabalho <- str_replace_all(pessoas$diasTrabalho, c("88"="Não sabe",
                                                                "1"="Segunda",
                                                                "2"="Terça",
                                                                "3"="Quarta",
                                                                "4"="Quinta",
                                                                "5"="Sexta",
                                                                "6"="Sábado",
                                                                "7"="Domingo",
                                                                "8"="Não tem dia fixo",
                                                                "99"="Não sabe/ não respondeu"))

pessoas$brigadaIncendio <- ifelse(pessoas$brigadaIncendio == "1", "Sim", "Não")
pessoas$CIPA <- ifelse(pessoas$CIPA == "1", "Sim", "Não")

pessoas$conheceRotasFugaPE <- if_else(pessoas$conheceRotasFugaPE=="0", "Não",
                                      if_else(pessoas$conheceRotasFugaPE=="1", "Sim", "", missing = NULL))
pessoas$conheceZAS_ZSS <- if_else(pessoas$conheceZAS_ZSS=="0", "Não",
                                  if_else(pessoas$conheceZAS_ZSS=="1", "Sim", "", missing = NULL))

pessoas$responsavelInstituicao <- if_else(pessoas$responsavelInstituicao=="0", "Não",
                                          if_else(pessoas$responsavelInstituicao=="1", "Sim", "", missing = NULL))

pessoas$idSincronismo <- NULL

#Monta base de logradouros
logradouros <- merge(Filtro7, Filtro3, by="id", all.x = TRUE)
logradouros <- merge(logradouros, Filtro4, by="id", all.x = TRUE)
logradouros <- merge(logradouros, Filtro5, by="id", all.x = TRUE)
logradouros <- merge(logradouros, Filtro6, by="id", all.x = TRUE)
logradouros <- merge(logradouros, Filtro10, by="id", all.x = TRUE)
logradouros$idSincronismo <- NULL
logradouros$numeroTentativa <- as.numeric(logradouros$numeroTentativa)

logradouros$possivelEntrevista[logradouros$passivelCadastro == "Não"] <- ""
logradouros$numeroTentativa[logradouros$passivelCadastro == "Não"] <- ""
logradouros$tipoOcorrencia[logradouros$passivelCadastro == "Não"] <- ""
logradouros$ocorrenciaOutro[logradouros$passivelCadastro == "Não"] <- ""
logradouros$incapacidadeResponder[logradouros$passivelCadastro == "Não"] <- ""
logradouros$motivoNaoCadastro[logradouros$passivelCadastro == "Sim"] <- ""
logradouros$pontoEncontro[logradouros$pontoEncontro == "000" | logradouros$pontoEncontro == "00" | logradouros$pontoEncontro == "0"] <- NA

logradouros <- logradouros[c("id",
                             "cidade",
                             "logradouro",
                             "numero",
                             "complemento",
                             "bairroDistrito",
                             "localidade",
                             "observacoes",
                             "data",
                             "pontoEncontro",
                             "passivelCadastro",
                             "motivoNaoCadastro",
                             "usosPropriedade",
                             "residencia",
                             "atividadeEconomica",
                             "atividadeEconomicaTipos",
                             "industria",
                             "comercio",
                             "servico",
                             "agropecuaria",
                             "extrativismo",
                             "entidade",
                             "entidadesTipos",
                             "temploReligioso",
                             "ong",
                             "instEducacional",
                             "instSaude",
                             "instPublica",
                             "autarquia",
                             "associacao",
                             "entidadesGeral",
                             "dataSincronismo",
                             "possivelEntrevista",
                             "numeroTentativa",
                             "tipoOcorrencia",
                             "ocorrenciaOutro",
                             "incapacidadeResponder",
                             "moradiaIsolamentoAcustico",
                             "moradiaAtivSinaisSonoros",
                             "moradiaAtivSinaisSonorosQuais",
                             "familiaPossuiTransporte",
                             "carroPequenoFam",
                             "caminhoneteFam",
                             "motoFam",
                             "caminhaoFam",
                             "bicicletaFam",
                             "cavaloCarrocaFam",
                             "barcoFam",
                             "outroTranspFam",
                             "abrigoEmergencia",
                             "abrigoOutroImovel",
                             "abrigoAmigos",
                             "abrigoParentes",
                             "abrigoEmergenciaTempo",
                             "nomeEmpresa",
                             "informaTelEmpresa",
                             "telEmpresa",
                             "atividadeEmpresa",
                             "informaCNPJ",
                             "cnpj",
                             "funcionaSegundaEmpresa",
                             "funcionaTercaEmpresa",
                             "funcionaQuartaEmpresa",
                             "funcionaQuintaEmpresa",
                             "funcionaSextaEmpresa",
                             "funcionaSabadoEmpresa",
                             "funcionaDomingoEmpresa",
                             "horaFuncionaSegundaEmpresa",
                             "horaFuncionaTercaEmpresa",
                             "horaFuncionaQuartaEmpresa",
                             "horaFuncionaQuintaEmpresa",
                             "horaFuncionaSextaEmpresa",
                             "horaFuncionaSabadoEmpresa",
                             "horaFuncionaDomingoEmpresa",
                             "pessoasAtendidas",
                             "pessoasAtendidasManha",
                             "pessoasAtendidasTarde",
                             "pessoasAtendidasNoite",
                             "pessoasAtendidasMadrugada",
                             "pessoasAtendidasDeficiencia",
                             "possuiProdQuimRadioativo",
                             "volumeProdQuimRadioativo",
                             "armazenamentoProdQuimRadioativo",
                             "rampasEmpresa",
                             "escadasEmpresa",
                             "elevadoresEmpresa",
                             "edificNivelRuaEmpresa",
                             "outroAcessoEmpresa",
                             "possuiSaidaEmerg",
                             "numSaidasEmerg",
                             "possuiCIPA",
                             "sabeVencCIPA",
                             "mesVencCIPA",
                             "possuiBrigIncendio",
                             "isolamentoAcustico",
                             "ativSinaisSonoros",
                             "consultOdontologicoEmpresa",
                             "marcenariaEmpresa",
                             "constrCivilEmpresa",
                             "serralheriaEmpresa",
                             "estudioMusicalEmpresa",
                             "outrasAtivSonorasEmpresa",
                             "ativSinaisSonorosQuaisOutros",
                             "contatoEmerg",
                             "contatoEmergTel",
                             "nomeInstituicao",
                             "informaTelInstituicao",
                             "telInstituicao",
                             "atividadeInstituicao",
                             "informaCnpjInstituicao",
                             "cnpjInstituicao",
                             "diasFuncionaInstituicao",
                             "funcionaSegundaInst",
                             "funcionaTercaInst",
                             "funcionaQuartaInst",
                             "funcionaQuintaInst",
                             "funcionaSextaInst",
                             "funcionaSabadoInst",
                             "funcionaDomingoInst",
                             "horaFuncionaInstituicaoSegunda",
                             "horaFuncionaInstituicaoTerca",
                             "horaFuncionaInstituicaoQuarta",
                             "horaFuncionaInstituicaoQuinta",
                             "horaFuncionaInstituicaoSexta",
                             "horaFuncionaInstituicaoSabado",
                             "horaFuncionaInstituicaoDomingo",
                             "alunosFreqEscolaManha",
                             "alunosFreqEscolaTarde",
                             "alunosFreqEscolaNoite",
                             "alunosDeficiencia",
                             "alunosDeficienciaQuais",
                             "alunosFreqInstituicaoDeficienciaFisicaManha",
                             "alunosFreqInstituicaoDeficienciaFisicaTarde",
                             "alunosFreqInstituicaoDeficienciaFisicaNoite",
                             "alunosFreqInstituicaoDeficienciaAuditivaManha",
                             "alunosFreqInstituicaoDeficienciaAuditivaTarde",
                             "alunosFreqInstituicaoDeficienciaAuditivaNoite",
                             "alunosFreqInstituicaoDeficienciaVisualManha",
                             "alunosFreqInstituicaoDeficienciaVisualTarde",
                             "alunosFreqInstituicaoDeficienciaVisualNoite",
                             "alunosFreqInstituicaoDeficienciaMentalManha",
                             "alunosFreqInstituicaoDeficienciaMentalTarde",
                             "alunosFreqInstituicaoDeficienciaMentalNoite",
                             "alunosFreqInstituicaoDeficienciaFalaManha",
                             "alunosFreqInstituicaoDeficienciaFalaTarde",
                             "alunosFreqInstituicaoDeficienciaFalaNoite",
                             "alunosFreqInstituicaoDificuldadeLocomocao",
                             "alunosFreqInstituicaoDificuldadeLocomocaoTipo",
                             "alunosFreqInstituicaoDificuldadeCaminharManha",
                             "alunosFreqInstituicaoDificuldadeCaminharTarde",
                             "alunosFreqInstituicaoDificuldadeCaminharNoite",
                             "alunosFreqInstituicaoAcamadasManha",
                             "alunosFreqInstituicaoAcamadasTarde",
                             "alunosFreqInstituicaoAcamadasNoite",
                             "alunosFreqInstituicaoCadeirantesManha",
                             "alunosFreqInstituicaoCadeirantesTarde",
                             "alunosFreqInstituicaoCadeirantesNoite",
                             "alunosFreqInstituicaoBengalaManha",
                             "alunosFreqInstituicaoBengalaTarde",
                             "alunosFreqInstituicaoBengalaNoite",
                             "alunosFreqInstituicaoMembroAmputadoManha",
                             "alunosFreqInstituicaoMembroAmputadoTarde",
                             "alunosFreqInstituicaoMembroAmputadoNoite",
                             "alunosFreqInstituicaoProblRespitarioLocomManha",
                             "alunosFreqInstituicaoProblRespitarioLocomTarde",
                             "alunosFreqInstituicaoProblRespitarioLocomNoite",
                             "alunosFreqInstituicaoPosOperatorioManha",
                             "alunosFreqInstituicaoPosOperatorioTarde",
                             "alunosFreqInstituicaoPosOperatorioNoite",
                             "alunosFreqInstituicaoProteseManha",
                             "alunosFreqInstituicaoProteseTarde",
                             "alunosFreqInstituicaoProteseNoite",
                             "alunosFreqInstituicaoProblColunaColomManha",
                             "alunosFreqInstituicaoProblColunaColomTarde",
                             "alunosFreqInstituicaoProblColunaColomNoite",
                             "alunosFreqInstituicaoObesasManha",
                             "alunosFreqInstituicaoObesasTarde",
                             "alunosFreqInstituicaoObesasNoite",
                             "alunosFreqInstituicaoSeqAcidenteManha",
                             "alunosFreqInstituicaoSeqAcidenteTarde",
                             "alunosFreqInstituicaoSeqAcidenteNoite",
                             "alunosFreqInstituicaoIdadeAvancadaManha",
                             "alunosFreqInstituicaoIdadeAvancadaTarde",
                             "alunosFreqInstituicaoIdadeAvancadaNoite",
                             "alunosFreqInstituicaoCriancaColoManha",
                             "alunosFreqInstituicaoCriancaColoTarde",
                             "alunosFreqInstituicaoCriancaColoNoite",
                             "alunosFreqInstituicaoOutraDificLocomManha",
                             "alunosFreqInstituicaoOutraDificLocomTarde",
                             "alunosFreqInstituicaoOutraDificLocomNoite",
                             "pessoasFreqInstituicaoSegundaManha",
                             "pessoasFreqInstituicaoSegundaTarde",
                             "pessoasFreqInstituicaoSegundaNoite",
                             "pessoasFreqInstituicaoTercaManha",
                             "pessoasFreqInstituicaoTercaTarde",
                             "pessoasFreqInstituicaoTercaNoite",
                             "pessoasFreqInstituicaoQuartaManha",
                             "pessoasFreqInstituicaoQuartaTarde",
                             "pessoasFreqInstituicaoQuartaNoite",
                             "pessoasFreqInstituicaoQuintaManha",
                             "pessoasFreqInstituicaoQuintaTarde",
                             "pessoasFreqInstituicaoQuintaNoite",
                             "pessoasFreqInstituicaoSextaManha",
                             "pessoasFreqInstituicaoSextaTarde",
                             "pessoasFreqInstituicaoSextaNoite",
                             "pessoasFreqInstituicaoSabadoManha",
                             "pessoasFreqInstituicaoSabadoTarde",
                             "pessoasFreqInstituicaoSabadoNoite",
                             "pessoasFreqInstituicaoDomingoManha",
                             "pessoasFreqInstituicaoDomingoTarde",
                             "pessoasFreqInstituicaoDomingoNoite",
                             "pessoasFreqInstituicaoDeficiencia",
                             "pessoasFreqInstituicaoDeficienciaTipo",
                             "pessoasFreqInstituicaoDeficienciaFisicaManha",
                             "pessoasFreqInstituicaoDeficienciaFisicaTarde",
                             "pessoasFreqInstituicaoDeficienciaFisicaNoite",
                             "pessoasFreqInstituicaoDeficienciaAuditivaManha",
                             "pessoasFreqInstituicaoDeficienciaAuditivaTarde",
                             "pessoasFreqInstituicaoDeficienciaAuditivaNoite",
                             "pessoasFreqInstituicaoDeficienciaVisualManha",
                             "pessoasFreqInstituicaoDeficienciaVisualTarde",
                             "pessoasFreqInstituicaoDeficienciaVisualNoite",
                             "pessoasFreqInstituicaoDeficienciaMentalManha",
                             "pessoasFreqInstituicaoDeficienciaMentalTarde",
                             "pessoasFreqInstituicaoDeficienciaMentalNoite",
                             "pessoasFreqInstituicaoDeficienciaFalaManha",
                             "pessoasFreqInstituicaoDeficienciaFalaTarde",
                             "pessoasFreqInstituicaoDeficienciaFalaNoite",
                             "pessoasFreqInstituicaoDificuldadeLocomocao",
                             "pessoasFreqInstituicaoDificuldadeLocomocaoTipo",
                             "pessoasFreqInstituicaoDificuldadeCaminharManha",
                             "pessoasFreqInstituicaoDificuldadeCaminharTarde",
                             "pessoasFreqInstituicaoDificuldadeCaminharNoite",
                             "pessoasFreqInstituicaoAcamadasManha",
                             "pessoasFreqInstituicaoAcamadasTarde",
                             "pessoasFreqInstituicaoAcamadasNoite",
                             "pessoasFreqInstituicaoCadeirantesManha",
                             "pessoasFreqInstituicaoCadeirantesTarde",
                             "pessoasFreqInstituicaoCadeirantesNoite",
                             "pessoasFreqInstituicaoBengalaManha",
                             "pessoasFreqInstituicaoBengalaTarde",
                             "pessoasFreqInstituicaoBengalaNoite",
                             "pessoasFreqInstituicaoMembroAmputadoManha",
                             "pessoasFreqInstituicaoMembroAmputadoTarde",
                             "pessoasFreqInstituicaoMembroAmputadoNoite",
                             "pessoasFreqInstituicaoProblRespitarioLocomManha",
                             "pessoasFreqInstituicaoProblRespitarioLocomTarde",
                             "pessoasFreqInstituicaoProblRespitarioLocomNoite",
                             "pessoasFreqInstituicaoPosOperatorioManha",
                             "pessoasFreqInstituicaoPosOperatorioTarde",
                             "pessoasFreqInstituicaoPosOperatorioNoite",
                             "pessoasFreqInstituicaoProteseManha",
                             "pessoasFreqInstituicaoProteseTarde",
                             "pessoasFreqInstituicaoProteseNoite",
                             "pessoasFreqInstituicaoProblColunaColomManha",
                             "pessoasFreqInstituicaoProblColunaColomTarde",
                             "pessoasFreqInstituicaoProblColunaColomNoite",
                             "pessoasFreqInstituicaoObesasManha",
                             "pessoasFreqInstituicaoObesasNoite",
                             "pessoasFreqInstituicaoSeqAcidenteManha",
                             "pessoasFreqInstituicaoSeqAcidenteTarde",
                             "pessoasFreqInstituicaoSeqAcidenteNoite",
                             "pessoasFreqInstituicaoIdadeAvancadaManha",
                             "pessoasFreqInstituicaoIdadeAvancadaTarde",
                             "pessoasFreqInstituicaoIdadeAvancadaNoite",
                             "pessoasFreqInstituicaoCriancaColoManha",
                             "pessoasFreqInstituicaoCriancaColoTarde",
                             "pessoasFreqInstituicaoCriancaColoNoite",
                             "pessoasFreqInstituicaoOutraDificLocomManha",
                             "pessoasFreqInstituicaoOutraDificLocomTarde",
                             "pessoasFreqInstituicaoOutraDificLocomNoite",
                             "pessoasFreqInstituicaoSemanaTotal",
                             "publicoExternoAtendidoManha",
                             "publicoExternoAtendidoTarde",
                             "publicoExternoAtendidoNoite",
                             "publicoExternoAtendidoMadrugada",
                             "pessoasAtendDeficienciaDificLocomocaoSemana",
                             "possuiProdQuimRadioativoInstituicao",
                             "possuiProdQuimRadioativoInstituicaoQuais",
                             "volumeProdQuimRadioativoInstituicao",
                             "armazenamentoProdQuimRadioativoInstituicao",
                             "acessoEdificacaoInstituicao",
                             "rampasInst",
                             "escadasInst",
                             "elevadoresInst",
                             "edificNivelRuaInst",
                             "outroAcessoInst",
                             "possuiSaidaEmergInstituicao",
                             "numSaidasEmergInstituicao",
                             "possuiBrigIncendioInstituicao",
                             "isolamentoAcusticoInstituicao",
                             "ativSinaisSonorosQuaisInstituicao",
                             "ativSinaisSonorosQuaisOutrosInstituicao",
                             "contatoEmergInstituicao",
                             "contatoEmergTelInstituicao",
                             "capacidadeMaxima",
                             "anoVencCIPA")]


#Força alterações específicas
pessoasPropriedade <- Filtro3 %>% select(id, residencia, atividadeEconomica, entidade)
pessoas <- merge(pessoas, pessoasPropriedade, by="id", all.x = TRUE)

pessoas$trabalha[((pessoas$atividadeEconomica=="Sim") |
                    (pessoas$entidade=="Sim")) &
                   (pessoas$residencia=="Não")] <- "Sim"

pessoas$trabalha[((pessoas$atividadeEconomica=="Não") &
                    (pessoas$entidade=="Não")) &
                   (pessoas$residencia=="Sim")] <- "Não"


pessoas$trabalha[(pessoas$relacaoResponsavelDomicilio == "Empregado(a)")] <- "Sim"

pessoas$trabalha[(pessoas$cargoInstituicao == "Proprietário/Sócio") | 
                   (pessoas$cargoInstituicao == "Gerente/Diretor de área") |
                   (pessoas$cargoInstituicao == "Funcionário") | 
                   (pessoas$cargoInstituicao == "Voluntário") |
                   (pessoas$cargoInstituicao == "Outro")] <- "Sim"

pessoas$mora[pessoas$residencia=="Não"] <- "Não"
pessoas <- pessoas %>% filter(nome != "")



# Monta base de animais
animais <- merge(Filtro8, Filtro7, by="id", all.x = TRUE)
animais <- animais %>% select("id",
                              "especieAnimal",
                              "nomeAnimal",
                              "porteAnimal",
                              "racaAnimal",
                              "corAnimal",
                              "sexoAnimal",
                              "donoAnimal",
                              "cidade")


# Remove filtros do ambiente
rm("Filtro3","Filtro4","Filtro5","Filtro6","Filtro7","Filtro8","Filtro10","Filtro11","Filtro13","pessoasPropriedade")





