library(ffbase)
library(data.table)
library(dplyr)

#ctrl + shift + h: seleciona o diretorio
setwd("D:/INEP/microdados_ed_basica_2018/microdados_ed_basica_2018/DADOS")

docentesSE <- fread(file="DOCENTES_SUDESTE.csv",sep="|",header = TRUE,
                    verbose=TRUE, data.table = FALSE,
                    select = c("ID_DOCENTE","IN_LICENCIATURA_1","ID_TURMA","IN_REGULAR","TP_ETAPA_ENSINO",
                               "CO_ENTIDADE","CO_REGIAO","CO_UF","TP_DEPENDENCIA"))

docentesNE <- fread(file="DOCENTES_NORDESTE.csv",sep="|",header = TRUE,
                    verbose=TRUE, data.table = FALSE,
                    select = c("ID_DOCENTE","IN_LICENCIATURA_1","ID_TURMA","IN_REGULAR","TP_ETAPA_ENSINO",
                               "CO_ENTIDADE","CO_REGIAO","CO_UF","TP_DEPENDENCIA"))

docentesN <- fread(file="DOCENTES_NORTE.csv",sep="|",header = TRUE,
                    verbose=TRUE, data.table = FALSE,
                    select = c("ID_DOCENTE","IN_LICENCIATURA_1","ID_TURMA","IN_REGULAR","TP_ETAPA_ENSINO",
                               "CO_ENTIDADE","CO_REGIAO","CO_UF","TP_DEPENDENCIA"))

docentesCO <- fread(file="DOCENTES_CO.csv",sep="|",header = TRUE,
                   verbose=TRUE, data.table = FALSE,
                   select = c("ID_DOCENTE","IN_LICENCIATURA_1","ID_TURMA","IN_REGULAR","TP_ETAPA_ENSINO",
                              "CO_ENTIDADE","CO_REGIAO","CO_UF","TP_DEPENDENCIA"))

docentesS <- fread(file="DOCENTES_SUL.csv",sep="|",header = TRUE,
                   verbose=TRUE, data.table = FALSE,
                   select = c("ID_DOCENTE","IN_LICENCIATURA_1","ID_TURMA","IN_REGULAR","TP_ETAPA_ENSINO",
                              "CO_ENTIDADE","CO_REGIAO","CO_UF","TP_DEPENDENCIA"))

docentes <- rbind(docentesCO,docentesN,docentesNE,docentesS,docentesSE)

docentes <- fread(file = "docentes.csv",sep = "|", header = TRUE,
                  data.table = FALSE)

write.table(docentes, file="docentes.csv", sep="|", quote = TRUE, na="", row.names = FALSE,
          col.names = TRUE)

colnames(docentes) = c("ID_DOCENTE","IN_LICENCIATURA_1","ID_TURMA","IN_REGULAR","TP_ETAPA_ENSINO",
                              "CO_ENTIDADE","CO_REGIAO","CO_UF","TP_DEPENDENCIA")

#docentes <- docentes %>% filter(!duplicated(ID_DOCENTE))


#Sudeste
matriculaSE <- fread(file="MATRICULA_SUDESTE.csv",sep="|",header = TRUE,
                    verbose=TRUE, data.table = FALSE,
                    select = c("ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                               "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE"))

matriculaSE <- matriculaSE %>%
     filter(TP_ETAPA_ENSINO == 25 | TP_ETAPA_ENSINO == 30 | TP_ETAPA_ENSINO == 35)

write.table(matriculaSE, file="matriculaSE_1EM.csv", sep="|", quote=TRUE, na="", col.names=TRUE)


#Nordeste
matriculaNE <- fread(file="MATRICULA_NORDESTE.csv",sep="|",header = TRUE,
                     verbose=TRUE, data.table = FALSE,
                     select = c("ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                                "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE"))

matriculaNE <- matriculaNE %>%
     filter(TP_ETAPA_ENSINO == 25 | TP_ETAPA_ENSINO == 30 | TP_ETAPA_ENSINO == 35)

write.table(matriculaNE, file="matriculaNE_1EM.csv", sep="|", quote=TRUE, na="", col.names=TRUE)


#Norte
matriculaN <- fread(file="MATRICULA_NORTE.csv",sep="|",header = TRUE,
                     verbose=TRUE, data.table = FALSE,
                     select = c("ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                                "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE"))

matriculaN <- matriculaN %>%
     filter(TP_ETAPA_ENSINO == 25 | TP_ETAPA_ENSINO == 30 | TP_ETAPA_ENSINO == 35)

write.table(matriculaN, file="matriculaN_1EM.csv", sep="|", quote=TRUE, na="", col.names=TRUE)


#Centro-Oeste
matriculaCO <- fread(file="MATRICULA_CO.csv",sep="|",header = TRUE,
                     verbose=TRUE, data.table = FALSE,
                     select = c("ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                                "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE"))

matriculaCO <- matriculaCO %>%
     filter(TP_ETAPA_ENSINO == 25 | TP_ETAPA_ENSINO == 30 | TP_ETAPA_ENSINO == 35)

write.table(matriculaCO, file="matriculaCO_1EM.csv", sep="|", quote=TRUE, na="", col.names=TRUE)


#Sul
matriculaS <- fread(file="MATRICULA_SUL.csv",sep="|",header = TRUE,
                     verbose=TRUE, data.table = FALSE,
                     select = c("ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                                "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE"))

matriculaS <- matriculaS %>%
     filter(TP_ETAPA_ENSINO == 25 | TP_ETAPA_ENSINO == 30 | TP_ETAPA_ENSINO == 35)

write.table(matriculaS, file="matriculaS_1EM.csv", sep="|", quote=TRUE, na="", col.names=TRUE)



#matriculaS <- fread(file="matriculas.csv",sep="|",header = TRUE,
#                    verbose=TRUE, data.table = FALSE)

#matriculas <- rbind(matriculaCO,matriculaN,matriculaNE,matriculaS,matriculaSE)

#write.table(matriculas, file="matriculas.csv", sep="|", quote = TRUE, na="",
#            col.names = TRUE)

matriculaSE_1EM <- fread(file="matriculaSE_1EM.csv",sep="|",header = TRUE,
                    verbose=TRUE, data.table = FALSE)

matriculaNE_1EM <- fread(file="matriculaNE_1EM.csv",sep="|",header = TRUE,
                     verbose=TRUE, data.table = FALSE)

matriculaN_1EM <- fread(file="matriculaN_1EM.csv",sep="|",header = TRUE,
                     verbose=TRUE, data.table = FALSE)

matriculaCO_1EM <- fread(file="matriculaCO_1EM.csv",sep="|",header = TRUE,
                     verbose=TRUE, data.table = FALSE)

matriculaS_1EM <- fread(file="matriculaS_1EM.csv",sep="|",header = TRUE,
                     verbose=TRUE, data.table = FALSE)

colnames(matriculaCO_1EM) = c("id","ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                              "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE")

colnames(matriculaN_1EM) = c("id","ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                             "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE")

colnames(matriculaNE_1EM) = c("id","ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                              "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE")

colnames(matriculaS_1EM) = c("id","ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                             "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE")

colnames(matriculaSE_1EM) = c("id","ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                              "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE")


matricula_1EM <- rbind(matriculaSE_1EM,matriculaNE_1EM,matriculaN_1EM,matriculaCO_1EM,matriculaS_1EM)

write.table(matricula_1EM,file="matricula_1EM.csv",sep="|", quote=TRUE, na="",col.names=TRUE, row.names = FALSE)

names(matriculaSE_1EM)

matricula_1EM <- fread(file="matricula_1EM.csv",sep="|",header = TRUE,
                   verbose=TRUE, data.table = FALSE)
colnames(matricula_1EM) = c("id","ID_ALUNO","ID_MATRICULA","NU_IDADE","TP_SEXO","TP_COR_RACA",
                              "TP_ETAPA_ENSINO","ID_TURMA","CO_ENTIDADE")



