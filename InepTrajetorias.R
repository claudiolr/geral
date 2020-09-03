library(data.table)
library(tidyverse)
require(janitor)



# Informe a pasta onde estão as bases do INEP
pasta <- "D:/CienciaDados/Bases/INEP/CensoEdBasica/"


# Escolha as etapas de ensinos (série/ano) usadas como ponto de partida
etapaEnsino <- c(6)


# Defina as variáveis que serão selecionadas (atente para as diferenças de nomenclatura a partir de2015)
variaveis2007 <- c("ANO_CENSO", "FK_COD_ALUNO", "FK_COD_ETAPA_ENSINO")
variaveis2015 <- c("NU_ANO_CENSO", "CO_PESSOA_FISICA", "TP_ETAPA_ENSINO")



# Carrega base de 2007 ----------------------------------------------------
ano <- 2007
caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")



# carrega a base
sudeste2007 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     # seleciona as variáveis escolhidas
                     select = variaveis2007) %>% 
        # Filtro pelo ano escolar (etapaEnsino, definida no início do código)
        filter(FK_COD_ETAPA_ENSINO == etapaEnsino)


nordeste2007 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2007) %>% 
        filter(FK_COD_ETAPA_ENSINO == etapaEnsino)


centrooeste2007 <- fread(file = paste0(caminho, "co.csv"),
                       sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                       select = variaveis2007) %>% 
        filter(FK_COD_ETAPA_ENSINO == etapaEnsino)


norte2007 <- fread(file = paste0(caminho, "NORTE.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2007) %>% 
        filter(FK_COD_ETAPA_ENSINO == etapaEnsino)


sul2007 <- fread(file = paste0(caminho, "SUL.csv"),
               sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
               select = variaveis2007) %>% 
        filter(FK_COD_ETAPA_ENSINO == etapaEnsino)


# Empilha bases regionais
brasil2007 <- rbind(sudeste2007, nordeste2007, norte2007, sul2007, centrooeste2007)


# Renomeia colunas e elimina alunos duplicados
base <- brasil2007 %>% 
        rename(ano = ANO_CENSO,
               codAluno = FK_COD_ALUNO,
               etapaEnsino = FK_COD_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


# Remove objetos do ambiente
rm(brasil2007, sudeste2007, nordeste2007, norte2007, sul2007, centrooeste2007)


# Limpa a memória ram
gc()



# Carrega base de 2008 ----------------------------------------------------

ano <- 2008
caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")

# carrega a base
sudeste2008 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     # seleciona as variáveis
                     select = variaveis2007) %>%
        # seleciona apenas os estudantes filtrados em 2007
        filter(FK_COD_ALUNO %in% base$codAluno)


nordeste2008 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


centrooeste2008 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


norte2008 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


sul2008 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


brasil2008 <- rbind(sudeste2008, nordeste2008, norte2008, sul2008, centrooeste2008)


brasil2008 <- brasil2008 %>% 
        rename(ano = ANO_CENSO,
               codAluno = FK_COD_ALUNO,
               etapaEnsino = FK_COD_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2008, nordeste2008, norte2008, sul2008, centrooeste2008)
gc()



# Cola 2008 na base -------------------------------------------------------

base <- rbind(base, brasil2008)
rm(brasil2008)
gc()



# Carrega base de 2009 ----------------------------------------------------

ano <- 2009


caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")


sudeste2009 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


nordeste2009 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


centrooeste2009 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


norte2009 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


sul2009 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


brasil2009 <- rbind(sudeste2009, nordeste2009, norte2009, sul2009, centrooeste2009)


brasil2009 <- brasil2009 %>% 
        rename(ano = ANO_CENSO,
               codAluno = FK_COD_ALUNO,
               etapaEnsino = FK_COD_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2009, nordeste2009, norte2009, sul2009, centrooeste2009)
gc()



# Cola 2009 na base -------------------------------------------------------

base <- rbind(base, brasil2009)
rm(brasil2009)
gc()


# Carrega base de 2010 ----------------------------------------------------

ano <- 2010


caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")


sudeste2010 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


nordeste2010 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


centrooeste2010 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


norte2010 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


sul2010 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


brasil2010 <- rbind(sudeste2010, nordeste2010, norte2010, sul2010, centrooeste2010)


brasil2010 <- brasil2010 %>% 
        rename(ano = ANO_CENSO,
               codAluno = FK_COD_ALUNO,
               etapaEnsino = FK_COD_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2010, nordeste2010, norte2010, sul2010, centrooeste2010)
gc()


# Cola 2010 na base -------------------------------------------------------

base <- rbind(base, brasil2010)
rm(brasil2010)
gc()



# Carrega base de 2011 ----------------------------------------------------

ano <- 2011


caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")



sudeste2011 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


nordeste2011 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


centrooeste2011 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


norte2011 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


sul2011 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


brasil2011 <- rbind(sudeste2011, nordeste2011, norte2011, sul2011, centrooeste2011)


brasil2011 <- brasil2011 %>% 
        rename(ano = ANO_CENSO,
               codAluno = FK_COD_ALUNO,
               etapaEnsino = FK_COD_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2011, nordeste2011, norte2011, sul2011, centrooeste2011)
gc()


# Cola 2011 na base -------------------------------------------------------

base <- rbind(base, brasil2011)
rm(brasil2011)
gc()



# Carrega base de 2012 ----------------------------------------------------

ano <- 2012


caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")


sudeste2012 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


nordeste2012 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


centrooeste2012 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


norte2012 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


sul2012 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


brasil2012 <- rbind(sudeste2012, nordeste2012, norte2012, sul2012, centrooeste2012)


brasil2012 <- brasil2012 %>% 
        rename(ano = ANO_CENSO,
               codAluno = FK_COD_ALUNO,
               etapaEnsino = FK_COD_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2012, nordeste2012, norte2012, sul2012, centrooeste2012)
gc()


# Cola 2012 na base -------------------------------------------------------

base <- rbind(base, brasil2012)
rm(brasil2012)
gc()



# Carrega base de 2013 ----------------------------------------------------

ano <- 2013


caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")


sudeste2013 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


nordeste2013 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


centrooeste2013 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


norte2013 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


sul2013 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


brasil2013 <- rbind(sudeste2013, nordeste2013, norte2013, sul2013, centrooeste2013)


brasil2013 <- brasil2013 %>% 
        rename(ano = ANO_CENSO,
               codAluno = FK_COD_ALUNO,
               etapaEnsino = FK_COD_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2013, nordeste2013, norte2013, sul2013, centrooeste2013)
gc()


# Cola 2013 na base -------------------------------------------------------

base <- rbind(base, brasil2013)
rm(brasil2013)
gc()



# Carrega base de 2014 ----------------------------------------------------

ano <- 2014


caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")


sudeste2014 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


nordeste2014 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


centrooeste2014 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


norte2014 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


sul2014 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2007) %>% 
        filter(FK_COD_ALUNO %in% base$codAluno)


brasil2014 <- rbind(sudeste2014, nordeste2014, norte2014, sul2014, centrooeste2014)


brasil2014 <- brasil2014 %>% 
        rename(ano = ANO_CENSO,
               codAluno = FK_COD_ALUNO,
               etapaEnsino = FK_COD_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2014, nordeste2014, norte2014, sul2014, centrooeste2014)
gc()


# Cola 2014 na base -------------------------------------------------------

base <- rbind(base, brasil2014)
rm(brasil2014)
gc()



# Carrega base de 2015 ----------------------------------------------------

ano <- 2015


caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")


sudeste2015 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


nordeste2015 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


centrooeste2015 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


norte2015 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


sul2015 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


brasil2015 <- rbind(sudeste2015, nordeste2015, norte2015, sul2015, centrooeste2015)


brasil2015 <- brasil2015 %>% 
        rename(ano = NU_ANO_CENSO,
               codAluno = CO_PESSOA_FISICA,
               etapaEnsino = TP_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2015, nordeste2015, norte2015, sul2015, centrooeste2015)
gc()


# Cola 2015 na base -------------------------------------------------------

base <- rbind(base, brasil2015)
rm(brasil2015)
gc()



# Carrega base de 2016 ----------------------------------------------------

ano <- 2016


caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")


sudeste2016 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


nordeste2016 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


centrooeste2016 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


norte2016 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


sul2016 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


brasil2016 <- rbind(sudeste2016, nordeste2016, norte2016, sul2016, centrooeste2016)


brasil2016 <- brasil2016 %>% 
        rename(ano = NU_ANO_CENSO,
               codAluno = CO_PESSOA_FISICA,
               etapaEnsino = TP_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2016, nordeste2016, norte2016, sul2016, centrooeste2016)
gc()


# Cola 2016 na base -------------------------------------------------------

base <- rbind(base, brasil2016)
rm(brasil2016)
gc()



# Carrega base de 2017 ----------------------------------------------------

ano <- 2017


caminho = paste0(pasta, ano, "/DADOS/MATRICULA_")


sudeste2017 <- fread(file = paste0(caminho, "SUDESTE.csv"),
                     sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                     select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


nordeste2017 <- fread(file = paste0(caminho, "NORDESTE.csv"),
                      sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                      select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


centrooeste2017 <- fread(file = paste0(caminho, "co.csv"),
                         sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                         select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


norte2017 <- fread(file = paste0(caminho, "NORTE.csv"),
                   sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                   select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


sul2017 <- fread(file = paste0(caminho, "SUL.csv"),
                 sep="|", header = TRUE, verbose=TRUE, data.table = FALSE,
                 select = variaveis2015) %>% 
        filter(CO_PESSOA_FISICA %in% base$codAluno)


brasil2017 <- rbind(sudeste2017, nordeste2017, norte2017, sul2017, centrooeste2017)


brasil2017 <- brasil2017 %>% 
        rename(ano = NU_ANO_CENSO,
               codAluno = CO_PESSOA_FISICA,
               etapaEnsino = TP_ETAPA_ENSINO) %>% 
        filter(!duplicated(codAluno))


rm(sudeste2017, nordeste2017, norte2017, sul2017, centrooeste2017)
gc()


# Cola 2017 na base -------------------------------------------------------

base <- rbind(base, brasil2017)
rm(brasil2017)
gc()



write_delim(base, "D:/CienciaDados/Bases/INEP/baseLong.csv", delim = ";", col_names = TRUE)



# Cria base ampla ---------------------------------------------------------

baseAmpla <- base %>% 
        pivot_wider(names_from = ano, values_from = etapaEnsino)




# Análises ----------------------------------------------------------------

base %>% 
        group_by(ano) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/2249788, digits = 3)*100) %>% 
        ggplot(aes(x = ano, y = N, fill = ano, label = Prop)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = Prop), vjust = -0.3)



base %>%
        mutate(TipoEnsino = case_when(etapaEnsino %in% seq(6, 33) ~ "Ensino Regular",
                                      etapaEnsino %in% seq(43, 73) ~ "EJA",
                                      TRUE ~ "Outros")) %>% 
        group_by(ano, TipoEnsino) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/2249788, digits = 3)*100) %>% 
        ggplot(aes(x = ano, y = N, fill = TipoEnsino, label = Prop)) +
        geom_bar(stat = "identity")



base %>%
        filter(!is.na(etapaEnsino)) %>% 
        mutate(TipoEnsino = case_when(etapaEnsino %in% seq(6, 33) ~ "Ensino Regular",
                                      etapaEnsino %in% seq(43, 73) ~ "EJA",
                                      TRUE ~ "Outros")) %>% 
        filter(TipoEnsino == "Ensino Regular") %>% 
        group_by(ano, TipoEnsino) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/2249788, digits = 3)*100) %>% 
        ggplot(aes(x = ano, y = N, fill = TipoEnsino, label = Prop)) +
        geom_bar(stat = "identity")



base %>% distinct(etapaEnsino)


colnames(docentes) = c("ID_DOCENTE","IN_LICENCIATURA_1","ID_TURMA","IN_REGULAR","TP_ETAPA_ENSINO",
                       "CO_ENTIDADE","CO_REGIAO","CO_UF","TP_DEPENDENCIA")

