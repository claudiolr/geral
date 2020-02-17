library(odbc)
library(RODBC)
library(DBI)


con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "gaia.erpsa.com.br,62328",
                 Database = "ERPSA_VALE_HEP",
                 UID = "HEP_claudio.resende",
                 PWD = rstudioapi::askForPassword("HEP_claudio.resende@123"))

#Encerra a conexão e fecha o banco
odbcCloseAll()


animais <- dplyr::tbl(con,"F008_Animais") %>% 
     dplyr::select(idFormularioResposta, especieAnimal, qualPorteOuTamanhoAnimal)
animais <- dplyr::as_tibble(animais)

