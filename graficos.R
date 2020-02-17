library(tidyverse)
library(ggplot2)
library(RColorBrewer)


###########################################################################
# Informarções do pacote RColorBrewer -------------------------------------
###########################################################################
RColorBrewer::display.brewer.all()
brewer.pal.info



###########################################################################
# Escala Likert -----------------------------------------------------------
###########################################################################
legConcordancia <- c("Discorda muito","Discorda",
                     paste("Não discorda","\n","nem concorda"),
                     "Concorda","Concorda muito")
legFrequencia <- c("Nunca","Quase nunca","Às vezes","Quase sempre","Sempre")



###########################################################################
# Tratamento da base ------------------------------------------------------
###########################################################################

setwd("C:/Users/Claudio/OneDrive/CienciaDados")
base <- read.csv("wiki4HE.csv", sep=";")


### Substitui não-respostas ("?") por NA
base$DOMAIN <- factor(base$DOMAIN, levels = c("1","2","3","4","5","6"))
base$USERWIKI <- factor(base$USERWIKI, levels = c("0", "1"))
base$ENJ1 <- factor(base$ENJ1, levels = c("1","2","3","4","5"))
base$ENJ2 <- factor(base$ENJ2, levels = c("1","2","3","4","5"))
base$YEARSEXP <- factor(base$YEARSEXP, levels = seq(0:43))


### Transforma variáveis categóricas (factor) em numéricas
base$DOMAIN <- as.numeric(base$DOMAIN)

base$USERWIKI <- as.numeric(base$USERWIKI)
base$USERWIKI <- ifelse(base$USERWIKI==1, 0, 1)

base$YEARSEXP <- as.numeric(base$YEARSEXP)
base$ENJ1 <- as.numeric(base$ENJ1)
base$ENJ2 <- as.numeric(base$ENJ2)



###########################################################################
# Cria índices de respostas -----------------------------------------------
###########################################################################

## Cria índice de Percepção de prazer pelo uso da Wikipedia (ENJ)
base$ENJ <- (base$ENJ1 + base$ENJ2)/2



###########################################################################
# Análises ----------------------------------------------------------------
###########################################################################



plot(table(base$USERWIKI, base$GENDER), col = brewer.pal(8, "Blues")[5:8])
barplot(table(base$PU1), space = 0.02, border = NA,
        col = brewer.pal(8, "Blues")[4:8],
        args.legend = list(x = "topleft"),
        names.arg = legConcordancia,
        main = paste("O uso da Wikipedia facilita para os estudantes", "\n","o desenvolvimento de novas habilidades"), font = 1,
        las = 1)

barplot(table(base$PU2), space = 0.02, border = NA,
        col = brewer.pal(8, "Greens")[4:8],
        args.legend = list(x = "topleft"),
        names.arg = legConcordancia,
        main = paste("O uso da Wikipedia aprimora", "\n","o aprendizado dos estudantes"), font = 1,
        las = 1)

barplot(table(base$PU3), space = 0.02, border = NA,
        col = brewer.pal(8, "Reds")[4:8],
        args.legend = list(x = "topleft"),
        names.arg = legConcordancia,
        main = paste("A Wikipedia é útil para o ensino"), font = 1,
        las = 1)