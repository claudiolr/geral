library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(MASS)
library(foreign)

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


### Descrição

# Gênero
ggplot(base, aes(x = as.factor(GENDER), fill=as.factor(GENDER))) + 
     geom_bar() + 
     labs(title = "Gênero dos professores entrevistados",
          x = "", y = NULL, size = 8) + 
     scale_fill_manual(values = c("#F57F17", "#BF360C"), labels = c("Masculino","Feminino")) +
     geom_text(aes(label=..count..), stat = "count", position=position_stack(0.9), color = "black", size = 5) +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14)) +
     scale_x_discrete(labels = NULL)


# Perfil de uso
ggplot(base, aes(x = as.factor(USERWIKI), fill = as.factor(USERWIKI))) + 
     geom_bar(na.rm = TRUE) + 
     labs(title = "Perfil de uso da Wikipedia",
          x = "", y = NULL) + 
     scale_fill_manual(values = c("#2C3E50", "#16A085"), labels = c("Não usuário","Usuário")) +
     geom_text(aes(label=..count..),stat = "count", position=position_stack(0.9), color = "black", size = 5) +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14)) +
     scale_x_discrete(labels = NULL)


# Idade por gênero
ggplot(data = base, mapping = aes(x = AGE, fill = as.factor(GENDER))) +
     geom_bar() +
     labs(title = "Idade dos professores entrevistados, por gênero",
          x = "Idade", y = NULL) +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14)) +
     scale_fill_manual(values = c("#F57F17", "#BF360C"), labels = c("Masculino","Feminino"))


# Área de atuação
ggplot(base, aes(x = DOMAIN, fill = as.factor(DOMAIN))) +
     geom_bar(stat = "count", position = 'dodge', na.rm = TRUE) +
     geom_text(aes(label = ..count..), stat = "count", position = position_stack(0.8), color = "black", size = 5) +
     labs(title = "Área de atuação dos professores",
          x = "Área", y = NULL) +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14)) +
     scale_fill_manual(values = c("#004D40","#00695C","#00796B","#00897B","#2980B9","#3498DB"),
                       labels = c("Artes & Humanidades", "Ciências",
                                  "Ciências da Saúde", "Engenharia & Arquitetura","Direito & Política", "Outra"))+
     scale_x_discrete(labels = NULL)


### Satisfação percebida
# Usuário
g1 <- ggplot(base, mapping = aes(x = ENJ1, fill = as.factor(USERWIKI))) +
     geom_bar(na.rm = TRUE) +
     labs(title = "O uso da Wikipedia estimula a curiosidade",
          x = NULL, y = "Frequência") +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14)) +
     scale_fill_manual(values = c("#2C3E50", "#16A085"), labels = c("Não usuário","Usuário"))

g2 <- ggplot(base, mapping = aes(x = ENJ2, fill = as.factor(USERWIKI))) +
     geom_bar(na.rm = TRUE) +
     labs(title = "O uso da Wikipedia é divertido",
          x = NULL, y = "Frequência") +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14)) +
     scale_fill_manual(values = c("#2C3E50", "#16A085"), labels = c("Não usuário","Usuário"))

gridExtra::grid.arrange(g1, g2, ncol = 2)

# Área de atuação
g3 <- ggplot(base, aes(x = ENJ1, fill = as.factor(DOMAIN))) +
     geom_bar(na.rm = TRUE) +
     labs(title = "O uso da Wikipedia \n estimula a curiosidade",
          x = "Escala (1 a 5)", y = "Frequência") +
     theme_dark() + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14)) +
     scale_fill_brewer(palette = "Reds",
                       labels = c("Artes & Humanidades", "Ciências",
                                  "Ciências da Saúde", "Engenharia & Arquitetura","Direito & Política", "Outra"))

g4 <- ggplot(base, aes(x = ENJ2, fill = as.factor(DOMAIN))) +
     geom_bar(na.rm = TRUE) +
     labs(title = "O uso da Wikipedia \n é divertido",
          x = "Escala (1 a 5)", y = "Frequência") +
     theme_dark() + 
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14)) +
     scale_fill_brewer(palette = "Reds",
                       labels = c("Artes & Humanidades", "Ciências",
                                  "Ciências da Saúde", "Engenharia & Arquitetura","Direito & Política", "Outra"))

gridExtra::grid.arrange(g3, g4, ncol = 2)


### Análise estatísticas
summary(base$AGE)
summary(base$ENJ1)
summary(base$ENJ2)

cor(base$USERWIKI, base$ENJ1, use = "na.or.complete")
cor(base$USERWIKI, base$ENJ2, use = "na.or.complete")
cor(base$USERWIKI, base$ENJ, use = "na.or.complete")

summary(lm(ENJ ~ USERWIKI, data = base))
