library(tidyverse)
library(RColorBrewer)
library(MASS)
library(foreign)


# Informarções do pacote RColorBrewer -------------------------------------
RColorBrewer::display.brewer.all()
brewer.pal.info


# Escala Likert -----------------------------------------------------------
legConcordancia <- c("Discorda muito","Discorda",
                     paste("Não discorda","\n","nem concorda"),
                     "Concorda","Concorda muito")
legFrequencia <- c("Nunca","Quase nunca","Às vezes","Quase sempre","Sempre")


# Tratamento da base ------------------------------------------------------
setwd("C:/Users/Claudio/OneDrive/CienciaDados")
base <- read.csv("wiki4HE.csv", sep=";")


### Substitui não-respostas ("?") por NA
base$DOMAIN <- factor(base$DOMAIN, levels = c("1","2","3","4","5","6"))
base$USERWIKI <- factor(base$USERWIKI, levels = c("0", "1"))
base$ENJ1 <- factor(base$ENJ1, levels = c("1","2","3","4","5"))
base$ENJ2 <- factor(base$ENJ2, levels = c("1","2","3","4","5"))



### Transforma variáveis categóricas (factor) em numéricas
base$YEARSEXP <- as.numeric(base$YEARSEXP)


# Transforma variável dependente em binária -------------------------------
base$ENJ1Recod <- as.factor(ifelse(is.na(base$ENJ1), NA,
                                   ifelse(base$ENJ1 %in% (1:3), 0, 1)))

base$ENJ2Recod <- as.factor(ifelse(is.na(base$ENJ2), NA,
                                   ifelse(base$ENJ2 %in% (1:3), 0, 1)))





# Análises ----------------------------------------------------------------

### Gráficos não utilizados
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
base %>%
     filter(!is.na(DOMAIN)) %>% 
ggplot(aes(x = DOMAIN, fill = as.factor(DOMAIN))) +
     geom_bar(stat = "count", position = "dodge", na.rm = TRUE) +
     geom_text(aes(label = ..count..),
               stat = "count",
               position = position_stack(),
               vjust = -0.3,
               color = "black",
               size = 4,
               font) +
     labs(title = "Área de atuação dos professores",
          x = "Área", y = NULL) +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14),
           legend.position = "bottom", legend.text.align = 0) +
     scale_fill_manual(values = c("#004D40","#00695C","#00796B","#00897B","#2980B9","#3498DB"),
                       labels = c("Artes & Humanidades", "Ciências",
                                  "Ciências da Saúde", "Engenharia & Arquitetura","Direito & Política", "Outra"))+
     scale_x_discrete(labels = NULL)


### Satisfação percebida
# Usuário
base %>%
     filter(!is.na(USERWIKI) & !is.na(ENJ1)) %>% 
     ggplot(mapping = aes(x = ENJ1, fill = USERWIKI)) +
     geom_bar(na.rm = TRUE) +
     labs(title = "O uso da Wikipedia estimula a curiosidade",
          x = NULL, y = "Frequência") +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14),
           legend.position = "bottom", legend.text.align = 0) +
     scale_fill_manual(values = c("#2C3E50", "#16A085"), labels = c("Não usuário","Usuário"))

base %>%
     filter(!is.na(USERWIKI) & !is.na(ENJ2)) %>% 
     ggplot(mapping = aes(x = ENJ2, fill = as.factor(USERWIKI))) +
     geom_bar(na.rm = TRUE) +
     labs(title = "O uso da Wikipedia é divertido",
          x = NULL, y = "Frequência") +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14),
           legend.position = "bottom", legend.text.align = 0) +
     scale_fill_manual(values = c("#2C3E50", "#16A085"), labels = c("Não usuário","Usuário"))


# Área de atuação
base %>% 
     filter(!is.na(DOMAIN) & !is.na(ENJ1)) %>% 
     ggplot(aes(x = ENJ1, fill = as.factor(DOMAIN))) +
     geom_bar(na.rm = TRUE) +
     labs(title = "O uso da Wikipedia\nestimula a curiosidade",
          x = "Escala (1 a 5)", y = "Frequência") +
     scale_fill_brewer(palette = "Reds",
                       labels = c("Artes & Humanidades", "Ciências",
                                  "Ciências da Saúde", "Engenharia & Arquitetura","Direito & Política", "Outra")) +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14),
           legend.position = "bottom", legend.text.align = 0)

base %>%
     filter(!is.na(DOMAIN) & !is.na(ENJ2)) %>% 
     ggplot(aes(x = ENJ2, fill = as.factor(DOMAIN))) +
     geom_bar(na.rm = TRUE) +
     labs(title = "O uso da Wikipedia \n é divertido",
          x = "Escala (1 a 5)", y = "Frequência") +
     scale_fill_brewer(palette = "Reds",
                       labels = c("Artes & Humanidades", "Ciências",
                                  "Ciências da Saúde", "Engenharia & Arquitetura","Direito & Política", "Outra")) +
     theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size = 20),
           legend.text = element_text(size = 12), axis.text.x = element_text(size = 14),
           legend.position = "bottom", legend.text.align = 0)



### Análise estatísticas
summary(base$AGE)
summary(base$ENJ1)
summary(base$ENJ2)

cor(base$USERWIKI, base$ENJ1, use = "na.or.complete")
cor(base$USERWIKI, base$ENJ2, use = "na.or.complete")
cor(base$USERWIKI, base$ENJ, use = "na.or.complete")

summary(lm(ENJ ~ USERWIKI, data = base))


modelo1 = glm(base$ENJ1Recod ~ base$AGE + base$GENDER + base$DOMAIN + base$PhD + base$YEARSEXP + base$UNIVERSITY + base$UOC_POSITION,
               family = binomial(link = "logit"))
summary(modelo1)


modelo2 = glm(base$ENJ1Recod ~ base$AGE + base$DOMAIN, family = binomial(link = "logit"))
summary(modelo2)
# -0.030620. Exp = 0.969844: cada ano a mais de idade do professor reduz em 3% a chance de ele concordar com a variável ENJ1
# 1.037115. Exp = 2.821066: ser da área de ciências aumenta 182% a chance de o professor concordar com a pergunta ENJ1
# y = 1.990016 - 0.030620 + 1.037115(DOMAIN==2)
# Uma pessoa de Ciências com 45 anos de idade:
# y = 1.990016 - 0.030620(45) + 1.037115 = 1.649231
# Probabilidade = exp(1.649231)/1 + exp(1.649231) = 0.8387871
# Essa pessoa tem 83% de chance de concordar com a variável ENJ1