require(tidyverse)
require(lubridate)
require(googlesheets4)
require(janitor)
require(extrafont)

# loadfonts(device = "win", quiet = FALSE)
# windowsFonts()
# font_import()

gs4_auth(email = "claudiolr@gmail.com")
dados <- read_sheet("https://docs.google.com/spreadsheets/d/1vwGULLOfN5bA0ZJyfDKa9T9zc8wDsdaZ2u6H-GwWGHk/edit#gid=358489604",
                    col_names = TRUE)

colnames(dados) <- c("dataHoraInicio","p11","p12","p12a","p21","p21a","p22","p31","p41","p41a","p42","p43",
                     "p51","p52","p53","p54","p55","p56","p57","p58","p61","p62","p63","p64", "p65", "p66",
                     "p67","p68","p69","p610","p71","p72","p73","p74","horaFim","impressoes")


dados <- dados %>% 
        add_column("DataInicio" = "", .after = "dataHoraInicio") %>% 
        add_column("dataHoraFim" = "", .after = "DataInicio") %>% 
        add_column("DataFim" = "", .after = "dataHoraFim") %>% 
        add_column("Duração" = "", .after = "DataFim") %>% 
        add_column("p721" = "", .after = "p72") %>% 
        add_column("p722" = "", .after = "p721") %>% 
        add_column("p723" = "", .after = "p722") %>% 
        add_column("p724" = "", .after = "p723") %>% 
        add_column("p725" = "", .after = "p724") %>% 
        add_column("p726" = "", .after = "p725") %>% 
        add_column("p727" = "", .after = "p726")


dados <- dados %>% 
        mutate(DataInicio = as.Date(substr(dataHoraInicio, 0, 10)),
               DataFim = DataInicio,
               horaFim = substr(horaFim, 12, 16),
               dataHoraFim = as.POSIXct(paste(DataFim, horaFim)),
               Duração = floor(decimal_date(dataHoraFim) - decimal_date(dataHoraInicio)),
               p721 = ifelse(grepl("acompanhar as ações da Anglo", p72), "Acompanhar as ações", NA),
               p722 = ifelse(grepl("proposição de mudanças e demandas ", p72), "Proposição de mudanças", NA),
               p723 = ifelse(grepl("acessarem e compreenderem informações", p72), "Acessar e compreender informações", NA),
               p724 = ifelse(grepl("se organizarem melhor no que diz respeito", p72), "Organizar melhor na participação nas decisões", NA),
               p725 = ifelse(grepl("Não ajuda em nada a comunidade", p72), "Não ajuda em nada", NA),
               p726 = ifelse(grepl("Piorou a situação da comunidade", p72), "Piorou a situação da comunidade", NA),
               p727 = ifelse(grepl("NS/NR", p72), "Não sabe/ não respondeu", NA),
        )

dados <- dados %>% mutate(horaFim = NULL)


base <- dados %>% filter(p12 == "Sim")


base72 <- base %>% 
        pivot_longer(cols = c(p721, p722, p723, p724, p725, p726, p727), names_to = "Variável", values_to = "Valor") %>% 
        select(dataHoraInicio, p21, p72, Variável, Valor) %>% 
        filter(!is.na(Valor))


# Configuração dos gráficos -----------------------------------------------


cinza <- "#4d4d4d"
azulClaro <- "#6bb0f1"
azulEscuro <- "#182a41"
verdeClaro <- "#68ac57"
verdeEscuro <- "#3d752d"
vermelho <- "#e41a1c"
laranja <- "#de7226"


tema <- function(top = 3, right = 5, bottom = -3, left = -3){
        theme(text = element_text(family = "Open Sans", colour = cinza),
              plot.title = element_text(size = 12, family = "Open Sans SemiBold", hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 0.5),
              axis.ticks = element_blank(),
              axis.title = element_text(colour = "gray40"),
              axis.text = element_text(size = 9),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x =  element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = "",
              legend.margin = margin(-2,0,15,0),
              legend.box.margin = margin(-5,-5,-5,-5),
              plot.caption.position = "plot",
              legend.text = element_text(size = 9),
              plot.margin = margin(top, right, bottom, left))
}

salvaPNG <- function(nome, largura = 12, altura = 6){
        ggsave(path = "C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/",
               filename = nome,
               width = largura,
               height = altura,
               units = "cm",
               dpi = 500)
}

rotulo <- function(tamanho = 4){
        geom_text(aes(y = Prop),
                  vjust = -0.5,
                  family = "Open Sans",
                  colour = "black",
                  size = tamanho)
}



# Identificação -----------------------------------------------------------


# Entrevistas por dia
base %>% 
        group_by(DataInicio) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = DataInicio, y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity", aes(fill = -N)) + 
        labs(title = "Entrevistas por dia", x = "", y = "") +
        geom_text(aes(y = Prop), vjust = -0.5, family = "Open Sans") +
        scale_x_date(date_breaks = "1 day", date_labels = "%d-%B") +
        scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 35)) +
        scale_fill_gradient(low = azulEscuro, high = azulClaro) +
        tema(3, 5, -3, -3) +
        theme(axis.text.x = element_text())

salvaPNG("entrevistas.png", 14, 8)



# P21.a - Entrevistado é o mesmo da lista?
base %>% 
        tabyl(p21a) %>% 
        mutate(percent = round(percent, digits = 3)*100)


base %>% 
        group_by(p21a) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = p21a, y = Prop, fill = p21a, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") +
        labs(title = "Pessoa entrevistada é a mesma da lista?", x = "", y = "", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5, family = "Open Sans") +
        scale_fill_manual(values = c(verdeEscuro, vermelho)) +
        scale_x_discrete(labels = c("Mesma da lista", "Pertence ao núcleo familiar")) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p22.png", 12, 6)



# Comunidade de residência/ propriedade
base %>% 
        group_by(p22) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p22, -N), y = Prop, fill = p22, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") +
        labs(title = "Comunidade onde a pessoa entrevistada reside ou possui propriedade",
             x = "", y = "", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 35)) +
        # scale_fill_manual(values = c(vermelho, verdeClaro, verdeEscuro, cinza)) +
        tema()

salvaPNG("p221.png", 20, 8)


# Bloco 4 -----------------------------------------------------------------

# 4.1
base %>% 
        group_by(p41) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p41, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity", aes(fill = p41)) +
        labs(title = "O(a) Senhor(a) sabe da existência destes\nespaços [de representação e participação]?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p41.png", 12, 8)



# 4.1.a
base %>% 
        filter(!is.na(p41a)) %>% 
        group_by(p41a) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p41a, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity", aes(fill = c(cinza, vermelho, verdeEscuro, laranja))) +
        labs(title = "O(a) Senhor(a) considera que esses espaços de\nparticipação e discussão são importantes\npara você e sua comunidade?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(cinza, verdeEscuro, vermelho, laranja),
                          labels = c("Não respondeu", "É muito\nimportante", "Não tem\nimportância nenhuma", "É pouco\nimportante")) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema(right = 6) +
        theme(axis.text.x = element_blank(),
              legend.position = "right")
salvaPNG("p41a.png", 15, 8)



# 4.2
base %>% 
        group_by(p42) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p42, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity", aes(fill = p42)) +
        labs(title = "O(a) Senhor(a) recebe informações da ATI39/NACAB\nfalando e convidando para as atividades de participação?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()
salvaPNG("p42.png", 13, 6)


# 4.3
base %>% 
        group_by(p43) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p43, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col(aes(fill = p43)) +
        labs(title = "O(a) Senhor(a) considera que hoje a comunidade está mais e\nmelhor organizada para participar, discutir e contribuir para a solução dos\nproblemas decorrentes do empreendimento da Anglo American?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema(right = 20)

salvaPNG("p43.png", 17, 9)




# Bloco 5 -----------------------------------------------------------------

# 5.1
base %>% 
        group_by(p51) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p51, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col(aes(fill = p51)) +
        labs(title = "O(a) Senhor(a) recebe informações da ATI39/NACAB\nsobre as atividades desenvolvidas pela\nAssessoria Técnica de uma maneira geral?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p51.png", 12, 7)


# 5.2
base %>% 
        filter(p51 == "Sim") %>% 
        group_by(p52) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p52, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity", aes(fill = p52)) +
        labs(title = "Se sim, o(a) Senhor(a) considera que as informações\nprestadas pela ATI39/NACAB são úteis para você?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p52.png", altura = 7)


# 5.3
base %>% 
        filter(p51 == "Sim") %>% 
        group_by(p53) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p53, -N), y = Prop, fill = p53, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") +
        labs(title = "O(a) Senhor(a) considera que as informações divulgadas\npela ATI39/NACAB são claras e de fácil entendimento?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        theme(plot.margin = margin(1, 2.5, 0.3, 0.5))

salvaPNG("p53.png",13, 7)


# 5.4
base %>% 
        filter(p51 == "Sim") %>% 
        group_by(p54) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = p54, y = Prop, fill = p54, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") +
        labs(title = "Em relação a quantidade (frequência) da divulgação de\ninformações pela ATI39/NACAB, o(a) Senhor(a) entende que:",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(verdeEscuro, laranja, vermelho)) +
        scale_x_discrete(labels = c("Quantidade adequada", "Deveria ser menos", "Deveria ser mais")) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p54.png", 14, 7)


# 5.5
base %>% 
        group_by(p55) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p55, -N), y = Prop, fill = p55, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") +
        labs(title = "O(a) Senhor(a) tem conhecimento de que a ATI39/NACAB realizou\natividades para os atingidos voltadas para a discussão de temas?",
             subtitle = "(Licenciamento Ambiental, Direito Minerário, Oficina de\nMapeamento Territorial Participativa, Cartografia Social)",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p55.png", 15, 8)


# 5.6
base %>% 
        group_by(p56) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p56, -N), y = Prop, fill = p56, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") +
        labs(title = "O(a) Senhor(a) tem conhecimento de que a ATI39/NACAB\nrealizou atividades em que foram discutidos temas?",
             subtitle = "(Direitos Humanos, Reativação Econômica/ Empreendedorismo)",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p56.png", 14, 7, units = "cm")


# 5.7
base %>% 
        group_by(p57) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p57, -N), y = Prop, fill = p57, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") +
        labs(title = "O(a) Senhor(a) tem conhecimento de que a ATI39/NACAB\nrealizou estudos técnicos-científicos sobre temas de\ninteresse dos atingidos?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p57.png", 14, 8)


# 5.8
base %>% 
        group_by(p58) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p58, -N), y = Prop, fill = p58, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Após a chegada da ATI39/NACAB, o(a) Senhor(a) considera que está\nmais bem informado sobre as questões envolvendo o\nempreendimento da Anglo American e suas consequências nas\ncomunidades atingidas?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p58.png", 15, 8)



# Bloco 6 -----------------------------------------------------------------

# 6.1
base %>% 
        group_by(p61) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p61, -N), y = Prop, fill = p61, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "O(a) Senhor(a) já apresentou alguma demanda (queixa) à\nATI39/NACAB sobre a sua situação em relação à Anglo American?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p61.png", 15, 8)


# 6.2
base %>% 
        filter(p61 == "Sim") %>% 
        group_by(p62) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p62, -N), y = Prop, fill = p62, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Se sim, o(a) Senhor(a) considera que a forma como\nfoi atendido(a) pelos técnicos da ATI39/NACAB foi adequada?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p62.png", 14, 7)


# 6.3
base %>% 
        filter(p61 == "Sim") %>% 
        group_by(p63) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p63, -N), y = Prop, fill = p63, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "O(a) Senhor(a) está bem informado(a)\nsobre os encaminhamentos dados à sua demanda?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p63.png", 14, 7)


# 6.4
base %>% 
        filter(p61 == "Sim") %>% 
        group_by(p64) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>%
        ggplot(aes(x = reorder(p64, -N), y = Prop, fill = p64, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "O(a) Senhor(a) considera que a ATI39/NACAB se empenhou o\nsuficiente para dar o melhor encaminhamento possível à sua demanda?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p64.png", 16, 8)



# 6.5
base %>% 
        group_by(p65) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p65, -N), y = Prop, fill = p65, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") +
        labs(title = "O(a) Senhor(a) tem conhecimento de que a ATI39/NACAB construiu\njunto aos atingidos o que tem sido chamado de “Pauta Coletiva”?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p65.png", 15, 8)


# 6.6
base %>% 
        filter(!is.na(p66)) %>% 
        group_by(p66) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p66, -N), y = Prop, fill = p66, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") +
        labs(title = "O(a) Senhor(a) considera que a Pauta Coletiva é:",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(verdeEscuro, vermelho, cinza)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p66.png", 12, 6)




# 6.7
base %>% 
        group_by(p67) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p67, -N), y = Prop, fill = p67, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "O(a) Senhor(a) sabe que alguns representantes dos atingidos têm\nparticipado de reuniões para discutir a situação das comunidades\nem relação à Anglo American?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(vermelho, cinza, verdeEscuro)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p67.png", 15, 8)



# 6.8
base %>% 
        group_by(p68) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p68, -N), y = Prop, fill = p68, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "O(a) Senhor(a) considera que a\nparticipação dos atingidos nessas reuniões:",
             x = "", y = "", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_fill_manual(values = c(verdeEscuro, laranja, cinza, vermelho)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        theme(axis.text.x = element_blank(),
              plot.margin = margin(0.1, 0.40, 0.1, 0.1, "cm"),
              legend.position = "bottom") +
        guides(fill = guide_legend(nrow = 2))

salvaPNG("p68.png", 15, 8)



# 6.9
base %>% 
        group_by(p69) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p69, -N), y = Prop, fill = p69, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "O(a) Senhor(a) sabia da existência do processo de\nMonitoramento e Avaliação dos trabalhos da\nATI39/NACAB antes de entrarmos em contato?",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(verdeEscuro, vermelho)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()

salvaPNG("p69.png", 12, 7)


# 6.10
base %>% 
        group_by(p610) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = reorder(p610, -N), y = Prop, fill = p610, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "O(a) Senhor(a) considera que esse processo de Monitoramento e\nAvaliação, em que os atingidos podem emitir sua opinião\nsobre o trabalho da ATI39/NACAB livremente, é:",
             x = "", y = "", fill = "") +
        rotulo() +
        scale_fill_manual(values = c(verdeEscuro, cinza, vermelho, laranja)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        theme(axis.text.x = element_blank(),
              plot.margin = margin(0.1, 0.40, 0.1, 0.1, "cm"),
              legend.position = "bottom") +
        guides(fill = guide_legend(nrow = 2))

salvaPNG("p610.png", 15, 8)



# Bloco 7 -----------------------------------------------------------------

# 7.1
p71 <- base %>% 
        group_by(p71) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p71 %>% ggplot(aes(x = p71, y = N, fill = -p71, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "7.1 - De acordo com o(a) Senhor(a) viu ao longo deste último ano, qual o seu grau de\nsatisfação em relação aos trabalhos desenvolvidos pela ATI39/NACAB em\numa escala de 1 a 10 em que 1 representa “muito insatisfeito” e\n10 representa “muito satisfeito”? [lista 1 a 10]",
             x = "", y = "N", fill = "") +
        geom_text(aes(y = N), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 15)) +
        scale_x_continuous(breaks = seq(1, 10, 1)) +
        tema()
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p71.png", width = 30, height = 15, units = "cm")


# 7.2
p72 <- base72 %>% 
        group_by(Valor) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/67, digits = 3)*100)

p72 %>% ggplot(aes(x = reorder(Valor, N), y = Prop, fill = Valor, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "7.2 - De um modo geral, o(a) Senhor(a) considera que o\ntrabalho da ATI39/NACAB ajuda os atingidos a:",
             subtitle = "(percentuais em relação ao total de entrevistados)",
             x = "", y = "%", fill = "") +
        geom_text(aes(y = Prop), hjust = -0.1) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line()) +
        coord_flip()
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p72.png", width = 30, height = 15, units = "cm")

        
# 7.3
p73 <- base %>% 
        group_by(p73) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p73 %>% ggplot(aes(x = reorder(p73, -N), y = N, label = paste0(N, " (", Prop, "%)"))) +
        geom_col(aes(fill = c(cinza, vermelho, verdeEscuro))) +
        labs(title = "7.3 - O(a) Senhor(a) tem interesse em continuar\nrecebendo Assessoria Técnica do NACAB?",
             x = "", y = "N", fill = "") +
        geom_text(aes(y = N), vjust = -0.5) +
        scale_fill_manual(values = c(verdeEscuro, cinza, vermelho)) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema()
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p73.png", width = 30, height = 15, units = "cm")



# Cruzamentos -------------------------------------------------------------

# Por comunidade
p22_31 <- base %>% 
        group_by(p22, p31) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_31 %>% ggplot(aes(x = reorder(p31, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Sabe da existência da ATI (3.1), por comunidade", x = "", y = "", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        facet_wrap( ~ p22, nrow = 2)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_31.png", width = 30, height = 15, units = "cm")



p22_42 <- base %>% 
        group_by(p22, p42) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_42 %>% ggplot(aes(x = reorder(p42, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Recebe informações da ATI39/NACAB falando e convidando\npara as atividades de participação (4.2), por comunidade", x = "", y = "%", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        facet_wrap( ~ p22, nrow = 2)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_42.png", width = 30, height = 15, units = "cm")



p22_43 <- base %>% 
        group_by(p22, p43) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_43 %>% ggplot(aes(x = reorder(p43, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Considera que hoje a comunidade está mais e\nmelhor organizada (4.3), por comunidade", x = "", y = "%", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        facet_wrap( ~ p22, nrow = 2)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_43.png", width = 30, height = 15, units = "cm")



p22_51 <- base %>% 
        group_by(p22, p51) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_51 %>% ggplot(aes(x = reorder(p51, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Recebe informações sobre as atividades desenvolvidas\npela Assessoria Técnica (5.1), por comunidade", x = "", y = "%", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        facet_wrap( ~ p22, nrow = 2)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_51.png", width = 30, height = 15, units = "cm")



p22_55 <- base %>% 
        group_by(p22, p55) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_55 %>% 
        ggplot(aes(x = reorder(p55, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Tem conhecimento de que a ATI39/NACAB realizou atividades\npara os atingidos voltadas para a discussão de temas (5.5), por comunidade",
             x = "", y = "%", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        facet_wrap( ~ p22, nrow = 2)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_55.png", width = 30, height = 15, units = "cm")



p22_56 <- base %>% 
        group_by(p22, p56) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_56 %>% ggplot(aes(x = reorder(p56, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Tem conhecimento de que a ATI39/NACAB realizou atividades em que foram discutidos temas (5.6), por comunidade",
             subtitle = "(Direitos Humanos, Reativação Econômica/ Empreendedorismo)",
             x = "", y = "%", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        facet_wrap( ~ p22, nrow = 2)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_56.png", width = 30, height = 15, units = "cm")



p22_57 <- base %>% 
        group_by(p22, p57) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_57 %>% ggplot(aes(x = reorder(p57, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Tem conhecimento de que a ATI39/NACAB realizou\nestudos técnicos-científicos sobre temas de interesse dos atingidos (5.7),\npor comunidade",
             x = "", y = "%", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        facet_wrap( ~ p22, nrow = 2)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_57.png", width = 30, height = 15, units = "cm")



p22_63 <- base %>% 
        filter(p61 == "Sim") %>% 
        group_by(p22, p63) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_63 %>% ggplot(aes(x = reorder(p63, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "6.3 - O(a) Senhor(a) está bem informado(a)\nsobre os encaminhamentos dados à sua demanda?",
             x = "", y = "%", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        facet_wrap( ~ p22, nrow = 2)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_63.png", width = 30, height = 15, units = "cm")



p22_65 <- base %>% 
        group_by(p22, p65) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_65 %>% ggplot(aes(x = reorder(p65, -N), y = Prop, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "6.5 - O(a) Senhor(a) tem conhecimento de que a ATI39/NACAB construiu\njunto aos atingidos o que tem sido chamado de “Pauta Coletiva”?",
             x = "", y = "%", fill = "") +
        geom_text(aes(y = Prop), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema() +
        facet_wrap( ~ p22, nrow = 2)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_65.png", width = 30, height = 15, units = "cm")



# 7.1 por comunidades
p22_71 <- base %>% 
        group_by(p22, p71) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100)

p22_71 %>% ggplot(aes(x = p71, y = N, fill = -p71, label = paste0(N, " (", Prop, "%)"))) +
        geom_col() +
        labs(title = "Satisfação [lista 1 a 10] com trabalhos desenvolvidos pela ATI39/Nacab, por comunidade",
             x = "", y = "N", fill = "") +
        geom_text(aes(y = N), vjust = -0.5) +
        scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 10)) +
        scale_x_continuous(breaks = seq(1, 10, 1)) +
        tema() +
        facet_wrap(~ p22)
ggsave("C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/p22_71.png", width = 35, height = 15, units = "cm")



# Comunicação x Avaliação -------------------------------------------------

base %>%
        filter(p31 != "Não respondeu") %>% 
        group_by(p42, p31) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        # pivot_wider(names_from = p51, values_from = N) %>% 
        ggplot(aes(x = p42, y = Prop, fill = p31)) + 
        geom_col(position = position_stack()) +
        labs(title = "Recebe informações da ATI (p4.2) x\nConhecimento da existência da ATI (p3.1)",
             x = "Sabe da existência da ATI",
             y = "Recebe informações da ATI") +
        tema()


base %>%
        filter(p31 != "Não respondeu") %>% 
        group_by(p42, p43) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        # pivot_wider(names_from = p51, values_from = N) %>% 
        ggplot(aes(x = p42, y = Prop, fill = p43)) + 
        geom_col(position = position_stack()) +
        labs(title = "Recebe informações da ATI (p4.2),\npor conhecimento da existência da ATI (p3.1)",
             x = "Sabe da existência da ATI",
             y = "Recebe informações da ATI") +
        tema()




# Salvamento --------------------------------------------------------------
openxlsx::write.xlsx(list(entrevistas = entrevistas,
                          entrevPesq = entrevPesq,
                          entrevPesqDia = entrevPesqDia,
                          p22 = p22, p221 = p221,
                          p31 = p31,
                          p41 = p41, p41a = p41a, p42 = p42, p43 = p43,
                          p51 = p51, p52 = p52, p53 = p53, p54 = p54, p55 = p55, p56 = p56, p57 = p57, p58 = p58,
                          p61 = p61, p610 = p610, p62 = p62, p63 = p63, p64 = p64, p65 = p65, p66 = p66, p67 = p67, p68 = p68, p69 = p69,
                          p71 = p71, p72 = p72, p73 = p73,
                          p22_31 = p22_31,
                          p22_42 = p22_42,
                          p22_43 = p22_43,
                          p22_51 = p22_51,
                          p22_55 = p22_55,
                          p22_56 = p22_56,
                          p22_57 = p22_57,
                          p22_63 = p22_63,
                          p22_65 = p22_65,
                          p22_71 = p22_71),
                     "C:/Users/Claudio/OneDrive/ATIR39/GraficosTabelas/tabelas.xlsx")

