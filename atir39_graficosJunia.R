require(tidyverse)
require(lubridate)
require(googlesheets4)


# Configuração dos gráficos -----------------------------------------------


tema2 <- function(){
        theme(text = element_text(family = "Open Sans"),
              plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 0.5),
              axis.ticks = element_blank(),
              axis.title = element_text(colour = "gray20"),
              axis.text = element_text(size = 9),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x =  element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = "",
              legend.margin = margin(-2,0,15,0),
              legend.box.margin = margin(-10,-10,-10,-10),
              plot.caption.position = "plot",
              legend.text = element_text(size = 9))
}




partComissAtingidos <- read.table("clipboard", header = TRUE, sep = "\t")

partComissAtingidos <- partComissAtingidos %>% 
        mutate(DATA = as.Date(DATA, tryFormats = c("%d/%m/%Y")))

partComissAtingidos %>%
        arrange(DATA) %>% 
        mutate(Data2 = format(DATA, "%d/%m/%Y")) %>% 
        ggplot(aes(x = reorder(Data2, DATA), y = PARTICIPANTES, fill = -PARTICIPANTES, labels = PARTICIPANTES)) +
        geom_col() +
        labs(title = "Participação da Comissão de Atingidos",
             x = "", y = "N") +
        geom_text(aes(label = PARTICIPANTES), size = 3, vjust = -0.2) +
        scale_y_continuous(breaks = seq(0, 15, 2), limits = c(0, 15)) +
        scale_fill_gradient(low = azulEscuro, high = azulClaro) +
        # scale_x_date(date_labels = "%d/%m/%Y") +
        tema2() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              panel.grid.minor.y = element_line())

ggsave("C:/Users/Claudio/OneDrive/ATIR39/ParticipacaoComissoes.png", width = 12, height = 8, units = "cm", dpi = 500)



notaGeralComissao <- read.table("clipboard", header = TRUE, sep = "\t", dec = ",")

notaGeralComissao <- notaGeralComissao %>% 
        rename(nota = Nota.Geral.Atribuída.às.Reuniões.Comissão.Atingidos,
               reunião = X)


notaGeralComissao %>% 
        ggplot(aes(x = reunião, y = nota, fill = -nota, labels = nota)) +
        geom_col() +
        labs(title = "Nota geral atribuída às reuniões da Comissão de Atingidos",
             x = "", y = "") +
        geom_text(aes(label = nota), size = 4, vjust = -0.3) +
        scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 11)) +
        scale_fill_gradient(low = azulEscuro, high = azulClaro) +
        tema() +
        theme(plot.margin = margin(0.4, 1.5, 0.2, 0.2, "cm"))
ggsave("C:/Users/Claudio/OneDrive/ATIR39/NotaGeralComissoes.png", width = 14, height = 6, units = "cm", dpi = 500)



notaAssuntos <- read.table("clipboard", header = TRUE, sep = "\t", dec = ",")

notaAssuntos %>% 
        ggplot(aes(x = reorder(Assuntos.tratados, X4.8), y = X4.8, fill = -X4.8, labels = X4.8)) +
        geom_col() +
        labs(title = "Avaliação média das reuniões, por quesito",
             x = "Quesito", y = "") +
        geom_text(aes(label = X4.8), size = 3, hjust = -0.3) + 
        scale_y_continuous(breaks = seq(0, 5, 1), limits = c(0, 5.2)) +
        tema() +
        theme(plot.margin = margin(0.4, 1.5, 0.2, 0.2, "cm")) +
        coord_flip()
ggsave("C:/Users/Claudio/OneDrive/ATIR39/NotaQuesitosComissoes.png", width = 12, height = 6, units = "cm", dpi = 500)



reunioesMob <- read.table("clipboard", header = TRUE, sep = "\t", dec = ",")

reunioesMob <- reunioesMob %>% 
        mutate(Data = as.Date(Data, tryFormats = c("%d/%m/%Y")))

reunioesMob %>% 
        mutate(Data = format(Data, "%d/%m/%Y")) %>% 
        ggplot(aes(x = as.factor(Data), y = Participantes, fill = -Participantes, labels = Participantes)) +
        geom_col() +
        labs(title = "Reuniões de mobilização (2019)",
             x = "", y = "Participantes") +
        geom_text(aes(label = Participantes), size = 6, vjust = -0.3) +
        scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 27)) +
        tema()
ggsave("C:/Users/Claudio/OneDrive/ATIR39/reunioesMob.png", width = 25, height = 15, units = "cm")



partAtingidos <- read.table("clipboard", header = TRUE, sep = "\t")

partAtingidos %>% 
        mutate(Data = as.Date(Data, format("%d/%m/%Y"))) %>% 
        group_by(Data, Evento) %>% 
        summarise(Participantes = sum(Participantes)) %>% 
        arrange(Data) %>% 
        mutate(Evento2 = paste0(format(Data, "%d/%m/%Y"), " - ", Evento)) %>% 
        ggplot(aes(x = reorder(Evento2, desc(Data)), y = Participantes, fill = -Participantes, labels = Participantes)) +
        geom_col() +
        labs(title = "Participação de atingidos nas Reuniões",
             x = "Reuniões", y = "Participantes") +
        geom_text(aes(label = Participantes), size = 3, angle = 0, hjust = -0.1, vjust = 0.5) +
        scale_fill_gradient(low = azulEscuro, high = azulClaro) +
        tema() +
        # theme(axis.text.y = element_text(size = 10, angle = 0, hjust = 1, vjust = 0.5),
        #       axis.text.x = element_text(size = 12, angle = 0, hjust = 1, vjust = 0.5)) +
        theme(axis.text = element_text(size = 7),
              plot.margin = margin(0.4, 0.5, 0.2, 0.2, "cm")) +
        coord_flip()
ggsave("C:/Users/Claudio/OneDrive/ATIR39/partAtingReunioes.png", dpi = 500)
# width = 15, height = 10, units = "cm", 



partSocioambiental <- read.table("clipboard", header = TRUE, sep = "\t")

partSocioambiental %>% 
        mutate(Data = as.Date(Data, format("%d/%m/%Y"))) %>% 
        group_by(Data, Reunião) %>% 
        summarise(Participantes = sum(Participantes)) %>% 
        arrange(Data) %>% 
        mutate(Evento = paste0(format(Data, "%d/%m/%Y"), " - ", Reunião)) %>% 
        ggplot(aes(x = reorder(Evento, desc(Data)), y = Participantes, fill = -Participantes, labels = Participantes)) +
        geom_col() +
        labs(title = "Reuniões de acesso e qualificação socioambiental",
             x = "Reuniões", y = "Participantes") +
        geom_text(aes(label = Participantes), size = 3, angle = 0, hjust = -0.1, vjust = 0.5) +
        tema2() +
        theme(plot.title = element_text(hjust = 2)) +
        coord_flip()
ggsave("C:/Users/Claudio/OneDrive/ATIR39/partSocioambiental.png",
       width = 16, height = 10, units = "cm",
       dpi = 500)



openxlsx::write.xlsx(list(notaAssuntos = notaAssuntos,
                          notaGeralComissao = notaGeralComissao,
                          partAtingidos = partAtingidos,
                          partComissAtingidos = partComissAtingidos,
                          partSocioambiental = partSocioambiental,
                          reunioesMob = reunioesMob), "C:/Users/Claudio/OneDrive/ATIR39/graficosJunia.xlsx")


