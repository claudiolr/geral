require(tidyverse)
require(lubridate)
require(googlesheets4)


equipe <- read_sheet("https://docs.google.com/spreadsheets/d/1HlD6twCfypOJX3MiKoPeUftHIAxBM1CPN7ZipUsRLC4/edit#gid=2131320238")

colnames(equipe) = c("dataHora", paste0("p", seq(1, 17, 1)))

equipe <- equipe %>% 
        mutate(p4 = str_replace(p4, "Jequitinonha", "Jequitinhonha"))

tema3 <- function(){
        theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 0.5),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.title = element_text(colour = "gray20"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = "bottom",
              legend.margin = margin(-10,0,15,0),
              legend.box.margin = margin(-10,-10,-10,-10),
              plot.caption.position = "plot",
              legend.text = element_text(size = 9),
              plot.caption = element_text(size = 9))
}

aviso <- "*(Percentuais em relação ao total de respondentes (25). Total soma mais de 100%)"



# 1. Com o que você contribuiu mais? Marque até 2 opções
p1 <- equipe %>% 
        select(p1) %>% 
        separate(col = p1, into = c("a", "b", "c"), sep = ";", remove = TRUE, fill = "right") %>% 
        pivot_longer(cols = c(a, b, c), values_to = "Categoria", values_drop_na = TRUE) %>% 
        group_by(Categoria) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)


p1 %>% ggplot(aes(x = reorder(Categoria, -N), y = Prop, fill = Categoria, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Com o que você contribuiu mais?",
             subtitle = "(até duas opções por respondente)*",
             x = "", y = "",
             fill = "",
             caption = aviso) +
        geom_text(aes(y = Prop), size = 4, fontface = "bold", vjust = -0.4) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 75)) +
        tema() +
        guides(fill = guide_legend(nrow = 2)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p1.png", width = 25, height = 15, units = "cm")


# 2. O que você pode melhorar? Marque até 2 opções
p2 <- equipe %>% 
        select(p2) %>% 
        separate(col = p2, into = c("a", "b", "c"), sep = ";", remove = TRUE, fill = "right") %>% 
        pivot_longer(cols = c(a, b), values_to = "Categoria", values_drop_na = TRUE) %>% 
        group_by(Categoria) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)  

p2 %>% ggplot(aes(x = reorder(Categoria, -N), y = Prop, fill = Categoria, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "O que você pode melhorar?",
             subtitle = "(até duas opções por respondente)*",
             x = "", y = "",
             fill = "",
             caption = aviso) +
        geom_text(aes(y = Prop), size = 3, vjust = -0.3) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 80)) +
        tema3() +
        guides(fill = guide_legend(nrow = 5)) 

ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p2.png",
       width = 16, height = 10, units = "cm",
       dpi = 500)


# 3. O que eu ganhei com o meu trabalho? Marque quantas opções quiser. 
p3 <- equipe %>% 
        select(p3) %>% 
        separate(col = p3, into = c("a", "b", "c", "d", "e", "f"), sep = ";", remove = TRUE) %>% 
        pivot_longer(cols = c(a, b, c, d, e, f), values_to = "Categoria", values_drop_na = TRUE) %>% 
        mutate(Categoria = case_when(Categoria == "Conheci novas pessoas e fiz novas amizades." ~ "Fiz novas amizades",
                                     Categoria == "Aprendi mais sobre o trabalho para o desenvolvimento social." ~ "Aprendi sobre o trabalho para o desenv. social",
                                     TRUE ~ Categoria)) %>% 
        group_by(Categoria) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)  

p3 %>% ggplot(aes(x = reorder(Categoria, -N), y = Prop, fill = Categoria, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "O que eu ganhei com o meu trabalho?",
             subtitle = "(até seis opções por respondente)*",
             x = "", y = "",
             fill = "",
             caption = aviso) +
        geom_text(aes(y = Prop), size = 3, vjust = -0.3) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema3() + 
        theme(legend.text = element_text(size = 10)) +
        guides(fill = guide_legend(nrow = 6, ncol = 1)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p3.png",
       width = 16, height = 10, units = "cm",
       dpi = 500)


# 4. Do que eu sinto mais falta? Marque até 3 opções.
p4 <- equipe %>% 
        select(p4) %>% 
        separate(col = p4, into = c("a", "b", "c"), sep = ";", remove = TRUE) %>% 
        pivot_longer(cols = c(a, b, c), values_to = "Categoria", values_drop_na = TRUE) %>% 
        group_by(Categoria) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)  

p4 %>% ggplot(aes(x = reorder(Categoria, -N), y = Prop, fill = Categoria, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Do que eu sinto mais falta?",
             subtitle = "(até três opções por respondente)*",
             x = "", y = "",
             fill = "",
             caption = aviso) +
        geom_text(aes(y = Prop), size = 3, vjust = -0.3) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 75)) +
        tema3() +
        guides(fill = guide_legend(nrow = 4))
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p4.png",
       width = 16, height = 10, units = "cm",
       dpi = 500)


# 5. Como você percebe a comunicação entre a equipe? Marque apenas uma respostas.
p5 <- equipe %>% 
        group_by(p5) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)  

p5 %>% ggplot(aes(x = reorder(p5, -N), y = Prop, fill = p5, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como você percebe a comunicação entre a equipe?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        geom_text(aes(y = Prop), size = 3, vjust = -0.3) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 52)) +
        tema3() +
        guides(fill = guide_legend(nrow = 4)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p5.png",
       width = 16, height = 10, units = "cm",
       dpi = 500)



# 6. Como foi sua relação com a prefeitura e os órgãos públicos locais? Marque apenas uma opção.
p6 <- equipe %>% 
        mutate(p6 = case_when(p6 == "Busquei apoio e encaminhei pedidos que foram atendidos. São parceiros próximos." ~ "Pedidos foram aceitos",
                              p6 == "Busquei apoio e encaminhei pedidos, mas não foram atendidos. São parceiros distantes." ~ "Pedidos não foram aceitos",
                              p6 == "Me relacionei, mas muitas vezes não diretamente" ~ "Não me relacionei diretamente",
                              p6 == "meu contato foi apenas com a equipe do Nacab e os atingidos." ~ "Contato apenas com Nacab e atingidos",
                              p6 == "Meu contato foi apenas com a equipe do NACAB." ~ "Contato apenas com Nacab",
                              p6 == "Minhas demandas, Como Coordenador Geral, foram bem aceitas..." ~ "Demandas foram bem aceitas",
                              p6 == "Não consegui me relacionar com esse público." ~ "Não consegui me relacionar",
                              p6 == "Não houve contato individual meu com os órgãos locais. Participei de momentos com os órgãos locais junto com  equipe do NACAB." ~ "Relação junto com equipe do Nacab",
                              p6 == "Poderia ter tido mais reuniões com poder público local de toda equipe, sobretudo técnica" ~ "Poderia ter tido mais reuniçoes")) %>% 
        group_by(p6) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)

p6 %>% ggplot(aes(x = reorder(p6, -N), y = Prop, fill = p6, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como foi sua relação com a prefeitura e os órgãos públicos locais?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        geom_text(aes(y = Prop), size = 3, vjust = -0.3) +
        scale_color_hue(l = 10, c = 180) +
        scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 40)) +
        tema3() +
        guides(fill = guide_legend(nrow = 5, ncol = 2)) +
        theme(legend.text = element_text(size = 9))
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p6.png",
       width = 16, height = 10, units = "cm",
       dpi = 500)



# 7. Como foi sua relação com os atingidos? Marque apenas uma opção. 
p7 <- equipe %>% 
        group_by(p7) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)  


p7 %>% ggplot(aes(x = reorder(p7, -N), y = Prop, fill = p7, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como foi sua relação com os atingidos?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        geom_text(aes(y = Prop), size = 4, fontface = "bold", vjust = -0.3) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 60)) +
        tema() +
        guides(fill = guide_legend(nrow = 8, ncol = 2))
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p7.png", width = 25, height = 15, units = "cm")



# 8. Como foi sua relação com a Anglo América? Marque apenas uma opção.
p8 <- equipe %>% 
        mutate(p8 = case_when(p8 == "Encontrei dificuldades para me relacionar com essa instituição." ~ "Encontrei dificuldades na relação",
                              p8 == "Me aproximei dos representantes mas isso não gerou melhora na relação." ~ "Me aproximei, mas não melhorou a relaçao",
                              p8 == "Meu contato foi apenas com a equipe do NACAB." ~ "Contato apenas com Nacab",
                              p8 == "Não estabeleci nenhum contato com eles." ~ "Não estabeleci contato")) %>% 
        group_by(p8) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)


p8 %>% ggplot(aes(x = reorder(p8, -N), y = Prop, fill = p8, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como foi sua relação com a Anglo American?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        geom_text(aes(y = Prop), size = 4, fontface = "bold", vjust = -0.3) +
        scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 40)) +
        tema3() +
        guides(fill = guide_legend(nrow = 2)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p8.png",
       width = 15, height = 7, units = "cm",
       dpi = 500)



# 9. Como foi sua relação com a Fundação Israel Pinheiro - FIP?  Marque apenas uma opção.
p9 <- equipe %>% 
        group_by(p9) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)


p9 %>% ggplot(aes(x = reorder(p9, -N), y = Prop, fill = p9, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como foi sua relação com a Fundação Israel Pinheiro - FIP?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        geom_text(aes(y = Prop), size = 4, fontface = "bold", vjust = -0.3) +
        scale_fill_manual(values = c(verdeClaro, cinza, verdeEscuro)) + 
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 60)) +
        tema3() +
        guides(fill = guide_legend(nrow = 3)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p9.png",
       width = 14, height = 8, units = "cm",
       dpi = 500)


# 10. Como foi sua relação com o Ministério Público? Marque apenas uma opção.
p10 <- equipe %>% 
        group_by(p10) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)


p10 %>% ggplot(aes(x = reorder(p10, -N), y = Prop, fill = p10, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como foi sua relação com o Ministério Público?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        geom_text(aes(y = Prop), size = 4, fontface = "bold", vjust = -0.3) +
        scale_fill_manual(values = c(verdeClaro, cinza, verdeEscuro)) + 
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 50)) +
        tema3() +
        guides(fill = guide_legend(nrow = 3)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p10.png",
       width = 14, height = 8, units = "cm",
       dpi = 500)


# 11. Como foi sua relação com a Associação dos Moradores de São Sebastião do Bom Sucesso - ASCOB ? Marque apenas uma opção
p11 <- equipe %>% 
        group_by(p11) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)


p11 %>% ggplot(aes(x = reorder(p11, -N), y = Prop, fill = p11, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como foi sua relação com a Associação dos\nMoradores de São Sebastião do Bom Sucesso - ASCOB ?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        geom_text(aes(y = Prop), size = 4, fontface = "bold", vjust = -0.4) +
        scale_fill_manual(values = c("#6ad667", "gray30", "#22781f")) + 
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 70)) +
        tema3() +
        guides(fill = guide_legend(nrow = 3)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p11.png",
       width = 14, height = 8, units = "cm",
       dpi = 500)


# 12. Como você observa os equipamentos de trabalho (eletrônicos, manuais e automóveis)? Marque apenas uma opção.
p12 <- equipe %>% 
        group_by(p12) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)


p12 %>% ggplot(aes(x = reorder(p12, -N), y = Prop, fill = p12, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como você observa os equipamentos de trabalho?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        geom_text(aes(y = Prop), size = 3, vjust = -0.4) +
        scale_fill_manual(values = c(laranja, verdeEscuro)) + 
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 70)) +
        tema3() +
        guides(fill = guide_legend(nrow = 3)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p12.png",
       width = 14, height = 8, units = "cm",
       dpi = 500)


# 13. Como você percebe os locais de trabalho? Marque quantas opções quiser. 
p13 <- equipe %>% 
        select(p13) %>%
        separate(col = p13, into = c("a", "b", "c", "d"), sep = ";", remove = TRUE, fill = "right") %>%
        pivot_longer(cols = c(a, b, c, d), values_to = "Categoria", values_drop_na = TRUE) %>% 
        group_by(Categoria) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)  


p13 %>% ggplot(aes(x = reorder(Categoria, -N), y = Prop, fill = Categoria, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como você percebe os locais de trabalho?",
             subtitle = "(até quatro opções por respondente)*",
             x = "", y = "",
             fill = "",
             caption = aviso) +
        geom_text(aes(y = Prop), size = 3, vjust = -0.4) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 80)) +
        tema3() +
        theme(legend.text = element_text(size = 10)) +
        guides(fill = guide_legend(nrow = 4)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p13.png",
       width = 16, height = 8, units = "cm",
       dpi = 500)


# 14. Como você percebe os cuidados da equipe com os equipamentos e locais de trabalho? Marque apenas uma opção. 
p14 <- equipe %>% 
        group_by(p14) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)


p14 %>% ggplot(aes(x = reorder(p14, -N), y = Prop, fill = p14, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como você percebe os cuidados da equipe com os\nequipamentos e locais de trabalho?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        scale_fill_manual(values = c("#de7226", "#22781f")) + 
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 60)) +
        geom_text(aes(y = Prop), size = 3, vjust = -0.4) +
        tema3() +
        guides(fill = guide_legend(nrow = 3)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p14.png",
       width = 14, height = 8, units = "cm",
       dpi = 500)


# 15. Quais os cuidados da equipe com o meio ambiente e comunidade local? Marque quantas opções quiser. 
p15 <- equipe %>% 
        select(p15) %>%
        separate(col = p15, into = c("a", "b", "c", "d", "e", "f", "g"), sep = ";", remove = TRUE, fill = "right") %>%
        pivot_longer(cols = c(a, b, c, d, e, f, g), values_to = "Categoria", values_drop_na = TRUE) %>% 
        group_by(Categoria) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)


p15 %>% ggplot(aes(x = reorder(Categoria, -N), y = Prop, fill = Categoria, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Quais os cuidados da equipe com o\nmeio ambiente e comunidade local?",
             subtitle = "(até oito opções por respondente)*",
             x = "", y = "",
             fill = "",
             caption = aviso) +
        geom_text(aes(y = Prop), size = 3, vjust = -0.4) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema3() +
        guides(fill = guide_legend(nrow = 8))  +
        theme(legend.text = element_text(size = 9))
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p15.png",
       width = 15, height = 12, units = "cm",
       dpi = 500)




# 16. Como você percebe as condições de comunicação do NACAB com os atingidos? Marque apenas uma opção.
p16 <- equipe %>% 
        group_by(p16) %>% 
        summarise(N = n()) %>% 
        mutate(Prop = round(N/25, digits = 3)*100)


p16 %>% ggplot(aes(x = reorder(p16, -N), y = Prop, fill = p16, label = paste0(N, " (", Prop, "%)"))) +
        geom_bar(stat = "identity") + 
        labs(title = "Como você percebe as condições de comunicação do NACAB com os atingidos?",
             subtitle = "(apenas uma opção por respondente)",
             x = "", y = "",
             fill = "") +
        geom_text(aes(y = Prop), size = 4, fontface = "bold", vjust = -0.4) +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 80)) +
        tema() +
        guides(fill = guide_legend(nrow = 4, ncol = 2)) 
ggsave("C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/p16.png", width = 25, height = 15, units = "cm")


# Salva tabelas
openxlsx::write.xlsx(list(p1 = p1,
                          p2 = p2,
                          p3 = p3,
                          p4 = p4,
                          p5 = p5,
                          p6 = p6,
                          p7 = p7,
                          p8 = p8,
                          p9 = p9,
                          p10 = p10,
                          p11 = p11,
                          p12 = p12,
                          p13 = p13,
                          p14 = p14,
                          p15 = p15,
                          p16 = p16),
                     "C:/Users/Claudio/OneDrive/ATIR39/Autoavaliacaoequipe/tabelas.xlsx")


