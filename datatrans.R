require(tidyverse)
require(lubridate)


# Carrega base de acidentes de carro 
baseOriginal <- read.csv(file = "D:\\CienciaDados\\Bases\\Datatran\\datatran2019.csv", sep = ";", dec = ",")

dados <- baseOriginal %>% 
     mutate(data_inversa = as.Date(data_inversa))



# Descrição dos dados

# Data: quantidade de acidentes por dia
dados %>% 
     count(data_inversa, name = "N") %>% 
     ggplot(aes(x = data_inversa, y = N)) + 
     geom_line() + 
        geom_smooth()


# Data: quantidade por semana
dados %>% 
     mutate(Semana = week(data_inversa)) %>% 
     count(Semana, name = "N") %>% 
     ggplot(aes(x = Semana, y = N)) + 
     geom_bar(stat = "identity")


# Data: quantidade por mês
dados %>% 
     mutate(Mês = month(data_inversa)) %>% 
     count(Mês, name = "N") %>% 
     ggplot(aes(x = Mês, y = N)) + 
     geom_bar(stat = "identity") + 
     scale_x_continuous(breaks = seq(1, 12, 1))



# Dia da semana
dados %>% 
     count(dia_semana) %>% 
     ggplot(aes(x = dia_semana, y = n, fill = dia_semana)) + 
     geom_bar(stat = "identity") + 
     scale_x_discrete(limits = c("domingo", "segunda-feira", "terça-feira", "quarta-feira", "quinta-feira",
                                 "sexta-feira", "sábado"))


# Mês
dados %>% 
        mutate(Mês = month(data_inversa)) %>% 
        count(Mês, name = "N") %>% 
        ggplot(aes(x = Mês, y = N, fill = -N)) + 
        geom_bar(stat = "identity") + 
        scale_x_continuous(breaks = seq(1, 12, 1))



# UF
dados %>% 
        count(uf, name = "N") %>% 
        ggplot(aes(x = uf, y = N, fill = uf)) + 
        geom_bar(stat = "identity") +
        theme(legend.position = "none")


# Causa do acidente
dados %>%
        count(causa_acidente, name = "N") %>% 
        ggplot(aes(x = reorder(causa_acidente, N), y = N, fill = -N)) +
        geom_col() +
        coord_flip()


# Tipo de acidente
dados %>%
        count(tipo_acidente, name = "N") %>% 
        ggplot(aes(x = reorder(tipo_acidente, N), y = N, fill = -N)) +
        geom_col() +
        coord_flip()


# Classificação do acidente
dados %>%
        count(classificacao_acidente, name = "N") %>% 
        ggplot(aes(x = reorder(classificacao_acidente, N), y = N, fill = -N)) +
        geom_col() +
        coord_flip()


# Fase do dia
dados %>%
        count(fase_dia, name = "N") %>% 
        ggplot(aes(x = reorder(fase_dia, N), y = N, fill = -N)) +
        geom_col() +
        coord_flip()


# Condição metereológica
dados %>%
        count(condicao_metereologica, name = "N") %>% 
        ggplot(aes(x = reorder(condicao_metereologica, N), y = N, fill = -N)) +
        geom_col() +
        coord_flip()


# Tipo de pista
dados %>%
        count(tipo_pista, name = "N") %>% 
        ggplot(aes(x = reorder(tipo_pista, N), y = N, fill = -N)) +
        geom_col() +
        coord_flip()


# Traçado da via
dados %>%
        count(tracado_via, name = "N") %>% 
        ggplot(aes(x = reorder(tracado_via, N), y = N, fill = -N)) +
        geom_col() +
        coord_flip()


# Pessoas envolvidas
dados %>% 
        select(pessoas, mortos, feridos_leves, feridos_graves, ilesos, ignorados) %>% 
        pivot_longer(cols = c(pessoas, mortos, feridos_leves, feridos_graves, ilesos, ignorados), values_to = "N") %>% 
        ggplot(aes(x = name, y = N)) +
        geom_boxplot()


# 
dados %>% 
        ggplot(aes(x = mortos)) +
        geom_boxplot()


dados %>% 
        ggplot(aes(x = pessoas, y = mortos)) +
        geom_point()