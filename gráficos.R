require(tidyverse)
require(lubridate)


# Carrega base de acidentes de carro 
baseOriginal <- read.csv(file = "D:\\CienciaDados\\Bases\\Datatran\\datatran2019.csv", sep = ";", dec = ",")

dados <- baseOriginal %>% 
        mutate(data_inversa = as.Date(data_inversa))


glimpse(dados)





# Gráfico de barras com uma variável ----------------------------------------------------------

### Análise de dados categóricos (uma variável)
## Sem transformação prévia de dados
# Gráfico de barras/colunas simples
dados %>% 
        ggplot(aes(x = fase_dia)) + 
        geom_bar()


# Adicionando etiquetas de valores (texto)
# Para gráficos em documentos de texto (word), o tamanho = 3 é o ideal
dados %>% 
        ggplot(aes(x = fase_dia)) + 
        geom_bar() +
        geom_text(aes(label =..count..), stat = "count", vjust = -0.2, size = 3)


# Adicionando etiquetas de valores (caixa)
dados %>% 
        ggplot(aes(x = fase_dia)) + 
        geom_bar() +
        geom_label(aes(label =..count..), stat = "count", vjust = -0.2, size = 3)


# Ajustando largura das barras
dados %>% 
        ggplot(aes(x = fase_dia)) + 
        # ajuste da largura: width (entre 0 e 1)
        geom_bar(width = 0.6) +
        geom_label(aes(label =..count..), stat = "count", vjust = -0.2, size = 3)




# Gráfico de barras com duas variáveis --------------------------------------------------------

# Cruzamento entre duas variáveis, com valore empilhados
# A posiçção default do geom_bar() é a position_stack()
dados %>% 
        ggplot(aes(x = fase_dia, fill = tipo_pista)) + 
        geom_bar()

# Para posicionar as barras lado a lado, usar position_dodge() ou position_dodge2()
# Parâmetros:
# width: útil para igualar a largura das barras
# padding: distância entre as barras (0 a 1)
dados %>% 
        ggplot(aes(x = fase_dia, fill = tipo_pista)) + 
        geom_bar(position = position_dodge(preserve = "single"))

dados %>% 
        ggplot(aes(x = fase_dia, fill = tipo_pista)) + 
        geom_bar(position = position_dodge2(reverse = TRUE,
                                            preserve = "single",
                                            padding = 0.1))


# Para comparação relativa, use 'position = "fill"'
dados %>% 
        ggplot(aes(x = fase_dia, fill = tipo_pista)) + 
        geom_bar(position = "fill")


# Adicionando rótulos de dados ('stack')
# Com 'position = position_stack()" é possível controlar a posição do rótulo, mas note que no gráfico abaixo
# os rótulos das categorias com menor frequência ficam sobrepostos, dificultando a leitura
dados %>% 
        ggplot(aes(x = fase_dia, fill = tipo_pista)) + 
        geom_bar(position = position_stack()) + 
        geom_text(aes(label =..count..), stat = "count", position = position_stack(vjust = 0.5), size = 3)


# Nesses casos, se os rótulos forem de suma importância, utilize o gráfico de barras posicionadas lado a lado
# 'position_dodge'
# (não se esqueça de regular a posição dos rótulos também com position_dodge e valor = 0.9)
dados %>% 
        ggplot(aes(x = fase_dia, fill = tipo_pista)) + 
        geom_bar(position = "dodge2") + 
        geom_text(aes(label =..count..), stat = "count", position = position_dodge(width = 0.9),
                  size = 3, vjust = -0.3)


# Para inverter a ordem em que as categorias da variável secundária aparecem, use 
# position_stack(reverse = TRUE)
# não se esqueça de inverter também os rótulos 
dados %>% 
        ggplot(aes(x = fase_dia, fill = tipo_pista)) + 
        geom_bar(position = position_stack(reverse = TRUE)) + 
        geom_text(aes(label =..count..), stat = "count", position = position_stack(vjust = 0.5,
                                                                                   reverse = TRUE), size = 3)




# Para analisar a composição interna das categorias, utilize 'position_fill'
# e insira rótulos de dados. Utilize o dplyr para calcular os valores absolutos e %

dados %>% 
        group_by(fase_dia) %>% 
        count(tipo_pista) %>% 
        mutate(Prop = round(n/sum(n), digits = 3)*100) %>% 
        ggplot(aes(x = fase_dia, y = Prop, fill = tipo_pista, label = Prop)) + 
        geom_col(position = position_fill()) +
        geom_text(aes(y = Prop), position = position_fill(vjust = 0.5)) +
        scale_y_continuous(labels = scales::percent)


dados %>% 
        group_by(fase_dia) %>% 
        count(tipo_pista) %>% 
        mutate(Prop = round(n/sum(n), digits = 3)*100) %>% 
        ggplot(aes(x = fase_dia, y = Prop, fill = tipo_pista, label = Prop)) + 
        geom_col(position = position_fill()) +
        geom_label(aes(y = Prop), position = position_fill(vjust = 0.5)) +
        scale_y_continuous(labels = scales::percent)


dados %>% 
        group_by(fase_dia) %>% 
        count(tipo_pista) %>% 
        mutate(Prop = round(n/sum(n), digits = 3)*100) %>% 
        ggplot(aes(x = fase_dia, y = Prop, fill = tipo_pista, label = Prop)) + 
        geom_col(position = position_fill()) +
        geom_label(aes(y = Prop), color = "white", position = position_fill(vjust = 0.5)) +
        scale_y_continuous(labels = scales::percent)


# Para reordenar as categorias, use scale_x_discrete

dados %>% 
        ggplot(aes(x = fase_dia, group = tipo_pista)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
        geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count",
                  vjust = -0.2, size = 3) + 
        scale_x_discrete(limits = c("Amanhecer", "Pleno dia", "Anoitecer", "Plena Noite"))


dados %>% 
        ggplot(aes(x = fase_dia, group = tipo_pista)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
        geom_label(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count",
                  vjust = -0.2, size = 3) + 
        scale_x_discrete(limits = c("Amanhecer", "Pleno dia", "Anoitecer", "Plena Noite"))




dados %>% 
        ggplot(aes(x = fase_dia, y = pessoas, fill = tipo_pista)) + 
        geom_bar(stat = "identity")


dados %>% 
        group_by(br) %>% 
        summarise(total = sum(pessoas))

