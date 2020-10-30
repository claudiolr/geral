require(tidyverse)
require(lubridate)
require(data.table)
require(corrplot)
require(modelr)
require(car)


# Carrega a base
airbnb <- fread("C:/Users/Claudio/OneDrive/Documentos/ProcessosSeletivos/Cinnecta/airbnb.csv")

# Visão geral sobre os dados
glimpse(dados)


# Personaliza o tema dos gráficos
extrafont::loadfonts(device = "win", quiet = FALSE) # carrega fontes do windows

cinza <- "#4d4d4d" # padroniza a cor dos textos dos gráficos


# função para denifir padrões de tema dos gráficos do ggplot
tema <- function(top = 3, right = 5, bottom = -3, left = 5, legenda = "none"){
        theme(text = element_text(family = "Open Sans", colour = cinza),
              plot.title = element_text(size = 12, family = "Open Sans SemiBold", hjust = 0.5),
              plot.subtitle = element_text(size = 11, hjust = 0.5),
              axis.ticks = element_blank(),
              axis.title = element_text(colour = "gray40"),
              axis.text = element_text(size = 10),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x =  element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = legenda,
              legend.margin = margin(-2,0,15,0),
              legend.box.margin = margin(-5,-5,-5,-5),
              plot.caption.position = "plot",
              legend.text = element_text(size = 9),
              plot.margin = margin(top, right, bottom, left))
}

summary(airbnb)

# Seleção e transformação dos dados -----------------------------------------------------------

# Seleciona as variáveis de interesse (exclui variáveis identificadoras e variáveis '_na')
dados <- airbnb %>% 
     select(-c(latitude, longitude, bedrooms_na:review_scores_value_na))


# transforma variáveis 'boolean'
dados <- dados %>% 
        mutate(host_is_superhost = case_when(host_is_superhost == "t" ~ 1,
                                             TRUE ~ 0),
               instant_bookable = case_when(instant_bookable == "t" ~ 1,
                                             TRUE ~ 0))


# Análise descritiva --------------------------------------------------------------------------

# Políticas e serviços das hospedagens

dados %>% 
        # transforma variável em categórica (factor)
        mutate(host_is_superhost = as.factor(host_is_superhost)) %>%
        count(host_is_superhost, name = 'N') %>% # conta valores
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% # cria variável de %
        # gráfico
        ggplot(aes(x = host_is_superhost, y = Prop, fill = host_is_superhost, label = N)) +
        geom_col(col = 'white') + # colunas contorno branco
        # títulos
        labs(title = "Anfitrião é 'superhost'?",
             subtitle = "(anfitriões experientes)",
             x = "", y = "%") +
        geom_text(aes(y = Prop), vjust = -0.2, colour = cinza) + # rótulos dos dados
        scale_x_discrete(labels = c('Não', 'Sim')) + # rótulos das colunas
        scale_fill_brewer(palette = 'Accent') + # padrão de cores
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 65)) + # intervalor do eixo y
        tema(left = 5) +
        theme(legend.position = 'none')

        
dados %>% 
        count(cancellation_policy, name = 'N') %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = cancellation_policy, y = Prop, fill = cancellation_policy, label = N)) +
        geom_col(position = position_dodge()) + 
        labs(title = "Políticas de cancelamento da reserva",
             x = "", y = "%") +
        geom_text(aes(y = Prop), hjust = -0.2, colour = cinza) +
        scale_x_discrete(labels = c('Flexível', 'Modedada', 'Rigorosa', 'Loga duração',
                                    'Super rigorosa (30 dias)', 'Super rigorosa (60 dias)')) +
        scale_fill_brewer(palette = 'Paired') +
        scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 45)) +
        tema(left = 5) + 
        coord_flip()

dados %>% 
        select(minimum_nights) %>% 
        pivot_longer(cols = minimum_nights, names_to = 'variavel', values_to = 'N') %>% 
        ggplot(aes(y = N, fill = variavel)) +
        geom_boxplot() + 
        labs(title = "Tempo mínimo de hospedagem (noites)",
             x = "", y = "N") + 
        scale_x_discrete(labels = c('Mínimo de noites')) + 
        scale_y_continuous(breaks = seq(0, 400, 20)) +
        tema() + 
        theme(axis.text.x = element_text())

dados %>% 
        select(minimum_nights) %>% 
        summary()

# Características da hospedagem
dados %>% 
        count(property_type, cancellation_policy, name = 'N') %>% 
        mutate(property_type = case_when(N < 150 ~ 'outros',
                                         TRUE ~ property_type)) %>%
        group_by(property_type, cancellation_policy) %>% 
        summarise(N = sum(N)) %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = property_type, y = Prop, fill = cancellation_policy, label = N)) +
        geom_col(position = position_stack()) + 
        labs(title = "Tipos de propriedade",
             x = "", y = "%") +
        # geom_text(aes(y = Prop), vjust = -0.2, colour = cinza) +
        scale_fill_discrete(labels = c('Flexível', 'Modedada', 'Rigorosa', 'Loga duração',
                                    'Super rigorosa (30 dias)', 'Super rigorosa (60 dias)'),
                            aesthetics = palette()) +
        # scale_fill_brewer(palette = 'Paired') +
        # scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema(left = 5, legenda = "bottom") + 
        coord_flip()


dados %>% 
        count(room_type, name = 'N') %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = room_type, y = Prop, fill = room_type, label = N)) +
        geom_col(position = position_dodge()) + 
        labs(title = "Tipos de acomodação",
             x = "", y = "%") +
        geom_text(aes(y = Prop), vjust = -0.2, colour = cinza) +
        scale_x_discrete(labels = c('Lugar inteiro', 'Quarto inteiro', 'Quarto compartilhado')) +
        scale_fill_brewer(palette = 'Paired') +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 70)) +
        tema(left = 5, legenda = "none")

dados %>% 
        count(room_type, cancellation_policy, name = 'N') %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = room_type, y = Prop, fill = cancellation_policy, label = N)) +
        geom_col(position = position_fill(), col = "white") + 
        labs(title = "Políticas de cancelamento por tipos de acomodação",
             x = "", y = "%") +
        # geom_text(aes(y = Prop), vjust = -0.2, colour = cinza) +
        scale_x_discrete(labels = c('Lugar inteiro', 'Quarto inteiro', 'Quarto compartilhado')) +
        scale_fill_discrete(labels = c('Flexível', 'Modedada', 'Rigorosa', 'Loga duração',
                                       'Super rigorosa (30 dias)', 'Super rigorosa (60 dias)')) +
        # scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 70)) +
        tema(left = 5, legenda = "bottom")


dados %>% 
        count(bed_type, name = 'N') %>% 
        mutate(Prop = round(N/sum(N), digits = 3)*100) %>% 
        ggplot(aes(x = bed_type, y = Prop, fill = bed_type, label = N)) +
        geom_col(position = position_dodge()) + 
        labs(title = "Tipos de cama",
             x = "", y = "%") +
        geom_text(aes(y = Prop), vjust = -0.2, colour = cinza) +
        scale_x_discrete(labels = c('Colchão de ar', 'Sofá', 'Futon', 'Sofá-cama', 'Cama')) +
        scale_fill_brewer(palette = 'Paired') +
        scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
        tema(left = 5) +
        theme(legend.position = 'none')


dados %>% 
        select(accommodates:beds) %>% 
        pivot_longer(cols = 1:4, names_to = 'variavel', values_to = 'N') %>% 
        ggplot(aes(x = variavel, y = N, fill = variavel)) +
        geom_boxplot() +
        labs(title = "Infraestrutura da hospedagem",
             x = "", y = "") +
        scale_x_discrete(labels = c('Hóspedes', 'Banheiros', 'Quartos', 'Camas')) +
        scale_fill_brewer(palette = 'Paired') +
        scale_y_continuous(breaks = seq(0, 100, 1), limits = c(0, 15)) +
        tema(left = 5) +
        theme(legend.position = 'none')


# Avaliações

dados %>% 
        select(review_scores_accuracy, review_scores_cleanliness, review_scores_checkin,
               review_scores_communication, review_scores_location, review_scores_value,
               review_scores_rating) %>% 
        summary()


dados %>% 
        select(review_scores_accuracy, review_scores_cleanliness, review_scores_checkin,
               review_scores_communication, review_scores_location, review_scores_value) %>% 
        pivot_longer(cols = 1:6, names_to = 'variavel', values_to = 'N') %>% 
        ggplot(aes(x = N, fill = N)) + 
        geom_histogram(binwidth = 1) + 
        labs(title = "Histogramas das avaliações específicas",
             x = "nota", y = "") +
        facet_wrap(~ variavel) +
        scale_x_continuous(breaks = seq(0, 10, 1)) + 
        tema(bottom = 3)


dados %>% 
        select(number_of_reviews) %>% summary()

quantile(dados$number_of_reviews)
mean(dados$number_of_reviews)

g1 <- dados %>% 
        select(number_of_reviews) %>% 
        pivot_longer(cols = 1, names_to = 'variavel', values_to = 'N') %>%
        ggplot(aes(x = N)) + 
        geom_histogram(col = "white", fill = "gray25", binwidth = 10) + 
        labs(title = "Histograma do número de avaliações",
             x = "", y = "") +
        scale_x_continuous(breaks = seq(0, 800, 100)) +
        scale_y_continuous(breaks = seq(0, 3000, 200)) +
        tema()

g2 <- dados %>% 
        select(number_of_reviews) %>% 
        pivot_longer(cols = 1, names_to = 'variavel', values_to = 'N') %>%
        ggplot(aes(x = variavel, y = N, fill = variavel)) + 
        geom_boxplot() + 
        labs(title = "Boxplot do número de avaliações",
             x = "", y = "") + 
        scale_y_continuous(breaks = seq(0, 700, 50)) +
        tema()

gridExtra::grid.arrange(g1, g2, ncol = 2)


# Preço

dados %>% 
        select(price) %>% 
        summary()


g3 <- dados %>% 
        ggplot(aes(x = price)) + 
        geom_histogram(binwidth = 100, col = "white") +
        labs(title = "Histograma de preços",
             x = "preço", y = "") + 
        tema()


g4 <- dados %>% 
        select(price) %>% 
        pivot_longer(cols = 1, names_to = "variavel", values_to = "N") %>% 
        ggplot(aes(x = variavel, y = N, fill = variavel)) +
        geom_boxplot() + 
        labs(title = "Boxplot de preços",
             x = "", y = "preço") + 
        tema

gridExtra::grid.arrange(g3, g4, ncol = 2)

# Análises bivariadas -------------------------------------------------------------------------

# Número de avaliações, avaliação geral
dados %>% 
        mutate(host_is_superhost = as.factor(host_is_superhost)) %>% 
        ggplot(aes(x = number_of_reviews, y = review_scores_rating)) + 
        geom_point(aes(colour = host_is_superhost)) + 
        labs(title = "Número total e valor geral das avaliações, por tipo de anfitrião",
             x = "número de avaliações", y = "avaliação geral") + 
        scale_colour_discrete(labels = c("Não", "Sim"), name = "Anfitrião é 'superhost'") + 
        tema(legenda = "bottom")


dados %>% 
        mutate(priceQ = ntile(price, 10),
               host_is_superhost = as.factor(host_is_superhost)) %>% 
        filter(priceQ != 10) %>% 
        ggplot(aes(x = price, y = number_of_reviews)) + 
        geom_point(aes(colour = host_is_superhost), alpha = 0.5) + 
        labs(title = "Preço e número de avaliações, por tipo de anfitrião",
             x = "preço", y = "número de avaliações") + 
        scale_colour_discrete(labels = c("Não", "Sim"), name = "Anfitrião é 'superhost'") + 
        tema(legenda = "bottom")


# Preço, número de avaliações e avaliação geral
dados %>% 
        mutate(priceQ = ntile(price, 10)) %>% 
        filter(priceQ != 10) %>% 
        ggplot(aes(x = price, y = number_of_reviews)) + 
        geom_point(aes(colour = number_of_reviews), alpha = 0.2) + 
        scale_colour_continuous(low = "blue", high = "red")

        theme(legend.position = 'none')


# Preço, número de avaliações e avaliação geral
dados %>% 
        select(price, number_of_reviews, review_scores_rating) %>%
        mutate(priceQ = ntile(price, 10)) %>% 
        filter(priceQ != 10) %>% 
        ggplot(aes(x = price, y = review_scores_rating)) + 
        geom_point(aes(colour = number_of_reviews, size = number_of_reviews), alpha = 0.5) +
        labs(title = "Preço da acomodação pela avaliação geral e pelo número de avaliações",
             x = "preço", y = "avaliação geral") + 
        scale_color_gradient(low = "blue", high = "red", name = "Número de avaliações") + 
        scale_size_continuous(name = "Número de avaliações") +
        tema(bottom = 3, legenda = "right")


# Preço, avaliação do preço
dados %>%
        ggplot(aes(x = price, y = review_scores_value, colour = number_of_reviews)) + 
        geom_point()

# Preço, acomodações
dados %>% 
        mutate(priceQ = ntile(price, 4)) %>% 
        filter(priceQ != 4) %>% 
        ggplot(aes(x = price, y = minimum_nights, colour = number_of_reviews)) +
        geom_point(alpha = 0.5) + 
        scale_y_continuous(breaks = seq(0, 400, 20))


# Preço, localidade
dados %>% 
        ggplot(aes(x = price, y = review_scores_location, colour = number_of_reviews)) + 
        geom_point()


# Preço e tipo de anfitrião
dados %>% 
        mutate(host_is_superhost = as.factor(host_is_superhost)) %>%
        ggplot(aes(x = price, y = review_scores_rating,
                   colour = host_is_superhost)) + 
        geom_point(alpha = 0.5) + 
        labs(title = "Avaliação pelo preço da hospedagem e pelo tipo de anfitrião",
             x = "preço", y = "avaliação", fill = "tipo de anfitrião") + 
        scale_color_discrete(name = "Anfitrião é 'superhost'", labels = c('Não', 'Sim')) + 
        tema(bottom = 5, legenda = "right")



# Predição ------------------------------------------------------------------------------------

# Tipo 1: avaliação como variável dependente
modelo <- dados %>% 
        mutate(host_is_superhost = as.factor(host_is_superhost),
               instant_bookable = as.factor(instant_bookable))

modelo1 <- lm(review_scores_rating ~ price + host_is_superhost +cancellation_policy + instant_bookable + 
                      property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type +
              minimum_nights + number_of_reviews + review_scores_rating, 
              data = modelo)

Anova(modelo1, type = 3)
anova_alt(modelo1)
summary(modelo1)
modelo1$



modelo2 <- lm(review_scores_rating ~ price + host_is_superhost + room_type + 
                      bathrooms + bedrooms + beds + minimum_nights + number_of_reviews, 
               data = modelo)

anova_alt(modelo2)
anova(modelo2)
summary(modelo2)


# Tipo 2: 'preço' como variável dependente
modelo3 <- lm(price ~ review_scores_rating + host_is_superhost +cancellation_policy + instant_bookable + 
                      property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type +
                      minimum_nights + number_of_reviews, 
              data = modelo)

Anova(modelo3, type = 3)


modelo4 <- lm(price ~ review_scores_rating + room_type + accommodates + bathrooms + 
                      bedrooms + minimum_nights + number_of_reviews, 
              data = modelo)

Anova(modelo4, type = 3)
summary(modelo4)


modelo$predito_modelo4 <- predict(modelo4, newdata = modelo)

modelo %>% select(price, predito_modelo4) %>% head(15)

media <- mean((modelo$price - modelo$predito_modelo4)^2)

sqrt(media)


# Tipo 3: 'host' como variável dependente

modelo5 <- glm(host_is_superhost ~ review_scores_rating + cancellation_policy + instant_bookable + 
                       property_type + room_type + accommodates + bathrooms + bedrooms + beds + bed_type +
                       minimum_nights + number_of_reviews + price,
               family = binomial(link = 'logit'), data = modelo)

Anova(modelo5, type = 3)


modelo6 <- glm(host_is_superhost ~ review_scores_rating + cancellation_policy + instant_bookable + 
                       room_type + minimum_nights + number_of_reviews,
               family = binomial(link = 'logit'), data = modelo)

Anova(modelo6, type = 3)
summary(modelo6)
anova_alt(modelo6)


# Tipo 4: 