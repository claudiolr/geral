require(tidyverse)
require(data.table)
require(datasus)


options(scipen = 999)



# Base do DataSUS (óbitos por data da ocorrência) ---------------------------------------------
url <- 'https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD-19-10-2020.csv'
baseSUS <- fread(url, sep2 = ";", header = TRUE)

ocorrencia <- baseSUS %>% 
        filter(CLASSI_FIN == 5 & EVOLUCAO == 2) %>% 
        select(DT_EVOLUCA) %>% 
        mutate(
                # DT_ENCERRA = as.Date(DT_ENCERRA, format = "%d/%m/%Y"),
                data = as.Date(DT_EVOLUCA, format = "%d/%m/%Y"),
                # Tempo = DT_ENCERRA - DT_EVOLUCA
                ) %>% 
        # pivot_longer(cols = c(DT_ENCERRA, DT_EVOLUCA), names_to = "Tipo", values_to = "data") %>% 
        count(data, name = "Ocorrências") %>% 
        filter(!is.na(data))



# Base do Brasil IO (óbitos por data da notificação) ------------------------------------------
brasilIO <- data.table::fread("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", encoding = "UTF-8")


notificaoes <- brasilIO %>% 
        filter(place_type == "state") %>% 
        # seleciona colunas de interesse renomeando cada uma delas
        select(data = date,
               # UF = state,
               # municipio = city,
               # codIBGE = city_ibge_code,
               # novosCasos = new_confirmed,
               novosObitos = new_deaths,
               # casos = last_available_confirmed,
               # obitos = last_available_deaths,
               # is_last = is_last,
               # popEstimada2019 = estimated_population_2019,
               # casos100k = last_available_confirmed_per_100k_inhabitants,
               # taxaObito = last_available_death_rate
        ) %>% 
        mutate(data = as.Date(data)) %>% 
        group_by(data) %>% 
        summarise(Notificações = sum(novosObitos))

        
        

# Análise -------------------------------------------------------------------------------------

comparacao <- notificaoes %>% 
        full_join(ocorrencia, by = 'data')

data <- max(comparacao$data)
data <- format(data, "%d de %B")

comparacao %>%
        pivot_longer(cols = c(Notificações, Ocorrências), names_to = "Tipo", values_to = "N") %>% 
        ggplot(aes(x = data, y = N, fill = Tipo)) + 
        geom_col(position = "dodge") +
        labs(title = "Óbitos por Covid-19 pela data de notificação e pela data da ocorrência",
             subtitle = paste0("(", data, ")"),
             x = "", y = "", fill = "",
             caption = "Fontes: Brasil.io (notificações) e DataSUS (ocorrências)") +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b") +
        scale_fill_manual(values = c("red", "blue"), labels = c("Data da notificação", "Data da ocorrência")) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 12),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              plot.caption = element_text(hjust = 1),
              legend.position = "bottom")


comparacao %>% 
        filter(max(Ocorrências))
