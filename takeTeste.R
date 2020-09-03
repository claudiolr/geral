# Pacotes
require(tidyverse)
require(openxlsx)
require(lubridate)
require(janitor)
require(extrafont)
require(caret)

# Carrega fontes
loadfonts(device = "win")


# Função para padronizar gráficos
tema <- function(){
        theme(text = element_text(family = "Open Sans", colour = "gray30"),
              plot.title = element_text(size = 12,
                                        hjust = 0.5),
              plot.subtitle = element_text(size = 10,
                                           hjust = 0.5),
              legend.position = "none",
              axis.title = element_blank()
        )
}


# Analistas -----------------------------------------------------------------------------------

# Carrega e trata a base
analistas <- read.xlsx("C:/Users/Claudio/OneDrive/ProcSeletivoTake/Indicadores de Atendimento.xlsx",
                       sheet = "ANALISTAS")


colnames(analistas) = c("mes", "agente", "tickets.solved", "first.reply.time",
                        "full.resolution.time", "tickets.dentro.SLA.first.reply",
                        "tickets.dentro.SLA.full.reply", "satisfaction.score", "one.touch.tickets",
                        "tickets.reopened", "requester.wait.time", "agent.wait.time", "first.resolution.time",
                        "replies", "replies.avg", "tickets.updates")


# Transforma a variável mês em string
analistas <- analistas %>% 
        # transforma o valor numérico (originário do Excel) para um vetor de data
        mutate(mes = as_date(mes, origin = "1899-12-30"),
               # alterar o vetor de data para string com formato de mês
               mes = format(mes, "%m"))



# Análises

# Boxsplot de todas as variáveis quantitativas
analistas %>% 
        # empilha as variáveis para plotar boxplots
        pivot_longer(cols = -c(mes, agente), names_to = "variavel", values_to = "valor") %>% 
        # gera gráfico
        ggplot(aes(x = agente, y = valor, fill = agente)) +
        geom_boxplot() + 
        tema() + theme(legend.position = "none") +
        facet_wrap( ~ variavel, scales = "free", nrow = 5)



analistas %>% 
        # seleciona colunas
        select(agente, mes, tickets.solved, satisfaction.score) %>% 
        group_by(mes) %>% 
        # normaliza variáveis pelo método min-max scale
        mutate(ticketsNorm = (tickets.solved-min(tickets.solved))/(max(tickets.solved) - min(tickets.solved)),
               indicador = (ticketsNorm+satisfaction.score)/2) %>%
        # cria variável novembro para plotar no gráfico
        mutate(novembro = ifelse(mes == 11, indicador, NA)) %>% 
        # pivot_wider(names_from = mes, values_from = Media) %>% 
        ggplot(aes(x = agente)) +
        geom_point(aes(y = novembro, size = 10), shape = 19, colour = "darkred") +
        geom_boxplot(aes(y = indicador, fill = agente)) +
        labs(title = "Indicador geral de desempenho",
             subtitle = "('tickets solved' e 'satisfaction score')") +
        scale_y_continuous(limits = c(0, 1)) +
        tema()


# cria data.frame normalizado
analistasNorm <- analistas %>% 
        mutate(tickets.solved = (tickets.solved-min(tickets.solved))/(max(tickets.solved)-min(tickets.solved)),
               first.reply.time = (first.reply.time-min(first.reply.time))/(max(first.reply.time)-min(first.reply.time)),
               full.resolution.time = (full.resolution.time-min(full.resolution.time))/(max(full.resolution.time)-min(full.resolution.time)),
               tickets.dentro.SLA.first.reply = (tickets.dentro.SLA.first.reply-min(tickets.dentro.SLA.first.reply))/(max(tickets.dentro.SLA.first.reply)-min(tickets.dentro.SLA.first.reply)),
               tickets.dentro.SLA.full.reply = (tickets.dentro.SLA.full.reply-min(tickets.dentro.SLA.full.reply))/(max(tickets.dentro.SLA.full.reply)-min(tickets.dentro.SLA.full.reply)),
               requester.wait.time = (requester.wait.time-min(requester.wait.time))/(max(requester.wait.time)-min(requester.wait.time)),
               agent.wait.time = (agent.wait.time-min(agent.wait.time))/(max(agent.wait.time)-min(agent.wait.time)),
               first.resolution.time = (first.resolution.time-min(first.resolution.time))/(max(first.resolution.time)-min(first.resolution.time)),
               replies = (replies-min(replies))/(max(replies)-min(replies)),
               replies.avg = (replies.avg-min(replies.avg))/(max(replies.avg)-min(replies.avg)),
               tickets.updates = (tickets.updates-min(tickets.updates))/(max(tickets.updates)-min(tickets.updates)))


# cria indicador e gera gráfico
analistasNorm %>% 
        select(agente, mes, full.resolution.time,
               replies.avg, tickets.updates, one.touch.tickets,
               tickets.reopened, tickets.solved, satisfaction.score) %>% 
        mutate(indAtendimento = (full.resolution.time +
                                         replies.avg + 
                                         tickets.updates + 
                                         one.touch.tickets +
                                         tickets.solved +
                                         satisfaction.score)/6,
               novembro = ifelse(mes == 11, indAtendimento, NA)) %>% 
        ggplot(aes(x = agente, y = indAtendimento, fill = agente)) + 
        geom_boxplot() +
        geom_point(aes(y = novembro, size = 10), shape = 19, colour = "darkred") +
        labs(title = "Indicador de respostas (replies)") +
        scale_y_continuous(limits = c(0, 1)) +
        tema()




# Consolidado ---------------------------------------------------------------------------------

# Carrega e trata a base
consolidado <- read.xlsx("C:/Users/Claudio/OneDrive/ProcSeletivoTake/Indicadores de Atendimento.xlsx",
                         sheet = "CONSOLIDADO")

colnames(consolidado) <- c("mes", "tickets.created", "tickets.solved", "first.reply.time",
                           "full.resolution.time", "tickets.dentro.SLA.first.reply",
                           "tickets.dentro.SLA.full.reply", "satisfaction.score", "one.touch.tickets",
                           "tickets.reopened", "requester.wait.time", "agent.wait.time", "first.resolution.time",
                           "replies", "replies.avg", "tickets.updates")


consolidado <- consolidado %>% 
        mutate(mes = as_date(mes, origin = "1899-12-30"),
               mes = format(mes, "%m"),
               first.reply.time = as.numeric(str_replace(str_remove(first.reply.time, " hrs"), ",", ".")),
               full.resolution.time = as.numeric(str_replace(str_remove(full.resolution.time, " hrs"), ",", ".")),
               requester.wait.time = as.numeric(str_replace(str_remove(requester.wait.time, " hrs"), ",", ".")),
               agent.wait.time = as.numeric(str_replace(str_remove(agent.wait.time, " hrs"), ",", ".")),
               first.resolution.time = as.numeric(str_replace(str_remove(first.resolution.time, " hrs"), ",", ".")),
               one.touch.tickets = as.numeric(str_remove(one.touch.tickets, " %"))/100,
               replies.avg = as.numeric(str_replace(replies.avg, ",", ".")))

consolidado %>% 
        select(tickets.solved) %>% 
        summary(tickets.solved)

consolidado %>% 
        ggplot(aes(x = mes, y = tickets.solved, label = tickets.solved)) +
        geom_bar(stat = "identity") + 
        geom_text(aes(y = tickets.solved))


consolidado %>% 
        pivot_longer(cols = -c(mes), names_to = "variavel", values_to = "valor") %>% 
        ggplot(aes(x = mes, y = valor, fill = mes)) +
        geom_bar(stat = "identity") + 
        tema() + theme(legend.position = "none") +
        facet_wrap( ~ variavel, scales = "free", nrow = 5)