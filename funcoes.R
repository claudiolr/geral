


# Funções -----------------------------------------------------------------

# For()

for(i in pessoas$`2.2.9`)
{
     print(pessoas$`2.2.9`[i])
}



for(i in x)
{
     print("Obrigado")
}


x = 1
while(x<=10)
{
     print(x)
     x = x+1
}


media <- function(x)
{
     m <- mean(x)
     print(m)
     cat("A média dos valores é:",m,".")
}



media(notas)


notas <- scan()



variancia <- function(x) {
     media <- mean(x)
     n <- length(x)
     var <- sum((x - media)^2)/n
     return(var)
}


variancia(notas)



megasena <- function(njogos)
{ # cria a função com nome de megasena
     numeros <- matrix(NA,6,njogos) # cria o arquivo que recebe os jogos
     for(i in 1:njogos)
     {
          numeros[,i] <- sample(1:60,6)
     }
     return(numeros)
}

megasena(3)


###########################################################################
### Atividade prática -------------------------------------------------------
# Cadastrar alunos
# Incluir as notas
# Mostrar a relação dos alunos aprovados e reprovados
# Mostrar a média das notas dos alunos


cadastro <- function(z)
{
     Nome <- as.character()
     Prova1 <- as.numeric()
     Prova2 <- as.numeric()
     Prova3 <- as.numeric()
     MediaEscolar <- as.numeric()
     Situacao <- as.character()
     
     alunos <- data.frame(Nome, Prova1, Prova2, Prova3, MediaEscolar, Situacao)
     alunos$Situacao <- as.character(alunos$Situacao)
     alunos$Nome <- as.character(alunos$Nome)
     
     print("Esta função realiza as seguintes atividades:")
     print("1. Cadastra alunos;")
     print("2. Recebe suas notas;")
     print("3. Mostra a média do aluno e retorna a sua situação (aprovado ou reprovado)")
     
     # Recebe notas do aluno i nas três provas
     print("Cadastro de alunos (digite 1)")
     x <- scan(n=1)
     i <- 1
     
     while(x==1)
     {
          print("Nome:")
          Nome <- scan(what = character(), nmax = 1)
          
          print("Nota da prova 1:")
          Prova1 <- scan(n=1)
          
          print("Nota da prova 2:")
          Prova2 <- scan(n=1)
          
          print("Nota da prova 3:")
          Prova3 <- scan(n=1)
          
          notas <- c(Prova1, Prova2, Prova3)
          
          for(j in 1:length(notas))
          {
               cat("Nota da prova",j,":")
               cat("", notas[j],"\n")
          }
          
          MediaEscolar <- mean(notas)
          cat("Média do aluno:",MediaEscolar,"\n")
          
          if (MediaEscolar>=7)
          {
               print("Aluno aprovado")
               Situacao <- "Aprovado"
          }
          
          if (MediaEscolar<7)
          {
               print("Aluno reprovado")
               Situacao <- "Reprovado"
          }
          
          alunos[i,] <- c(Nome,Prova1,Prova2,Prova3,MediaEscolar,Situacao)
          print("Cadastrar outro aluno? Sim -> 1 Não -> 0")
          x <- scan(n=1)
          
          i <- i+1
     } # fim do while
     
     print("Fim do cadastro")
     print("Banco de dados:")
     
     return(alunos)
     
}


