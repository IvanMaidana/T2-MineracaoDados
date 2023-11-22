#Miguel Miron Silva - Trabalho II Mineração de Dados

library(dplyr)
library(ggplot2)
library(rpart)
library(caret)

data <- read.csv("http://www-usr.inf.ufsm.br/~joaquim/UFSM/DM/TF_2023%20-%20FII.csv")

# Substitua todos os valores "N/A" por NA no data frame
data1 <- mutate_all(data, function(x) ifelse(x == "N/A", NA, x))

#verifica a porcentagem de NA por cada coluna
data2 <- round(colSums(is.na(data1))*100/nrow(data1), 2)

#exclui as colunas que tem a maioria dos valores como NA
data1 = data1[, -c(19, 20, 21)]

# Função que converte um número em string que contenha caracteres específicos para valores numéricos
toNumeric <- function(x){
  y <- x
  y <- gsub("\\s", "", y, fixed = TRUE)
  y <- gsub(".", "", y, fixed = TRUE)
  y <- gsub(",", "", y, fixed = TRUE)
  y <- gsub("%", "", y, fixed = TRUE)
  as.numeric(y)/100
}

# Converter os valores em string para valores numéricos
data1[, 3:ncol(data1)] <- apply(data1[, 3:ncol(data1)], MARGIN = c(1,2), toNumeric)
data1$QUANT..ATIVOS <- data1$QUANT..ATIVOS * 100

# Crie um histograma com base nos dados da coluna "P.VP"
ggplot(data1, aes(x = P.VP)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de P.VP", x = "P.VP", y = "Contagem")

#Função para registrar todos os nomes diferentes
filtrar_nomes <- function(df, coluna) {
  nomes <- c()
  for (i in 1:nrow(df)) {
    nome <- df[i, coluna]
    if (!nome %in% nomes) {
      nomes <- c(nomes, nome)
    }
  }
  return(nomes)
}

listanomes <- filtrar_nomes(data1, 2)

localizar_na_nan_inf <- function(df) {
  resultados <- list()
  for (i in 1:5) {
    coluna <- df[, i]
    resultados[[i]] <- c(
      sum(is.na(coluna)),
      sum(is.nan(coluna)),
      sum(is.infinite(coluna))
    )
  }
  return(resultados)
}

# Separa os dados entre um dataset com dados já classificados e um sem classificação
data2 <- data1[,c("TIPO", "P.VP", "DY..12M..MÉDIA", "QUANT..ATIVOS", "NUM..COTISTAS")]
data2 <- na.omit(data2)
data3 <- tail(data2, 5)
data2 <- head(data2, nrow(data2)-5)
data2 <- data2 %>% mutate_if(is.character, as.factor)
data4 <- head(data1, nrow(data1)-5)

#Agrupamentos para ajudar a legibilidade dos dados

patrimonial_data2 <- kmeans(data2[,c(2,3)],5)
patrimonial2_data2 <- kmeans(data2[,c(3,4)],5)
patrimonial3_data2 <- kmeans(data2[,c(4,5)],5)
plot(data2[,c(2,3)], col = patrimonial_data2$patrimonial_data2, pch = 3, cex = 1)
#mercado_financeiro = t(as.matrix(data4))
#mercado_financeiro.cor <- as.dist(1 - cor(t(mercado_financeiro)))
#hierarq_financ.cor <- hclust(mercado_financeiro.cor, method = "complete", members=NULL)
#plot(hierarq_financ.cor, ylab = "Altura", xlab = "Grupos de acoes", main = NULL)

# Divide em um conjunto de testes e outro de treino
set.seed(245)
data_rows <- floor(0.80 * nrow(data2))
train_indices <- sample(c(1:nrow(data2)), data_rows)
train_data <- data2[train_indices,]
test_data <- data2[-train_indices,]

#Criacao do modelo de classificacao
modelo_Rpart <- rpart(TIPO ~ P.VP + DY..12M..MÉDIA + QUANT..ATIVOS + NUM..COTISTAS, data=train_data, method = "class")
summary(modelo_Rpart)
print(modelo_Rpart)

#Realizacao das predicoes no dataset
predict(modelo_Rpart, test_data)
plot(modelo_Rpart)


#Plotagem da importancia das variaveis
var_rpart <- varImp(modelo_Rpart)
print(var_rpart)
plot(var_rpart)

#predicao dos dados novos
predict(modelo_Rpart, data3)
#Papel, papel, tijolo e tijolo

