# Instale as bibliotecas necessárias
# install.packages("tidyverse")

# Carregue as bibliotecas necessárias
library(caret)

# Carregue o conjunto de dados
raw_data <- read.csv("http://www-usr.inf.ufsm.br/~joaquim/UFSM/DM/TF_2023%20-%20FII.csv")
fii_data <- raw_data

# Função que converte valores em string para numérico
format_to_numeric <- function(x){
  as.numeric(gsub(",", "", gsub("\\.", "", gsub("\\s", "", gsub("%", "", x)))))/100
}
# Converte todos "N/A" em NA
fii_data <- mutate_all(fii_data, function(x) ifelse(x == "N/A", NA, x))
# Converte todos os campos a partir da 3ª coluna para numérico
fii_data[, 3:ncol(fii_data)] <- apply(fii_data[, 3:ncol(fii_data)], MARGIN = c(1,2), format_to_numeric)
# Conserta a conversão dos campos inteiros
fii_data$QUANT..ATIVOS <- as.integer(fii_data$QUANT..ATIVOS * 100)
fii_data$NUM..COTISTAS <- as.integer(fii_data$NUM..COTISTAS * 100)
# Substitui "?" por NA
fii_data$TIPO[fii_data$TIPO == "?"] <- NA
# Transforma tipo em um factor
fii_data <- fii_data %>% mutate_if(is.character, as.factor)
# Remove as 3 últimas colunas, pois possuem apenas valores NA
fii_data = fii_data[, -c(19, 20, 21)]

# Explore os dados
str(fii_data)  # Checa a estrutura do dataset
summary(fii_data)  # Estatísticas resumidas do dataset
sapply(fii_data, function(x) sum(is.na(x)))  # Checa valores faltantes

# Visualiza caracteristicas
# Exemplo: Matriz scatterplot para as variáveis selecionadas
pairs(~P.VP + DY..12M..MÉDIA + QUANT..ATIVOS + NUM..COTISTAS + VARIAÇÃO.PATRIMONIAL, data = fii_data, col=fii_data$TIPO, pch=16)

# Plota a frequência de cada tipo
fii_data %>% 
  ggplot(aes(x = TIPO)) + 
  geom_bar(fill = "blue", color = "black", alpha = 0.7) + 
  labs(title = "Frequência de cada TIPO", x = "TIPO", y = "Frequency")


## PRÉ PROCESSAMENTO

# Lidando com valores faltantes
# Exclui as 2 primeiras colunas da checagem
colunas <- colnames(fii_data)[3:ncol(fii_data)]

# Substitui os valores faltantes de um tipo com a média do tipo para o valor.
fii_data_imputed <- fii_data %>%
  group_by(TIPO) %>%
  mutate(across(all_of(colunas), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

# Função para tratar outliers de cada grupo
treat_outliers_within_group <- function(df, column) {
  df %>%
    group_by(TIPO) %>%
    mutate(
      across(all_of(column), function(x) {
        qnt <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
        lower <- qnt[1] - 1.5 * IQR(x, na.rm = TRUE)
        upper <- qnt[2] + 1.5 * IQR(x, na.rm = TRUE)
        replace(x, x < lower | x > upper, median(x, na.rm = TRUE))
      })
    ) %>%
    ungroup()
}

# Trata outliers para cada grupo
fii_data_outliers_treated <- fii_data_imputed
for (col in colunas) {
  fii_data_outliers_treated <- treat_outliers_within_group(fii_data_outliers_treated, col)
}

fii_data <- fii_data_outliers_treated

# Identifica as observações cujos tipos ainda são desconhecidos
unknown_assets <- fii_data[is.na(fii_data$TIPO), ]
fii_data <- na.omit(fii_data)

shopping_rows <- filter(fii_data, TIPO == "SHOPPING")
fii_data <- bind_rows(fii_data, shopping_rows)

shopping_rows <- filter(fii_data, TIPO == "MISTO")
fii_data <- bind_rows(fii_data, shopping_rows)

fii_data

# Visualiza caracteristicas
# Exemplo: Matriz scatterplot para as variáveis selecionadas
pairs(~P.VP + DY..12M..MÉDIA + QUANT..ATIVOS + NUM..COTISTAS + VARIAÇÃO.PATRIMONIAL, data = fii_data, col=fii_data$TIPO, pch=16)

############################# GRAFICOS DE CORRELAÇÃO DE COLUNAS############################################################################


# Criar o gráfico de barras relação entre cotistas e o tipo do fundo de investimento
grafico <- ggplot(fii_data, aes(x = TIPO, y = NUM..COTISTAS, fill = TIPO)) +
  geom_bar(stat = "identity") +
  labs(title = "Relação entre Tipo de Investimento e Número de Cotistas", x = "Tipo de Investimento", y = "Número de Cotistas") +
  theme_minimal()
# Exibir o gráfico
print(grafico)


# Criar o gráfico de barras relação entre Quantidade de ativos e o tipo do fundo de investimento
grafico <- ggplot(fii_data, aes(x = TIPO, y = QUANT..ATIVOS, fill = TIPO)) +
  geom_bar(stat = "identity") +
  labs(title = "Relação entre Tipo de Investimento quantidade de Ativos", x = "Tipo de Investimento", y = "Quantidade de Ativos") +
  theme_minimal()
# Exibir o gráfico
print(grafico)

##############################################################################################################################################


#######################CRIACAO DE ARVORE#######################################################
##########CLASSIFICAÇÃO USANDO RANDOM FOREST####################################################
# Carrega as bibliotecas necessárias para o modelo de classificação
library(caret)
library(randomForest)

# Seleciona as colunas relevantes para o modelo de classificação
model_data <- fii_data %>%
  select(TIPO, P.VP, DY..12M..MÉDIA,  QUANT..ATIVOS, NUM..COTISTAS, PREÇO.ATUAL..R.., PATRIMÔNIO.LÍQUIDO, VARIAÇÃO.PREÇO, DY.PATRIMONIAL, VARIAÇÃO.PATRIMONIAL, VOLATILIDADE)

model_data

# Divide os dados em conjuntos de treino e de teste
set.seed(100)
train_index <- createDataPartition(model_data$TIPO, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Define o controle da validação cruzada
cv_control <- trainControl(method = "cv", number = 8)  # 8-fold cross validation

# Treina o modelo de Random Forest utilizando validação cruzada
model <- train(
  TIPO ~ .,
  data = train_data,
  method = "rf",  # Random Forest
  trControl = cv_control
)

# Printar o modelo resultante após a validação cruzada
print(model)

# Realizar predições no dataset de treino
predictions <- predict(model, newdata = test_data)

# Avaliar o modelo, printar matriz de confusão
confusion_matrix <- confusionMatrix(predictions, test_data$TIPO)
print(confusion_matrix)

# Plotar a importância de cada variável no modelo
var_importance <- varImp(model)
print(var_importance)
plot(var_importance)

# Salvar o modelo de treinamento (opcional)
saveRDS(model, "fii_classification_model.rds")

## Predições

predictions_unknown <- predict(model, newdata = unknown_assets)
print(predictions_unknown)


###############CLASSIFICAÇÃO  USANDO O PACKAGE RPART######################################################
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("caret")
#install.packages("e1071")
# Carregar o pacote para criar o modelo da arvore
library(rpart)
#carrega o pacote que plota a arvore de forma mais vizual
library(rpart.plot)
library(caret)
#carrega o pacote para verificar accuracy
library(e1071)


# Seleciona as colunas relevantes para o modelo de classificação
model_data <- fii_data %>%
  select(TIPO, P.VP, DY..12M..MÉDIA,  QUANT..ATIVOS, NUM..COTISTAS, PREÇO.ATUAL..R.., PATRIMÔNIO.LÍQUIDO, VARIAÇÃO.PREÇO, DY.PATRIMONIAL, VARIAÇÃO.PATRIMONIAL, VOLATILIDADE)

model_data

# Divide os dados em conjuntos de treino e de teste
set.seed(100)
train_index <- createDataPartition(model_data$TIPO, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]


# Define o controle da validação cruzada
cv_control <- rpart.control(cp = 0.01, minsplit = 5, xval = 10)

# Treina o modelo de Árvore de Decisão com validação cruzada
modelo_arvore <- rpart(
  formula = TIPO ~.,
  data = train_data,
  control = cv_control
)


# Realiza predições nos dados de teste
previsoes <- predict(modelo_arvore, newdata = test_data[-1], type = 'class')
# Cria a matriz de confusão
matriz_confusao = table(test_data$TIPO , previsoes)
# Exibe a matriz de confusão
confusionMatrix(matriz_confusao)

# Plotar a importância de cada variável no modelo
var_importance <- varImp(modelo_arvore)
print(var_importance)
plot(var_importance)



#descobrir os valores da ultimas 5 colunas do dataframe
descoberta <- tail(unknown_assets, 5)
descoberta
previsoes <- predict(modelo_arvore, newdata = descoberta[-1], type = 'class')
previsoes




