# Instale as bibliotecas necessárias
# install.packages("tidyverse")

# Carregue as bibliotecas necessárias
library(caret)
library(caretEnsemble)

# Carregue o conjunto de dados
raw_data <- read.csv("http://www-usr.inf.ufsm.br/~joaquim/UFSM/DM/TF_2023%20-%20FII.csv")
fii_data <- raw_data

format_to_numeric <- function(x){
  as.numeric(gsub(",", "", gsub("\\.", "", gsub("\\s", "", gsub("%", "", x)))))/100
}
fii_data <- mutate_all(fii_data, function(x) ifelse(x == "N/A", NA, x))
fii_data[, 3:ncol(fii_data)] <- apply(fii_data[, 3:ncol(fii_data)], MARGIN = c(1,2), format_to_numeric)
fii_data$QUANT..ATIVOS <- as.integer(fii_data$QUANT..ATIVOS * 100)
fii_data$NUM..COTISTAS <- as.integer(fii_data$NUM..COTISTAS * 100)
fii_data$TIPO[fii_data$TIPO == "?"] <- NA
fii_data <- fii_data %>% mutate_if(is.character, as.factor)
fii_data = fii_data[, -c(19, 20, 21)]

# Explore os dados
str(fii_data)  # Checa a estrutura do dataset
summary(fii_data)  # Estatísticas resumidas do dataset
sapply(fii_data, function(x) sum(is.na(x)))  # Checa valores faltantes

# Visualiza caracteristicas
# Exemplo: Matriz scatterplot para as variáveis selecionadas
selected_vars <- c("P.VP", "DIVIDEND.YIELD", "DY..12M..ACUMULADO", "VARIAÇÃO.PREÇO", "QUANT..ATIVOS", "NUM..COTISTAS", "VOLATILIDADE")
pairs(~P.VP + DIVIDEND.YIELD + DY..12M..ACUMULADO + VARIAÇÃO.PREÇO + QUANT..ATIVOS + NUM..COTISTAS + VOLATILIDADE, data = fii_data, col=fii_data$TIPO, pch=16)

## PRÉ PROCESSAMENTO

# Handling Missing Values
# Exclude the first two columns from imputation
columns_to_impute <- colnames(fii_data)[3:ncol(fii_data)]

# Impute missing values with the mean for each "TIPO"
fii_data_imputed <- fii_data %>%
  group_by(TIPO) %>%
  mutate(across(all_of(columns_to_impute), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

# Identify columns for outlier treatment
columns_to_treat_outliers <- colnames(fii_data)[3:ncol(fii_data)]

# Function to treat outliers within each group
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

# Treat outliers within each group
fii_data_outliers_treated <- fii_data_imputed
#for (col in columns_to_treat_outliers) {
#  fii_data_outliers_treated <- treat_outliers_within_group(fii_data_outliers_treated, col)
#}

# CLASSIFICAÇÃO

fii_data <- fii_data_outliers_treated

# Visualiza caracteristicas
# Exemplo: Matriz scatterplot para as variáveis selecionadas
selected_vars <- c("P.VP", "DIVIDEND.YIELD", "DY..12M..ACUMULADO", "VARIAÇÃO.PREÇO", "QUANT..ATIVOS", "NUM..COTISTAS", "VOLATILIDADE")
pairs(~P.VP + DIVIDEND.YIELD + DY..12M..ACUMULADO + VARIAÇÃO.PREÇO + QUANT..ATIVOS + NUM..COTISTAS + VOLATILIDADE, data = fii_data, col=fii_data$TIPO, pch=16)

# Carrega as bibliotecas necessárias para o modelo de classificação
library(caret)
library(randomForest)

# Identifica as observações cujos tipos ainda são desconhecidos
unknown_assets <- fii_data[is.na(fii_data$TIPO), ]
fii_data <- na.omit(fii_data)

shopping_rows <- filter(fii_data, TIPO == "SHOPPING")
fii_data <- bind_rows(fii_data, shopping_rows)

shopping_rows <- filter(fii_data, TIPO == "MISTO")
fii_data <- bind_rows(fii_data, shopping_rows)

# Seleciona as colunas relevantes para o modelo de classificação
model_data <- fii_data %>%
  select(TIPO, P.VP, `DY..12M..ACUMULADO`, QUANT..ATIVOS, NUM..COTISTAS, PREÇO.ATUAL..R.., PATRIMÔNIO.LÍQUIDO, VARIAÇÃO.PREÇO, DY.PATRIMONIAL, VARIAÇÃO.PATRIMONIAL, VOLATILIDADE)

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

## PREDICTIONS

predictions_unknown <- predict(model, newdata = unknown_assets)
print(predictions_unknown)

# Check the data after handling missing values
#str(fii_data)  # Checa a estrutura do dataset
#summary(fii_data)  # Estatísticas resumidas do dataset
#sapply(fii_data, function(x) sum(is.na(x)))  # Checa valores faltantes
