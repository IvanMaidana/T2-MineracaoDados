# Colunas:
# [01] Fundos
# [02] Tipo
# [03] Preço Atual
# [04] Liquidez diária
# [05] P/VP = Valor de mercado na bolsa de valores / Valor patrimonial
# [06] Dividend Yield (rendimento de dividendos)
# [07] Dividend Yield 12M Acumulado
# [08] Dividend Yield 3M Media
# [09] Dividend Yield 12M Media
# [10] Variação do preço
# [11] Patrimônio líquido
# [12] VPA = Valor patrimonial por ação
# [13] P/VPA = Preço de uma ação / Valor patrimonial por ação
# [14] Dividend Yield Patrimonial
# [15] Variação Patrimonial
# [16] Quantidade de Ativos
# [17] Volatilidade
# [18] Numero de cotistas
# [19] Taxa de gestão
# [20] Taxa de performance
# [21] Taxa de administração
#
#
# Objetivos
# 1. Descobrir padrões e anomalias nos dados (pelo menos 2 tipos)
# 2. Obter estatísticas básicas
# 3. Demonstrar características visualmente (plot)
# 4. Criar um modelo de classificação para encontrar a classe dos 5 ativos desconhecidos (presentes no fim do dataset)
#   4.1. Campos importantes: "TIPO", "P.VP", "DY..12M..MEDIA", "QUANT.ATIVOS", "NUM.COTISTAS"
# 5. Criar um relatório descrevendo o processo
#
#
# Avaliação
# 1. A qualidade do volume de descrição e análise do processo (2 pontos)
# 2. O acerto das classes na atividade mínima (2.5 pontos) 
# 3. A apresentação (2.5 pontos) 
# 4. O script (1 ponto) 
# 5. Uso de técnicas, análises e outras descobertas (2)
#
#
# Entrega
# 1. Um arquivo PDF (ou HTML) com toda a descrição do trabalho e as previsões (mínimo 4 páginas).
# 2. O script usado na análise e mineração dos dados. 
# O trabalho deve ser entregue por e-mail até dia 21/11, às 11:59 
# (assunto: “[DM] alunoA, alunoB, alunoC”.). 

# Instale os pacotes necessários
# install.packages("dplyr")
# install.packages("ggplot2")

# Carregue os pacotes necessários
library(dplyr)
library(ggplot2)

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

# Crie um histograma com base nos dados da coluna "P.VP"
ggplot(data1, aes(x = P.VP)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de P.VP", x = "P.VP", y = "Contagem")

# Separa os dados entre um dataset com dados já classificados e um sem classificação
data2 <- data1[,c("TIPO", "P.VP", "DY..12M..MÉDIA", "QUANT..ATIVOS", "NUM..COTISTAS")]
data2 <- na.omit(data2)
data3 <- tail(data2, 5)
data2 <- head(data2, nrow(data2)-5)
data2 <- data2 %>% mutate_if(is.character, as.factor)

# Divide em um conjunto de testes e outro de treino
set.seed(245)
data_rows <- floor(0.80 * nrow(data2))
train_indices <- sample(c(1:nrow(data2)), data_rows)
train_data <- data2[train_indices,]
test_data <- data2[-train_indices,]
