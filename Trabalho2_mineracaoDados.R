data <- read.csv("http://www-usr.inf.ufsm.br/~joaquim/UFSM/DM/TF_2023%20-%20FII.csv")
data

# Instale o pacote dplyr se ainda não estiver instalado
# install.packages("dplyr")

# Instale o pacote ggplot2 se ainda não estiver instalado
# install.packages("ggplot2")

# Carregue o pacote dplyr
library(dplyr)

# Carregue o pacote ggplot2
library(ggplot2)

# Substitua todos os valores "N/A" por NA no data frame
data1 <- mutate_all(data, function(x) ifelse(x == "N/A", NA, x))

#verifica a porcentagem de NA por cada coluna
data2 <- round(colSums(is.na(data1))*100/nrow(data1), 2)
data2

#exclui as colunas que tem a maioria dos valores como NA
data1 = data1[, -c(19, 20, 21)]


# Converter P.VP para numérico
data1$P.VP <- as.numeric(data1$P.VP)

# Crie um histograma com base nos dados da coluna "P.VP"
ggplot(data1, aes(x = P.VP)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de P.VP", x = "P.VP", y = "Contagem")


