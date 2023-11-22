**Universidade Federal de Santa Maria
ELC1080 - Mineração de Dados - 2023/2**
**Professor:** Joaquim V. C. Assunção
**Alunos:** Ivan Maidana, Matheus de Almeida, Miguel
# Introdução

Neste trabalho, iremos utilizar um dataset de Fundos de Investimentos Imobiliários (FII) brasileiro. Esse dataset, possui as seguintes colunas:

1. **Fundos:** Código do fundo (`PRTS11`, `SRVD11`, `IBFF11`)
2. **Tipo:** Tipo do fundo (5 classes presentes: `DESENVOLVIMENTO`, `MISTO`, `PAPEL`, `SHOPPING` `TIJOLO` )
3. **Preço atual:** Representa a cotação atual das cotas do fundo em Reais (R$). Reflete o valor de mercado do fundo no momento.
4. **Liquidez diária:** Indica a liquidez diária do fundo, representando o volume médio diário de negociação das cotas do fundo em Reais (R$).
5. **P/VP = Preço de mercado na bolsa de valores / Valor patrimonial:** Indica o quanto os investidores estão dispostos a pagar pelo patrimônio líquido da empresa. Este índice é usado para avaliar se um fundo está subvalorizado ou sobrevalorizado.
6. **Dividend Yield (rendimento de dividendos):** Representa o rendimento de dividendos, que é o rendimento anual de dividendos como uma percentagem do preço atual de mercado das cotas do fundo. Fornece uma indicação dos rendimentos gerados pelo fundo.
7. **Dividend Yield 12M Acumulado:** Indica o rendimento de dividendos acumulado nos últimos 12 meses.
8. **Dividend Yield 3M Media:** Representa o rendimento médio de dividendos nos últimos 3 meses.
9. **Dividend Yield 12M Media:** Representa o rendimento médio de dividendos dos últimos 12 meses.
10. **Variação do preço:** Reflete a variação do preço das cotas do fundo, indicando o quanto o preço variou em um determinado período.
11. **Patrimônio Líquido:** Representa o valor patrimonial líquido (NAV) ou patrimônio líquido do fundo. É o valor total dos ativos do fundo menos os seus passivos.
12. **VPA = Valor patrimônial / Ação:** Significa Valor Patrimonial por Ação, que se traduz em Valor Patrimonial Líquido (NAV) por ação. É calculado dividindo o valor patrimonial líquido pelo número total de ações.
13. **P/VPA = Preço de uma ação / Valor patrimonial por ação:** Representa o índice Preço/NAV, calculado dividindo o preço atual de mercado por ação pelo valor patrimonial líquido por ação.
14. **Dividend Yield patrimonial:** Indica o rendimento de dividendos com base no valor patrimonial líquido do fundo.
15. **Variação patrimonial:** Reflete a variação do valor patrimonial líquido do fundo, indicando o quanto o valor patrimonial líquido mudou em um período específico.
16. **Quantidade de ativos:** Representa a quantidade de ativos detidos pelo fundo.
17. **Volatilidade:** Indica a volatilidade dos retornos do fundo, refletindo o quanto os retornos do fundo flutuam ao longo do tempo.
18. **Número de cotistas:** Representa o número de cotistas ou cotistas do fundo.
19. **Taxa de gestão:** Indica a taxa de administração cobrada pelo fundo.
20. **Taxa de performance:** Representa a taxa de performance cobrada pelo fundo com base no seu desempenho em relação a um benchmark.
21. **Taxa de administração:** Indica a taxa de administração cobrada pelo fundo para despesas administrativas.

A partir destes dados, descobriremos padrões e anomalias usando técnicas mostradas na disciplina. Serão também obtidas estatísticas básicas e demonstradas características visualmente através de plots. O intuito final é a construção de um modelo de classificação para encontrar a classe dos 5 ativos desconhecidos presentes ao fim do dataset.

Este relatório descreve todo o processo utilizado.

# Exploração dos dados

Primeiramente, carregamos os dados a serem utilizados:

```R 
raw_data <- read.csv("http://www-usr.inf.ufsm.br/~joaquim/UFSM/DM/TF_2023%20-%20FII.csv")
```

O dataset bruto recém carregado possui a seguinte aparência:

![[Image1.png]]


### Conversão dos dados

Dentro do dataset carregado, os valores *"N/A"* ainda não estão devidamente formatados, eles precisam ser convertidos para um valor `NA` propriamente dito. Utilizaremos a partir de agora `fii_data` como nosso dataframe.

```R
fii_data <- raw_data

# Converte todos "N/A" em NA
fii_data <- mutate_all(fii_data, function(x) ifelse(x == "N/A", NA, x))
```

Os dados numéricos apresentados pelo dataset estão em formato de string. Antes de serem visualizados, eles precisam passar por uma transformação para que sejam representados realmente como dados em formato numérico.

```R
# Função que converte valores em string para numérico
format_to_numeric <- function(x){
  as.numeric(gsub(",", "", gsub("\\.", "", gsub("\\s", "", gsub("%", "", x)))))/100

# Converte todos os campos a partir da 3ª coluna para numérico
fii_data[, 3:ncol(fii_data)] <- apply(fii_data[, 3:ncol(fii_data)], MARGIN = c(1,2), format_to_numeric)
}

# Conserta a conversão dos campos inteiros
fii_data$QUANT..ATIVOS <- as.integer(fii_data$QUANT..ATIVOS * 100)
fii_data$NUM..COTISTAS <- as.integer(fii_data$NUM..COTISTAS * 100)
```

Nos campos presentes dentro do dataset, os números, que antes da conversão representados por strings, apresentavam caracteres adicionais, como vírgulas, pontos, sinais de porcentagem e espaços em branco. O algoritmo funciona através de uma função que elimina todos esses caracteres "a mais" e divide o número resultante por 100, pois a maioria dos dados são reais. Para os dados inteiros, no entanto, uma correção é realizada posteriormente, multiplicando o valor por 100 e convertendo-o para inteiro. OBS: As primeiras colunas não são tocadas, pois possuem apenas dados categóricos.

Há a necessidade também de se trabalhar nos dados da coluna `TIPO`, já que esta coluna, além de dados `NA`, também possui dados rotulados como "?" ao final do dataset. Estes dados precisam ser tratados para também constarem como `NA` a fim de não atrapalhar a transformação para um tipo `factor`, que deverá acontecer posteriormente. As 3 últimas colunas também precisam ser removidas, pois possuem apenas valores `NA` e, portanto, não serão úteis para o nosso trabalho.

```R
# Substitui "?" por NA
fii_data$TIPO[fii_data$TIPO == "?"] <- NA
# Transforma tipo em um factor
fii_data <- fii_data %>% mutate_if(is.character, as.factor)
# Remove as 3 últimas colunas, pois possuem apenas valores NA
fii_data = fii_data[, -c(19, 20, 21)]
```

Por fim, nosso dataset `fii_data` fica representado da seguinte forma:

![[Image2.png]]

### Estatísticas

Podemos checar a estrutura do dataset utilizando `str(fii_data)`. Desta forma, encontramos 2 colunas do tipo factor, 2 do tipo inteiro e o restante do tipo numérico.

![[Pasted image 20231121183553.png]]

Também podemos visualizar estatísticas resumidas do dataset através de `summary(fii_data)`:

![[Pasted image 20231121183757.png]]

Também é possível identificar a quantidade de valores faltantes para cada coluna através de `sapply(fii_data, function(x) sum(is.na(x)))`:

![[Pasted image 20231121184027.png]]

Como podemos observar, algumas colunas possuem valores `NA` excessivos. Estas, não serão utilizadas em conjunto com o nosso classificador. Utilizaremos campos considerados importantes como `TIPO`, `P.VP`, `DY..12M..MÉDIA`, `QUANT..ATIVOS` e `NUM..COTISTAS`, além de campos que possuam um número reduzido de `NA`. Pelo menos que contenham menos valores `NA` do que o campo `P.VP`. 
### Visualizações

Podemos visualizar a frequência de cada tipo dentro do dataset, utilizando o script abaixo:

```R
# Plota a frequência de cada tipo
fii_data %>% 
	ggplot(aes(x = TIPO)) + 
	geom_bar(fill = "blue", color = "black", alpha = 0.7) + 
	labs(title = "Frequência de cada TIPO", x = "TIPO", y = "Frequency")
```

![[Pasted image 20231121191751.png]]

Isso demonstra uma quantidade muito superior de observações do `TIPO` `TIJOLO` dentro do dataset. Em segundo, temos observações do `TIPO` `PAPEL`. Isso demonstra que temos um conjunto desbalanceado, ou seja, não possuímos o mesmo número de observações para cada `TIPO`. Nestes casos, podemos recorrer a um resampling: aumento das instâncias de um `TIPO` minoritário, ou a diminuição das instâncias majoritárias. Também podemos utilizar classificadores que lidem bem com datasets desbalanceados, tais como *Random Forest*, que cria múltiplas *Decision Trees* e combina as suas predições, além do *Gradient Boosting*. O classificador escolhido para este trabalho, foi o *Random Forest* - também optamos por um oversampling de classes que estavam sendo difíceis de atingir uma precisão satisfatória durante a construção do classificador.

Podemos também encontrar histogramas para quaisquer colunas, utilizando o script abaixo:

```R
# Especifique a coluna para o histograma
column_for_histogram <- "P.VP"

# Plota um histograma para cada valor único na coluna "TIPO"
fii_data %>%
  ggplot(aes(x = !!sym(column_for_histogram))) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~TIPO, scales = "free") +
  labs(title = paste("Histogramas para", column_for_histogram),
       x = column_for_histogram,
       y = "Frequency")
```

![[Pasted image 20231121190610.png]]

Isso demonstra um padrão visível na distribuição dos dados da coluna `P.VP` que poderá ser eventualmente explorado pelo nosso classificador.

Podemos encontrar uma matriz scatterplot para as variáveis que considerarmos importantes utilizando `pairs(~P.VP + DY..12M..MÉDIA + QUANT..ATIVOS + NUM..COTISTAS + VARIAÇÃO.PATRIMONIAL, data = fii_data, col=fii_data$TIPO, pch=16)`.

![[Pasted image 20231121185711.png]]

Ao plotarmos os gráficos com os dados praticamente brutos, onde apenas foram realizadas as conversões de tipagem de dados dentro do dataset, podemos observar que os padrões estão dificilmente reconhecíveis e há a presença de outliers, valores muito distantes dos demais que possivelmente trariam problemas para classificação. Os motivos para a falta de um padrão observável nestes dados podem ser resumido à uma grande quantidade de dados faltantes (`NA`) e à uma grande quantidade de outliers: precisamos tratar esses problemas durante a fase de pré-processamento.
# Pré-processamento dos dados

Precisamos realizar o pré-processamento para facilitar o encontro de padrões dentro dos nossos dados. Para isso, teremos duas abordagens: primeiramente, lidaremos com dados faltantes e, posteriormente, lidaremos com dados outliers.
### Lidando com os dados faltantes

Podemos lidar com os dados faltantes substituindo-os pela média do `TIPO` para o valor. Utilizamos o seguinte script para realizar isso:

```R
# Exclui as 2 primeiras colunas da checagem
colunas <- colnames(fii_data)[3:ncol(fii_data)]

# Substitui os valores faltantes de um tipo com a média do tipo para o valor.
fii_data_imputed <- fii_data %>%
  group_by(TIPO) %>%
  mutate(across(all_of(colunas), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()
```

Nesse script, a função `group_by` é utilizada para agrupar o dataset pela coluna `TIPO`. Isso significa que cada operação subsequente será realizada separadamente a cada grupo definido pelos valores únicos de `TIPO`. Já a função `mutate` aplica uma transformação em todas colunas especificadas pelo vetor `colunas`. Para cada coluna, checa se o valor é `NA`, caso seja, substitui o valor com a média da coluna.
### Lidando com outliers

Podemos lidar com os dados outliers de maneira similar, substituindo-os pela mediana do `TIPO` para o valor. O seguinte script foi utilizado:

```R
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
```

Neste script, a função `treat_outliers_within_group` calcula um limite inferior e um limite superior para cada coluna baseado no intervalo interquartil (`IQR`) e substituí os valores fora deste intervalo pela mediana da coluna em cada grupo. Posteriormente, o script usa um loop para iterar cada uma das colunas especificadas em `colunas`. O resultado é armazenado em `fii_data` após o final do loop.

### Toques finais

Para a finalização do pré-processamento, vamos remover do dataset as linhas cujo `TIPO` ainda é desconhecido (`NA`). Estes serão armazenados em um novo dataframe. Além disso, duplicaremos instâncias do tipo `SHOPPING` e `MISTO`, pois a taxa de precisão em testes anteriores para as tais estava muito baixa.

```R
# Identifica as observações cujos tipos ainda são desconhecidos
unknown_assets <- fii_data[is.na(fii_data$TIPO), ]
fii_data <- na.omit(fii_data)

# Duplica instâncias do tipo SHOPPING
shopping_rows <- filter(fii_data, TIPO == "SHOPPING")
fii_data <- bind_rows(fii_data, shopping_rows)

# Duplica instâncias do tipo MISTO
shopping_rows <- filter(fii_data, TIPO == "MISTO")
fii_data <- bind_rows(fii_data, shopping_rows)
```
### Visualizando os dados pós-processados

Podemos visualizar o scatterplot dos dados da mesma maneira que fizemos anteriormente:

![[Pasted image 20231121201742.png]]

Agora, podemos observar que os dados aparentam pertencer a padrões reconhecíveis por uma máquina, além de estarem muito menos esparsos. Esses dados serão separados em conjuntos de treino e de testes e jogados para um modelo de classificação.
# Modelo de classificação

Para o nosso modelo de classificação, será utilizado um modelo de *Random Forest*, as colunas que serão utilizadas na construção desse modelo serão `TIPO`, que servirá como label, e atributos como : `P.VP`, `DY..12M..MEDIA`, `QUANT..ATIVOS`, `NUM..COTISTAS`, `PREÇO.ATUAL..R..`, `PATRIMÔNIO.LÍQUIDO`, `VARIAÇÃO.PŔEÇO`, `DY.PATRIMONIAL`, `VARIAÇÃO.PATRIMONIAL` e `VOLATILIDADE`.

### Conjuntos de treino e de testes

Nosso dataset será dividido em um conjunto de treino e um conjunto de testes: 80% das instâncias irão para o conjunto de treino, enquanto os restantes 20% irão para o conjunto de testes. O treinamento utilizará um controle de *cross-validation* de 8 folds: dessa maneira, o modelo será treinado 8 vezes, cada vez utilizando 8-1 folds para treinamento e o fold restante para validação. *Cross-validation* reduz a variança, maximiza o uso da informação disponível e avalia quão bem um modelo generaliza para dados novos e não vistos.

```R
# Seleciona as colunas relevantes para o modelo de classificação
model_data <- fii_data %>%
  select(TIPO, P.VP, DY..12M..MÉDIA,  QUANT..ATIVOS, NUM..COTISTAS, PREÇO.ATUAL..R.., PATRIMÔNIO.LÍQUIDO, VARIAÇÃO.PREÇO, DY.PATRIMONIAL, VARIAÇÃO.PATRIMONIAL, VOLATILIDADE)

# Divide os dados em conjuntos de treino e de teste
set.seed(100)
train_index <- createDataPartition(model_data$TIPO, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Define o controle da validação cruzada
cv_control <- trainControl(method = "cv", number = 8)  # 8-fold cross validation
```
### Random Forest

Foi utilizado um modelo *Random Forest*, que cria múltiplas *Decision Trees* e combina as suas predições.

```R
# Treina o modelo de Random Forest utilizando validação cruzada
model <- train(
  TIPO ~ .,
  data = train_data,
  method = "rf",  # Random Forest
  trControl = cv_control
)
```

Posteriormente, são realizadas as predições, a avaliação do modelo através da matriz de confusão e o armazenamento da importância de cada variável dentro do modelo:

```R
# Realizar predições no dataset de treino
predictions <- predict(model, newdata = test_data)

# Avaliar o modelo, printar
confusion_matrix <- confusionMatrix(predictions, test_data$TIPO)

# Plotar a importância de cada variável no modelo
var_importance <- varImp(model)
```
# Resultados

Resultado da matriz de confusão do modelo sobre o conjunto de testes:

![[Pasted image 20231121205249.png]]

O modelo alcançou uma acurácia de 93% para o conjunto de testes, dados estes que não foram vistos pelo modelo em nenhum momento. Isso demonstra uma alta capacidade de generalização do modelo.

O plot da importância de cada variável para a classificação dentro do modelo também foi realizado, sendo os valores respectivos apresentados abaixo:

![[Pasted image 20231121205446.png]]

![[Pasted image 20231121205551.png]]

Além disso, foi realizada a predição para cada uma das instâncias em que `TIPO` estava com valores `NA`:

![[Pasted image 20231121205728.png]]

As últimas 5 predições correspondem às predições requisitadas no enunciado do trabalho. O método de classificação proposto encontrou: `TIJOLO`, `PAPEL`, `PAPEL`, `TIJOLO` e `PAPEL`.
