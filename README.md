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

![[Pasted image 20231121181710.png]]


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

![[Pasted image 20231121183210.png]]

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

Isso demonstra uma quantidade muito superior de observações do `TIPO` `TIJOLO` dentro do dataset. Em segundo, temos observações do `TIPO` `PAPEL`. Isso demonstra que temos um conjunto desbalanceado, ou seja, não possuímos o mesmo número de observações para cada `TIPO`. Nestes casos, podemos recorrer a um resampling: aumento das instâncias de um `TIPO` minoritário, ou a diminuição das instâncias majoritárias. Também podemos utilizar classificadores que lidem bem com datasets desbalanceados, tais como *Random Forest*, que cria múltiplas *Decison Trees* e combina as suas predições, além do *Gradient Boosting*. O classificador escolhido para este trabalho, foi o *Random Forest* - também optamos por um oversampling de classes que estavam sendo difíceis de atingir uma precisão satisfatória durante a construção do classificador.

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

### Lidando com os dados faltantes

### Lidando com outliers

# Modelo de classificação

### RandomForest

# Resultados

# Conclusão
