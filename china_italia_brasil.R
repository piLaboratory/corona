source("covid_est_exponencial.R") ## pra garantir dados o mais atualizados o possivel
library(zoo) ## Manipulacao de time series, agora nao muito importante, mas pode ser depois

## Italia: todos os dados
italia.n <- as.matrix(data[data$Country.Region=="Italy",-(1:4)])
dim(italia.n) <- NULL
## Junta às datas (depende do outro codigo
italia <- zoo(x = italia.n[italia.dia.zero:length(italia.n)],
              order.by= datas[italia.dia.zero:length(italia.n)])
## Um plot da serie temporal completa
plot(italia, type="p")

## Minimo de casos para o dia zero
italia.n.casos <- 15
## N de dias para incluir no ajuste
dias.final <- 7
## Ajuste a partir do dia zero
italia.dia.zero <- min(which(italia>=italia.n.casos, arr.ind=TRUE))
## Ajuste do modelo aos primeiros 7 dias a partir do dia zero
x <- 0:(dias.final-1)
y <- italia[italia.dia.zero:(italia.dia.zero+dias.final-1)]
## Ajuste do modelo Poisson
## Inicio do periodo
italia.inicio.fit <- glm(y~x, family=poisson)
## Ultimos 7 dias
y <- italia[(length(italia)-dias.final+1):length(italia)]
italia.fim.fit <- glm(y~x, family=poisson)
## Coeficientes
coef(italia.inicio.fit)[2]
coef(italia.fim.fit)[2]
## tempos de duplicacao
## Inicio
log(2)/coef(italia.inicio.fit)[2]
## Atual
log(2)/coef(italia.fim.fit)[2]
## Intervalos de confianca
## Inicio
log(2)/confint(italia.inicio.fit)[2,]
## Fim
log(2)/confint(italia.fim.fit)[2,]
