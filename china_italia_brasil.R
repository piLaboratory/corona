source("covid_est_exponencial.R") ## pra garantir dados o mais atualizados o possivel
library(zoo) ## Manipulacao de time series, agora nao muito importante, mas pode ser depois

################################################################################
## Italia
################################################################################
italia.n <- as.matrix(data[data$Country.Region=="Italy",-(1:4)])
dim(italia.n) <- NULL
## Junta às datas
italia <- zoo(x = italia.n, order.by= datas)
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
## tempos de duplicacao ##
## Inicio
log(2)/coef(italia.inicio.fit)[2]
## Atual
log(2)/coef(italia.fim.fit)[2]
## Intervalos de confianca
## Inicio
log(2)/confint(italia.inicio.fit)[2,]
## Fim
log(2)/confint(italia.fim.fit)[2,]


## Italia oms
italia.raw <- read.csv("https://covid.ourworldindata.org/data/full_data.csv", as.is=TRUE) %>%
    filter(location=="Italy") %>%
    select(date,total_cases)
italia.oms <- zoo(x = italia.raw$total_cases,
             order.by = as.Date(italia.raw$date, "%Y-%m-%d"))

## Verificando imapcto dos ajustes lineares
## N de casos dia zero
it.oms.n <- 75

italia.oms.76 <- fitP.zoo(italia.oms[min(which(italia.oms>it.oms.n, arr.ind=TRUE)):length(italia.oms)], only.coef=FALSE)
italia.oms.fim <- fitP.zoo(italia.oms[(length(italia.oms)-7): length(italia.oms)], only.coef=FALSE)
## Previstos pelos dois modelos
newdata <- data.frame(ndias=rev(max(time(italia.oms))-time(italia.oms[which(italia.oms>it.oms.n, arr.ind=TRUE)])))
it.fit.76 <- predict(italia.oms.76, newdata=newdata, type="response")
## plot
plot(italia.oms[min(which(italia.oms>it.oms.n, arr.ind=TRUE)):length(italia.oms)], log="y", type="p")
lines(zoo(it.fit.76, time(italia.oms[min(which(italia.oms>it.oms.n, arr.ind=TRUE)):length(italia.oms)])),col="blue")


################################################################################
## Brasil
################################################################################
brasil.raw <- read.csv("brazil_wikipedia_timeseries.csv", as.is=TRUE)
## Converte para time series
brasil <- zoo(x=brasil.raw$casos.acumulados,
              order.by = as.Date(brasil.raw$dia, "%d-%m-%Y"))
plot(brasil, log="x", type="p")
## Ultimos 7 dias
y <- brasil[(length(brasil)-dias.final+1):length(brasil)]
brasil.fim.fit <- glm(y~x, family=poisson)
## Coeficientes
coef(brasil.fim.fit)[2]
## tempos de duplicacao ##
## Atual
log(2)/coef(brasil.fim.fit)[2]
## Intervalos de confianca
## Fim
log(2)/confint(brasil.fim.fit)[2,]


################################################################################
## China
################################################################################
china.raw <- read.csv("https://covid.ourworldindata.org/data/full_data.csv", as.is=TRUE) %>%
    filter(location=="China") %>%
    select(date,total_cases)

china <- zoo(x = china.raw$total_cases,
             order.by = as.Date(china.raw$date, "%Y-%m-%d"))
## Um plot da serie temporal completa
plot(china, type="p")

## Minimo de casos para o dia zero
china.n.casos <- 15
## N de dias para incluir no ajuste
dias.final <- 7
## Ajuste a partir do dia zero
china.dia.zero <- min(which(china>=china.n.casos, arr.ind=TRUE))
## Ajuste do modelo aos primeiros 7 dias a partir do dia zero
x <- 0:(dias.final-1)
y <- china[china.dia.zero:(china.dia.zero+dias.final-1)]
## Ajuste do modelo Poisson
## Inicio do periodo
china.inicio.fit <- glm(y~x, family=poisson)
## Ultimos 7 dias
y <- china[(length(china)-dias.final+1):length(china)]
china.fim.fit <- glm(y~x, family=poisson)
## Coeficientes
coef(china.inicio.fit)[2]
coef(china.fim.fit)[2]
## tempos de duplicacao ##
## Inicio
log(2)/coef(china.inicio.fit)[2]
## Atual
log(2)/coef(china.fim.fit)[2]
## Intervalos de confianca
## Inicio
log(2)/confint(china.inicio.fit)[2,]
## Fim
log(2)/confint(china.fim.fit)[2,]
