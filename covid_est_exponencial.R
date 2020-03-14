## Funcoes
source("functions.R")
library(ggplot2)
library(dplyr)
library(magrittr)
### Leitura dos dados do repositório John Hopkins
data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", as.is=TRUE)
## Dados em matriz com paises/regios nas linhas e dias nas colunas
## Cria vetor com as datas de cada dia em formato Date 
datas <- as.Date(gsub("X","",colnames(data)[-c(1:4)]), "%m.%d.%y")
## Seleciona regioes/paise com pelo menos 90 casos
n.corte.casos <- 90
max.cases <- apply(data[,-c(1:4)], 1, max)
data.2 <- data[max.cases> n.corte.casos, ]

## Reformata dados para que o dia zero seja o primeiro que teve um certo n de casos
## N de casos no dia zero
n.casos <- 10
## Indice das colunas da data do dia zero
indices <- apply(data.2[,-c(1:4)], 1, function(x) min(which(x>=n.casos, arr.ind=TRUE)))
## Numero de dias a contar a partir do dia em que N casos =10
## Aqui usei o numero máximo de dias a partir do zero que temos no Brasil
## podemos rever, ou melhor ainda fazer uma running window
(n.dias <- ncol(data.2)-(indices[data.2$Country.Region=="Brazil"]+4))
## Cria o teste logico para selecionar paise e regioes que têm pelo menos
## o n de dias solicitado a partir do dia zero
criterio <- (ncol(data.2)-3-indices)>=n.dias&(indices+4+n.dias)<=ncol(data.2)
## Aplica o criterio
data.3 <- data.2[criterio,]
indices.3 <- indices[criterio]
## Aqui começa um loop que passa por cada linha da matriz, e seleciona o mesmo n de dias a partir do zero
dia.zero <- matrix( ncol=n.dias, nrow= nrow(data.3))
for(i in 1:nrow(data.3))
        dia.zero[i,] <- as.matrix(data.3[i, (indices.3[i]+4):(indices.3[i]+3+n.dias)])

## Coeficients da regressão Poisson N de casos x tempo, para cada regiao/pais
## Como o modelo Poisso tem link function log o coeficiente já é a taxa exponencial de crescimento
## Seleciona as colunas que identificam cada linha
est.cfs <- data.3[,1:4]
## Esta linha aplica a funcao que faz o ajuste a cada linha da tabela com as series apartir do dia zero
## E adiciona os coeficientes correspondentes à tabela de coeficientes criada acima
## Está um tanto críptica pq concatena comandos, pensar em código mais claro.
est.cfs <- cbind(est.cfs, t(sapply(1:nrow(dia.zero), fitP, dados=dia.zero)))
##  Adiciona identificador único que combina nome da reião e do país
est.cfs %<>%
    mutate(nome = paste(Province.State,Country.Region, sep=" "),
           nome.ord = factor(nome, levels=nome[order(coef)]))

## Extrai coeficientes estimados do Brasil em um vetor separado (conveniencia)
brazil.cfs <- est.cfs[est.cfs$Country.Region=="Brazil",]

## N de casos x previstos com ICs no Brasil
brasil.forecast <- data.frame(y=dia.zero[data.3$Country.Region=="Brazil", ], x=1:ncol(dia.zero))
brasil.forecast.glm <-glm(y ~x , data=brasil.forecast, family=poisson)
brasil.forecast.forecast <- data.frame(x=1:(n.dias+10))
tmp <- predict(brasil.forecast.glm, newdata=brasil.forecast.forecast, se.fit=TRUE)
brasil.forecast.forecast$ln.pred <- tmp$fit
brasil.forecast.forecast$lse <- tmp$se.fit
brasil.forecast.forecast %<>%
    mutate(n.pred = exp(ln.pred), n.pred.low = exp(ln.pred-2*lse), n.pred.upp =exp(ln.pred+2*lse))

## Trajetorias que tiveram coeficientes similares ao Brasil
## cria o data.frame para receber os dados
## n de dias a mais do que o que temos hoje para o brazil
dias.mais <- 10
n.dias.2 <- n.dias + dias.mais
## Seleciona os paises com dias igual a n de dias acima do limite acima 
criterio.2 <- (ncol(data.2)-3-indices)>=n.dias.2&(indices+4+n.dias.2)<=ncol(data.2)
data.4 <- data.2[criterio.2,]
indices.4 <- indices[criterio.2]
## Prepara a matriz para receber os dados
dia.zero.forecast <- matrix( ncol=n.dias.2, nrow= nrow(data.4))
## Preenche a matriz com as linhas dos regioes e paises que cumprem o criterio acima
for(i in 1:nrow(data.4))
    dia.zero.forecast[i,] <- as.matrix(data.4[i, (indices.4[i]+4):(indices.4[i]+3+n.dias.2)])
## Seleciona apenas os paises que tiveram coeficientes estimados dentro do IC da estimativa do Brasil
sim.br <- est.cfs$nome[est.cfs$coef.low<=brazil.cfs$coef.up&est.cfs$coef.upp>=brazil.cfs$coef.low]
sel.sim.br <- with(data.4, paste(paste(Province.State,Country.Region, sep=" "))) %in% sim.br
dia.zero.forecast.b <- dia.zero.forecast[sel.sim.br,]

## Exporta csv com coeficientes estimados e seus erros-padrão
write.csv(est.cfs[, -c(11,12)], file="coeficientes_7_dias.csv")
