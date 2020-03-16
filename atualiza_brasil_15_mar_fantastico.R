source("china_italia_brasil.R")
## data de hoje
hoje <- as.Date("2020-03-15", "%Y-%m-%d")
## Escreva aqui n de casos
casos.hoje <- 200    
tmp <-  c(brasil,zoo(casos.hoje, hoje))
## Estimativa do tempo de duplicação
log(2)/fitP.zoo(tmp[11:length(tmp)])[c(2,5,6)]
##Anterior
log(2)/fitP.zoo(brasil[11:length(brasil)])[c(2,5,6)]

## Italia
## Escreva aqui n de casos
casos.hoje <-  24747   
tmp <-  c(italia.oms,zoo(casos.hoje, hoje))
##
index <- min(which(italia.oms>75, arr.ind=TRUE))
## Estimativa do tempo de duplicação

log(2)/fitP.zoo(tmp[index:length(tmp)])[c(2,5,6)]

log(2)/fitP.zoo(italia.oms[index:length(italia.oms)])[c(2,5,6)]
log(2)/fitP.zoo(italia.oms[index:(index+6)])[c(2,5,6)]

log(2)/fitP.zoo(italia[(length(italia)-):length(italia)])[c(2,5,6)]

log(2)/fitP.zoo(tmp[(length(tmp)-6):length(tmp)])[c(2,5,6)]


plot(italia.oms[index:length(italia.oms)], type="p", log="y")
             
