## data de hoje
hoje <- as.Date("2020-03-15", "%Y-%m-%d")
## Escreva aqui n de casos
casos.hoje <-     
tmp <-  c(brasil,zoo(casos.hoje, hoje))
## Estimativa do tempo de duplicação
log(2)/fitP.zoo(tmp[11:length(tmp)])[c(2,5,6)]

             
