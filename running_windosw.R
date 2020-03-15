library(runner)
source("china_italia_brasil.R")

##
brasil.d0 <- diazero(brasil,15)
brasil.rw <- runner(brasil.d0, function(x) log(2)/fitP.zoo(x)["coef"], k =5, idx = time(brasil.d0))
plot(zoo(brasil.rw, time(brasil.d0)))

italia.d0 <- diazero(italia,15)[-(1:5)]
italia.rw <- runner(italia.d0, function(x) log(2)/fitP.zoo(x)["coef"], k =7, idx = time(italia.d0))
plot(zoo(italia.rw,time(italia.d0)))

china.d0 <- diazero(china,15)
china.rw <- runner(china.d0, function(x) log(2)/fitP.zoo(x)["coef"], k =5, idx = time(china.d0))
plot(zoo(china.rw,time(china.d0)), log="y")
