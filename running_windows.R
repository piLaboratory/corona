library(runner)
source("china_italia_brasil.R")

## Numero de dias da running window
k <- 14
##
brasil.d0 <- diazero(brasil,15)
brasil.rw <- runner(brasil.d0, function(x) log(2)/fitP.zoo(x)["coef"], k =k, idx = time(brasil.d0))
plot(zoo(brasil.rw, time(brasil.d0)))

italia.d0 <- diazero(italia.oms,15)
italia.rw <- runner(italia.d0, function(x) log(2)/fitP.zoo(x), k =k, idx = time(italia.d0))%>%
    t() %>%
    zoo(time(italia.d0))
plot(zoo(italia.rw["coef",],time(italia.d0)))
italia.rw %>%
    fortify() %>%
    ggplot(aes(Index,coef)) +
    geom_line() +
    geom_ribbon(aes(ymin=coef.low, ymax=coef.upp), alpha=0.25) +
    ylab("Tempo de duplicação") +
    theme_bw()


china.d0 <- diazero(china,15)
china.rw <- runner(china.d0, function(x) log(2)/fitP.zoo(x)["coef"], k =k, idx = time(china.d0))
plot(zoo(china.rw,time(china.d0)), log="y")


