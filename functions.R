## Fits a glm and returns the coeficients fro a regular time series, which is a line of an data.frame "dados"
fitP <- function(i, dados, family=poisson){
    x <- 0:(length(dados[i,])-1)
    y <- dados[i,]
    fit <- glm(y~x, family=family)
    ci <- confint(fit)
    results <- c(coef(fit),ci[1,], ci[2,])
    names(results) <- c("intercept", "coef", "int.low", "int.upp", "coef.low", "coef.upp")
    results
}


## Fits a glm and returns the coeficientes fom a zoo object with time series
fitP.zoo <- function(zoo.obj, family=poisson, only.coef=TRUE){
    ndias <- rev(max(time(zoo.obj)) - time(zoo.obj))
    fit <- glm(zoo.obj ~ ndias, family = family)
    if(only.coef){
        ci <- confint(fit)
        results <- c(coef(fit),ci[1,], ci[2,])
        names(results) <- c("intercept", "coef", "int.low", "int.upp", "coef.low", "coef.upp")
    }
    else
        results  <-  fit
    return(results)
    }

## Corta uma seria temporal da classe zoo, tirando todos os valores anteriores ao primeiro valor igual
## a um certo limite (n.casos). Para fazer a serie a partir do dia zero
diazero <- function(zoo.obj, limite.x){
    dia.zero <- min(which(zoo.obj>=limite, arr.ind=TRUE))
    zoo.obj[dia.zero:length(zoo.obj)]
    }
