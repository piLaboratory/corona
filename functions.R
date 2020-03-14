## Fits a glm and returns the coeficients
fitP <- function(i, dados, family=poisson){
    x <- 0:(length(dados[i,])-1)
    y <- dados[i,]
    fit <- glm(y~x, family=family)
    ci <- confint(fit)
    results <- c(coef(fit),ci[1,], ci[2,])
    names(results) <- c("intercept", "coef", "int.low", "int.upp", "coef.low", "coef.upp")
    results
}


