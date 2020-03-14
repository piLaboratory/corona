source("covid_est_exponecial.R")

## Series temporal de n de casos
pdf(file="series_temporais_brutas.pdf")
matplot(y=t(data.2[-c(1:4)]), type="l", log="y",
        main = paste("Países/regiões com >", n.corte.casos,"casos em", max(datas)))
dev.off()


## Plot das series temporais selecionadas até aqui, todas começando no dia zero)
pdf(file="series_temporais_a_partir_dia_zero.pdf")
matplot(x=0:(n.dias-1),t(dia.zero), type="l", log="y",
        ylab="N de casos",
        xlab=paste("Dias a partir do primeiro com ", n.casos," casos", sep=""))
dev.off()


## Coeficientes estimados da exponencial com +/1 2 x erro padrão, para cada País/Região
p1 <- est.cfs %>%
    ggplot(aes(nome.ord, coef)) +
    geom_point() +
    geom_linerange(aes(ymin=coef.low, ymax=coef.upp)) +
    ylab("Exponential coefficient") +
    xlab("") +
    geom_hline(yintercept=brazil.cfs$coef.low, col="blue") +
    geom_hline(yintercept=brazil.cfs$coef.upp, col="blue") +
    coord_flip()
pdf(file="coeficientes.pdf", width=12, height=9)
print(p1)
dev.off()

## Crescimento previsto nos próximos dias no Brasil
## usando o modelo ajustado
p2 <- brazil.10.forecast %>%
    ggplot(aes(x, n.pred)) +
    geom_line() +
    geom_ribbon(aes(ymin=n.pred.low, ymax=n.pred.upp), alpha=0.2)+
    geom_point(data=brazil.10, aes(x,y))
pdf(file="brazil_pred_mais_10_dias_IC%1d.pdf", onefile=FALSE)
print(p2)
print(p2+scale_y_log10())
dev.off()

## Trajetoria do N de casos no Brasil e em paises cujo coeficiente estimado
## caiu no intervalode  confiança da estimativa para o Brasil
pdf(file="trajetorias_coef_similares_ao_brazil%1d.pdf", width=9, height=7.5, onefile=FALSE)
matplot(t(dia.zero.forecast.b), type="l", lty=1, col="grey", ylab="Nr cases", lwd=0.75)
lines(n.pred ~ x, data=brasil.forecast.forecast, lwd=2.5, lty=2, col="darkblue")
lines(y ~ x, data=brasil.forecast, lwd=2.5, col="darkblue")
matplot(t(dia.zero.forecast.b), type="l", lty=1, col="grey", ylab="Nr cases", lwd=0.75, log="y")
lines(n.pred ~ x, data=brasil.forecast.forecast, lwd=2.5, lty=2, col="darkblue")
lines(y ~ x, data=brasil.forecast, lwd=2.5, col="darkblue")
dev.off()

### Coeficientes x latitude: nao muito elucidadtivo, fica a indicacao para
## eventual exploracao contra outras variaveis.
p3 <- est.cfs %>%
    ggplot(aes(Lat, coef)) +
    geom_point() +
    geom_linerange(aes(ymin=coef.low, ymax=coef.upp)) +
    ylab("Exponential coefficient") +
    xlab("Latitude") 
pdf(file="coefs_latitudes.pdf")
print(p3)
dev.off()
