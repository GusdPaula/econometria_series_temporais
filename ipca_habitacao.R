##################################################################
# Um Modelo SARIMA para a inflação da habitação medida pelo IPCA #
##################################################################


library("readxl")
library(tseries)
library(forecast)
library(lmtest)

ipca_hab <- read_excel("C:/Users/gusta/Downloads/STP-20230501150142377.xlsx")

head(ipca_hab)
tail(ipca_hab)

attach(ipca_hab)

## Transformar a série em um "objeto" de séries temporais (opcional)

IPCA <- ts(IPCA_HABITACAO, start=c(1991, 1), end=c(2023, 3), frequency=12)
IPCA

## A série é estacionária?

par(mfrow=c(1,1))
plot.ts(IPCA, main="IPCA HABITAÇÃO",
        ylab="Preço",
        xlab="Tempo")

adf.test(IPCA)

# H0: A série possui raiz unitária (não estacionária)
# H1: A série não possui raiz unitária (estacionária)

# Resultado: p-value = 0.375
# Como p-valor < 0,05, rejeita-se H0, ao nível d e0,05.
# OBS: Série estacionária

#### Gráficos de ACF e PACF

par(mfrow=c(1,2))
acf(IPCA, lag=48, main="ACF ipca habitacao", ylab="")
pacf(IPCA, lag=48, main="PACF ipca habitacao", ylab="")

## OBS: Gráficos sugerem padrão sazonal.



#### Obter série com diferença sazonal

mod1 <- diff(IPCA, lag=12)

#### ACF e PACF com diferença sazonal

par(mfrow=c(1,2))
acf(mod1, lag=48, main="ACF ipca habitacao com diferenças", ylab="")
pacf(mod1, lag=48, main="PACF ipca habitacao com diferenças", ylab="")

## Sugestão de modelos: 

# Arima Sazonal (1,0,1)x(1,1,0)_12 ou
# Arima Sazonal (1,0,1)x(0,1,1)_12

######################### Modelo Arima Sazonal

########### Arima Sazonal (1,0,1)x(1,1,0)_12

mod_ipca <- Arima(mod1, order=c(1,0,1), seasonal=list(order=c(1,1,0), period=12))
mod_ipca

## AIC = 2260.8

coeftest(mod_ipca)

########### Arima Sazonal (1,0,1)x(0,1,1)_12

mod_ipca2 <- Arima(mod1, order=c(1,0,1), seasonal=list(order=c(0,1,1), period=12))
mod_ipca2

## AIC = 2173.21

coeftest(mod_ipca2)

# Todos os coeficientes se mostraram estatisticamente significantes.


# AIC - Arima Sazonal (1,0,1)x(1,1,0)_12: 2260.8
# AIC - Arima Sazonal (1,0,1)x(0,1,1)_12: 2173.21

## CONCLUSÃO: Como AIC do modelo Sarima MA(1) < AIC do modelo Sarima AR(1)
# temos, então, que o modelo Sarima MA(1) é mais adequado.

#### Diagnóstico

# Ho: Os resíduos são ruído branco
# H1: Os resíduos não são ruído branco.

res1 <- residuals(mod_ipca2)
Box.test(res1, type="Ljung-Box")

# Resultado: p-value = 0.8656
# Como p-valor > 0,05, não rejeitamos Ho, ao nível de 0,05.

par(mfrow=c(1,1))

acf(res1)

#### Predições

pred = predict(mod_ipca2, n.ahead=12)
pred

ts.plot(mod1, pred$pred, main="Predições de Ameaças",
        lty=c(1,1),
        lwd=2,
        col=c(4,2))


future <- forecast(mod_ipca2, h=12)
plot(future)
future

detach(ipca_hab)
