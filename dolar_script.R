#################  Dados Dólar

library(readxl)

dolar <- read_excel("C:/Users/gusta/Downloads/STP-20230426195120992.xlsx")

head(dolar)
tail(dolar)

attach(dolar)

##### Gráfico da série

# Verificar a necessidade de tornar a serie estacionaria.
plot.ts(DOLAR_PRECO,main = "serie de dolar preco")

#OBS: serie visivelmente nao estacionaria
#(nao se comporta ao redor de uma media constante)

###########Log-retorno
rdolar <- diff(log(DOLAR_PRECO))
head(rdolar)
plot.ts(rdolar, main="log-retorno de dolar")

# OBS.: Série visivelmente estacionária (comportamento ao redor de uma média constante)

####### Normalidade da distribuição dos dados do log-retorno

hist(rdolar, probability = TRUE)

# Aumentar o número de classes (eixo x)

hist(rdolar, probability = TRUE, nclass=20)

# OBS: Variável com distribuição aparentemente normal.

# Histograma com ajuste de densidade normal
hist(rdolar, probability = TRUE, nclass=20, ylim=c(0,20))
curve(dnorm(x, mean(rdolar), sd(rdolar)), add=TRUE, col="red", lwd=2)

# OBS: O gráfico histograma ultrapassou a densidade normal ajustada.

# Gráfico QQ-plot do log-retorno
qqnorm(rdolar)
qqline(rdolar)


###  Teste de normalidade
library(tseries)
jarque.bera.test(rdolar)

# H0: os dados possuem distruição normal.
# H1: os dados não possuem distruição normal.

# Resultado: p-value = 0.0352

# OBS: Como o p-valor < 0.05, rejeita-se H0: a série de log-retorno possui distruição normal.

######### Junção dos gráficos
par(mfrow=c(2,2))

plot.ts(DOLAR_PRECO, main = "Dólar")
plot.ts(rdolar, main = "Log-retorno Dólar")
hist(rdolar, probability = TRUE, nclass=20, ylim=c(0,20))
curve(dnorm(x, mean(rdolar), sd(rdolar)), add=TRUE, col="red", lwd=2)
qqnorm(rdolar)
qqline(rdolar)

# Assimetria e curtose
install.packages('e1071', dependencies=TRUE)
library(e1071)
par(mfrow=c(1,1)) # Voltar ao normal (gráfico com apenas uma figura)

hist(rdolar, nclass=20, probability = TRUE, ylim=c(0,20))

skewness(rdolar)
# Resultado: 0.1030688
# OBS: Levemente assimétrica  à esquerda

# Curtose
## OBS:
# e(x) = 0 -> distrib. mesocúrtica.
# e(x) > 0 -> distrib. leptocúrtica.
# e(x) < 0 -> distrib. platicúrtica.

kurtosis(rdolar)

# Resultado: 0.7411481
# OBS.: e(x) > 0 -> distribuição leptocúrtica.

####### Modelagem ARIMA (p, d, q)

# Identificar os tipos de modelos e os valores (p, d, q)
## (FAC e FACP)

par(mfrow=c(1,2))

acf(rdolar, lag=20)
pacf(rdolar, lag=20)

## Sugestão: AR(14) ou MA(14)

library(forecast)

# Para que o comando faça a transfromação "diff log" da série é necessário incluir o argumento 'lambda=0'

# AR (14)
mod1 <- Arima(DOLAR_PRECO, order=c(14,1,0), lambda = 0)
mod1

# AIC=-1596.31

## Analisar os coeficientes

library(lmtest)

coeftest(mod1)

# OBS.: Considerar apenas os coeficientes 14
# Os coeficientes a serem removidos devem receber valor "zero".

mod2 <- Arima(DOLAR_PRECO, order=c(14,1,0), lambda=0, fixed=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA))
mod2

# AIC=-1611.49 
coeftest(mod2)
# Estimate Std. Error z value Pr(>|z|)  
# ar14 -0.150672   0.063275 -2.3812  0.01726 *
# OBS: TOdos os coeficientes estatiscamente significantes ao nível de 0,05

# MA(14)

mod3 <- Arima(DOLAR_PRECO, order=c(0,1,14), lambda=0)
mod3
# AIC=-1594.52

## Analisar os coeficientes
coeftest(mod3)

# Considerar apenas os coeficientes 6* e 14
# Os coeficientes a serem removidos devem receber o valor "zero"

mod4 <- Arima(DOLAR_PRECO, order = c(0,1,14), lambda = 0, fixed=c(0,0,0,0,0,NA,0,0,0,0,0,0,0,NA))
mod4

# AIC=-1610.78   

coeftest(mod4)

# Desconsiderar o coeficiente 6

mod5 <- Arima(DOLAR_PRECO, order = c(0,1,14), lambda = 0, fixed=c(0,0,0,0,0,0,0,0,0,0,0,0,0,NA))
mod5

# AIC=-1610.52

coeftest(mod5)

# OBS: Todos os coeficientes se mostraram estatisticamente significantes ao nível de 0,05

#OBS:
# AIC - AR (14) =-1611.49 
# AIC - MA (14) = -1610.78 

# obs: Conclusão: como a AIC do modelo AR(14) < AIC do modelo AR(14), temos
# evidências de que o modelo AR(14) ajuste-se melhor aos dados.

### Análise diagnóstica

## teste de Box_Ljung
# H0: Os resíduos são independentes (ruído branco)
# H1: Os resíduos não são independentes.

res <- residuals(mod2)
Box.test(res, lag=20, type="Ljung")

# Resultado: p-value = 0.6047
# OBS: p-valor > 0.05. Não se rejeita H0, AO NÍVEL DE 0,05

# Visão gráfica da autocorrelação residual
par(mfrow=c(1,1))
acf(res, lag=20)

# Modelo AR(14)

# Xt = -0.150672(t-14) + e(t)
