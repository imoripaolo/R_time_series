# Let's set a working directory and load packages
# Vamos definir uma área de trabalho e carregar pacotes
setwd("~/R/Time Series")
library(readxl)
library(lmtest)
library(FitAR)

# Let's load the database
# Vamos carregar a base de dados
MICHIGAN <- read_excel("MICH.xlsx")

# The ts function transforms object in "time series" if possible
# A função ts transforma o objeto em "série de tempo" se possível
MICH <- ts(MICHIGAN$MICH)

# Let's plot the series
# Vamos plotar a série
plot(MICH, type = "l")

# Let's take the acf and pacf of the series
# Vamos tirar o acf e o pacf da série
acf <- acf(MICH, lag.max=40)
pacf <- pacf(MICH, lag.max=40)

# At first, we think that this series is a AR(1) or MA(1), but let's see 
# how to estimate the model with different configurations
# A princípio, podemos pensar que essa série é um AR(1) ou MA(1), mas vamos 
# ver como estimar o modelo com diferentes configurações

# AR(1)
ar1 <- arima(MICH, order=c(1,0,0), include.mean = TRUE, method="ML")
coeftest(ar1) 
# Important: pay attention to the order of inputs in order=c(1,0,0)
# Important 2: include.mean = FALSE: regression without constant
# Importante: preste atenção na ordem dos inputs em order=c(1,0,0)
# Importante 2: include.mean = FALSE: regressão sem constante

# Residual Analysis / Análise dos Resíduos
plot(ar1$residuals)
acf(ar1$residuals, lag.max=40)
pacf(ar1$residuals, lag.max=40)

# Some specification tests
# Alguns testes de especificação
LB <- LjungBoxTest (ar1$residuals,k=2,StartLag=1)
plot(LB[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ar1$residuals)
qqline(ar1$residuals)

# MA(1)
ma1 <- arima(MICH, order=c(0,0,1), include.mean = TRUE, method="ML")
coeftest(ma1) 

plot(ma1$residuals)
acf(ma1$residuals, lag.max=40)
pacf(ma1$residuals, lag.max=40)

LB <- LjungBoxTest (ma1$residuals,k=2,StartLag=1)
plot(LB[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ma1$residuals)
qqline(ma1$residuals)


# ARMA(1,1)
arma11 <- arima(MICH, order=c(1,0,1), include.mean = TRUE, method="ML")
coeftest(arma11) 

plot(arma11$residuals)
acf(arma11$residuals, lag.max=40)
pacf(arma11$residuals, lag.max=40)

LB <- LjungBoxTest (arma11$residuals,k=2,StartLag=1)
plot(LB[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(arma11$residuals)
qqline(arma11$residuals)

# AR(2)
ar2 <- arima(MICH, order=c(2,0,0), include.mean = TRUE, method="ML")
coeftest(ar2) 

plot(ar2$residuals)
acf(ar2$residuals, lag.max=40)
pacf(ar2$residuals, lag.max=40)

LB <- LjungBoxTest (ar2$residuals,k=2,StartLag=1)
plot(LB[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ar2$residuals)
qqline(ar2$residuals)

