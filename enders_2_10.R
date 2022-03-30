# Base: Section 2.10 Applied Econometric Time Series, by Enders
# Base: Seção 2.10 Applied Econometric Time Series, do Enders

# Let's set a working directory and load packages
# Vamos definir uma área de trabalho e carregar pacotes
setwd("~/R/Enders")
library(readxl)
library(FitAR)

# Loading Databases. Download it at Enders's site
# Carregando base de dados. Faça download no site do Enders
quarterly_7775706 <- read_excel("quarterly.7775706.xls")
View(quarterly_7775706)

# We'll create a variable according to the example
# Vamos criar a variável conforme o exemplo
interest <- ts(quarterly_7775706$r5 - quarterly_7775706$Tbill)

# Let's check general aspects of the series
# Vamos olhar para a série de forma geral
plot(interest, type="l")
acf_level <- acf(interest, lag.max=40) 
pacf_level <- pacf(interest, lag.max=40) 

# Let's suppose max p = 2 and max q = 6 
# Vamos supor max p = 2 e max q = 6

# Basic ARMA example: let's check what the model return us:
# Exemplo ARMA básico: vamos ver o que o modelo nos retorna:
example <- arima(interest, order=c(1,0,0), include.mean = TRUE, method="ML")
example

# AIC Selection criteria / Critério de Seleção AIC:
example[["aic"]]

# How can we use the AIC to select a model?
# Como podemos utilizar o AIC para selecionar um modelo
p1q0 <- arima(interest, order=c(1,0,0), include.mean = 
                TRUE, method="ML")[["aic"]]
p2q0 <- arima(interest, order=c(2,0,0), include.mean = 
                TRUE, method="ML")[["aic"]]
p1q1 <- arima(interest, order=c(1,0,1), include.mean = 
                TRUE, method="ML")[["aic"]]
p2q1 <- arima(interest, order=c(2,0,1), include.mean = 
                TRUE, method="ML")[["aic"]]
p1q2 <- arima(interest, order=c(1,0,2), include.mean = 
                TRUE, method="ML")[["aic"]]
p2q2 <- arima(interest, order=c(2,0,2), include.mean = 
                TRUE, method="ML")[["aic"]]
p1q3 <- arima(interest, order=c(1,0,3), include.mean = 
                TRUE, method="ML")[["aic"]]
p2q3 <- arima(interest, order=c(2,0,3), include.mean = 
                TRUE, method="ML")[["aic"]]
p1q4 <- arima(interest, order=c(1,0,4), include.mean = 
                TRUE, method="ML")[["aic"]]
p2q4 <- arima(interest, order=c(2,0,4), include.mean = 
                TRUE, method="ML")[["aic"]]
p1q5 <- arima(interest, order=c(1,0,5), include.mean = 
                TRUE, method="ML")[["aic"]]
p2q5 <- arima(interest, order=c(2,0,5), include.mean = 
                TRUE, method="ML")[["aic"]]
p1q6 <- arima(interest, order=c(1,0,6), include.mean = 
                TRUE, method="ML")[["aic"]]
p2q6 <- arima(interest, order=c(2,0,6), include.mean = 
                TRUE, method="ML")[["aic"]]

aic <- rbind(c(p1q0,p1q1,p1q2,p1q3,p1q4,p1q5,p1q6),
             c(p2q0,p2q1,p2q2,p2q3,p2q4,p2q5,p2q6))
aic
which.min(aic[1,])
which.min(aic[2,])

# Let's assume an ARMA(6,2) / Vamos supor um ARMA(6,2)
reg <- arima(interest, order=c(2,0,6), include.mean = TRUE, method="ML")
reg

# We can also check the model in log scale and difference scale


