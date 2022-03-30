# Let's set a working directory and load packages
# Vamos definir uma área de trabalho e carregar pacotes
library(readxl)
library(forecast)

# We'll utilize the MICH database / Vamos utilizar a base MICH
michigan <- read_excel("MICH.xlsx")

# We'll separate observations in 2 parts
# Vamos separar observações em 2 partes
mich1 <- ts(michigan$MICH[1:100])
mich2 <- ts(michigan$MICH[101:120])

# We'll use mich1 for inference and try to forecast in mich2
# Vamos utilizar mich 2 para inferência e fazer previsão em mich2

# The ARIMA model / O modelo ARIMA 
mich_reg <- Arima(mich1, order=c(1,0,0), include.mean = TRUE, method="ML")

# Fitted Values / Valores Ajustados
plot(mich1, type = "l", col = "red")
lines(mich_reg[["fitted"]], type = "l", col = "blue")

# Forecast for the last 20 observations
# Previsão para as últimas 20 observações
forecast <- forecast(mich2, model=mich_reg)
plot(mich2, type = "l", col = "red")
lines(forecast[["fitted"]], type = "l", col = "blue")

plot(forecast[["residuals"]], type = "l", col = "red")
