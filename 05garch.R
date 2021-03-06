# Let's set a working directory and load packages
# Vamos definir uma �rea de trabalho e carregar pacotes
library(readxl)
library(forecast)
library(tseries)

# We'll utilize the MICH database / Vamos utilizar a base MICH
michigan <- read_excel("MICH.xlsx")
mich1 <- ts(michigan$MICH)


# The ARIMA Model / O Modelo ARIMA
reg <- arima(mich1, order=c(1,0,0), include.mean = TRUE, method="ML")

# Let's check the residuals of the regression
# Vamos verificar os res�duos da regress�o
residuals <- reg[["residuals"]]
plot(residuals, type="l")

# We can take the acf and pacf of residuals
# Vamos verificar o acf e pacf dos res�duos
residual_acf <- acf(residuals, lag.max=40)
residual_pacf <- pacf(residuals, lag.max=40)

# We can model the residuals with an ARIMA model
# Podemos modelar os res�duos com um modelo ARIMA
residual_reg <- arima(residuals, order=c(1,0,0), include.mean = TRUE, method="ML")
summary(residual_reg)

# Or we can use the garch function
# Ou ent�o podemos utilizar a fun��o garch
mich_res <- garch(mich1, order = c(1, 0), series = "h")
mich_res[["coef"]]

fitted_errors <- mich_res[["fitted.values"]]
plot(fitted_errors, type="l")

