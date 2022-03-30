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

# We'll create a variable in log scale
# Vamos criar a variável em escala log
r5 <- quarterly_7775706$r5
ln_r5 <- ts(log(quarterly_7775706$r5))
diff_ln_r5 <- c()
for (t in 1:211) {diff_ln_r5[t] <- ts(ln_r5[t+1] - ln_r5[t])}

# Why not log the "interest" variable of the previous video?
# Por que não aplicar o log na variável "interest" do vídeo anterior?
plot(r5, type="l")
acf_level <- acf(r5, lag.max=40) 
pacf_level <- pacf(r5, lag.max=40) 

plot(ln_r5, type="l")
acf_level <- acf(ln_r5, lag.max=40) 
pacf_level <- pacf(ln_r5, lag.max=40) 

plot(diff_ln_r5, type="l")
acf_level <- acf(diff_ln_r5, lag.max=40) 
pacf_level <- pacf(diff_ln_r5, lag.max=40) 

# Therefore, we can use log or difference operators to generate a
# stationary time series
# Assim, podemos utilizar operadores log ou diferença para gerar uma série
# de tempo estacionária.

# Now, we can utilize the previous procedings to estimate the model
# Agora, podemos utilizar os procedimentos anteriores para estimar o modelo


