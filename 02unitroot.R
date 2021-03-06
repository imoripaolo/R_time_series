# Let's set a working directory and load packages
# Vamos definir uma �rea de trabalho e carregar pacotes
setwd("~/R/Time Series")
library(tseries)

# For this, we'll use databases included on the tseries package
# Agora, vamos usar bases de dados incluidas no pacote tseries

# First the camp database. Let's load it and plot it
# Primeiro, a base camp. Vamos carregar e plotar
data(camp)
plot(camp)

# The same for the bev database.
# O mesmo para a base bev
data(bev)
plot(bev)

# And now for the NelPlo. We'll use the real.wages series
# Agora para a NelPlo. Vamos usara  s�rie real.wages
data(NelPlo)
plot(real.wages)


# Let's see how to perform the ADF Test
# Vamos ver como fazer o teste ADF
x <- rnorm(1000) # no unit-root / sem raiz unit�ria
plot(x)
adf.test(x)
y <- diffinv(x) # unit root / com raiz unit�ria
plot(y)
adf.test(y)

# Now, we'll perform the ADF Test in the selected TS
# Agora, vamos fazer o teste ADF nas s�ries selecionadas
adf.test(camp, alternative = c("stationary"),
         k = trunc((length(camp)-1)^(1/3)))
adf.test(camp, alternative = c("explosive"),
                     k = trunc((length(camp)-1)^(1/3)))
# Note we can select the alternative hypothesis. Check documentation!
# Note que podemos selecionar a hip�tese alternativa. Verifique documenta��o!

adf.test(bev, alternative = c("stationary"),
                     k = trunc((length(bev)-1)^(1/3)))
adf.test(bev, alternative = c("explosive"),
                     k = trunc((length(bev)-1)^(1/3)))

# Now, for the KPSS Test. Let's also see the x and y examples.
# Agora o teste KPSS. Vamos ver tamb�m os exemplos x e y.
x <- rnorm(1000) # level stationary / estacion�ria em n�vel
kpss.test(x)
y <- cumsum(x) # unit root / raiz unit�ria
kpss.test(y)
z <- 0.3*(1:1000)+rnorm(1000) # trend stationary / tend�ncia estacion�ria
kpss.test(z, null = "Trend")

# For the camp series
# Para a s�rie camp
kpss.test(camp, lshort = TRUE)
kpss.test(camp, null = c("Trend"), lshort = TRUE)

# The Phillips-Perron test
# O Teste Phillips-Perron
x <- rnorm(1000) # no unit-root / sem raiz unit�ria
pp.test(x)
y <- cumsum(x) # unit root / raiz unit�ria
pp.test(y)

# Also, for the camp series
# Novamente, para a s�rie camp
pp.test(camp, alternative = c("stationary"),
        type = c("Z(alpha)"), lshort = TRUE)
pp.test(camp, alternative = c("explosive"),
        type = c("Z(alpha)"), lshort = TRUE)

