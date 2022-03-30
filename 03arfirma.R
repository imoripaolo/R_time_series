# Let's set a working directory and load packages
# Vamos definir uma área de trabalho e carregar pacotes
library(arfima)

# We can simulate an arfima model with the arfima.sim function
# Podemos simular um modelo arfirma como a função arfirma.sim
set.seed(6533)

# Let's test the function a bit
# Vamos testar um pouco a função

sim <- arfima.sim(1000, model = list(phi = .2, dfrac = .3, dint = 2))
plot(sim)
# Note: dfrac and dint: fractional and integer parts of I
# Nota: dfrac e dint: partes fracionária e inteira de I

sim2 <- arfima.sim(1000, model = list(phi = .3, dfrac = .3, dint = 2))
plot(sim2)

sim3 <- arfima.sim(1000, model = list(phi = .2, dfrac = .2, dint = 2))
plot(sim3)

sim4 <- arfima.sim(1000, model = list(phi = .2, dfrac = .3, dint = 1))
plot(sim4)

sim5 <- arfima.sim(1000, model = list(phi = .2, dfrac = .3, dint = 2, theta = .1))
plot(sim5)

# We can fit an arfima model with the arfima function
# Podemos ajustar um modelo arfirma com a função arfirma
fit <- arfima(sim, order = c(1, 2, 0))
fit
# Note: order = c(p, d, q), and d is the integer part
# Nota: order = c(p, d, q), e d é a parte inteira

# We can test many different configurations
# Podemos testar várias configurações diferentes
fit2 <- arfima(sim, order = c(1, 1, 0))
fit2

fit3 <- arfima(sim, order = c(2, 2, 0))
fit3

fit4 <- arfima(sim, order = c(1, 2, 1))
fit4

