# We'll simulate some TS / Vamos simular algumas s�ries de tempo

# White Noise / Ru�do Branco
epsilon <- rnorm(100,0,1)
plot(epsilon, type = "l")
# We won't set.seed, as it won't matter for this, but do as you will
# N�o vamos fazer set.seed, pois n�o importa agora, mas fa�a como quiser

# From epsilon, we'll manually simulate some series
# A partir de epsilon, vamos simular manualmente algumas s�ries

# AR(1) - Let's test coeficients 0.5, 0.1 and 0.9
# AR(1) - Vamos ver coeficientes 0.5, 0.1 e 0.9
y <- c()
y[1] <- epsilon[1]
for (i in 2:100) {y[i] <- 0.5*y[i-1] + epsilon[i]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

y <- c()
y[1] <- epsilon[1]
for (i in 2:100) {y[i] <- 0.1*y[i-1] + epsilon[i]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

y <- c()
y[1] <- epsilon[1]
for (i in 2:100) {y[i] <- 0.9*y[i-1] + epsilon[i]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

# MA(2)
y <- c()
y[1] <- epsilon[1]
y[2] <- epsilon[2] + 0.2*epsilon[1]
for (i in 3:100) {y[i] <- epsilon[i] + 0.2*epsilon[i-1] + 0.6*epsilon[i-2]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

# Unit Root Series / S�rie com Raiz Unit�ria
y <- c()
y[1] <- epsilon[1]
for (i in 2:100) {y[i] <- y[i-1] + epsilon[i]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

# Series with Determinist Trend / S�rie com Tend�ncia Determin�stica
y <- c()
y[1] <- epsilon[1]
for (i in 2:100) {y[i] <- 0.05*i + 0.5*y[i-1] + epsilon[i]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

# Series with both trends / S�rie com as duas tend�ncias
y <- c()
y[1] <- epsilon[1]
for (i in 2:100) {y[i] <- 0.05*i + y[i-1] + epsilon[i]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

## ARCH(1)
h <- c()
h[1] <- epsilon[1]
for (i in 2:100) {h[i] <- sqrt(1.7 + 0.5*h[i-1])*epsilon[i] }
plot(h, type="l",col = "red")
lines(epsilon, type="l", col = "blue")
# Note: be careful with sqrt as to not generate NaNs
# Nota: cuidado com a raiz quadrada para n�o gerar NaNs

y <- c()
y[1] <- h[1]
for (i in 2:100) {y[i] <- 0.5*y[i-1] + h[i]}
plot(y, type="l",col = "red")
lines(h, type="l", col = "blue")
lines(epsilon, type="l", col = "green")

# Non gaussian errors / Erros n�o normais
u <- runif(100,-1,1)
plot(epsilon, type="l",col = "red")
lines(u, type="l", col = "blue")

y1 <- c()
y1[1] <- epsilon[1]
for (i in 2:100) {y1[i] <- 0.5*y1[i-1] + epsilon[i]}
y2 <- c()
y2[1] <- u[1]
for (i in 2:100) {y2[i] <- 0.5*y2[i-1] + u[i]}

plot(y1, type="l",col = "red")
lines(y2, type="l", col = "blue")


# Series with Break / S�rie com Quebra
y <- c()
y[1] <- epsilon[1]
for (i in 2:60) {y[i] <- y[i-1] + epsilon[i]}
for (i in 61:100) {y[i] <- 0.5*y[i-1] + epsilon[i]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

# Seasonality / Sazonalidade
y <- c()
y[1] <- epsilon[1]
for (i in 2:4) {y[i] <- 0.5*y[i-1] + epsilon[i]}
for (i in 5:100) {y[i] <- 0.5*y[i-1] + epsilon[i] + 0.5*y[i-4]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

# Random Parameters / Par�metros Aleat�rios
phi <- runif(100,0,1)
y <- c()
y[1] <- epsilon[1]
for (i in 2:100) {y[i] <- phi[i]*y[i-1] + epsilon[i]}
plot(y, type="l",col = "red")
lines(epsilon, type="l", col = "blue")

# Function arima.sim: simulate arima models
# Fun��o arima.sim: simule modelos arima

arima_sim <-arima.sim(n = 100, 
                      list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))
plot(arima_sim)


arima_sim2 <-arima.sim(n = 100, list(order = c(1,1,0), ar=0.5), innov=epsilon)
plot(arima_sim2)
