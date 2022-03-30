# Let's set a working directory and load packages
# Vamos definir uma área de trabalho e carregar pacotes
library(mFilter)
data(unemp)
plot(unemp,main="Unemployment Series",col=1,ylab="")

# The Baxter-King Filter / Filtro Baxter King
opar <- par(no.readonly=TRUE)
unemp.bk <- bkfilter(unemp)
plot(unemp.bk)
unemp.bk1 <- bkfilter(unemp, drift=TRUE)
unemp.bk2 <- bkfilter(unemp, pl=8,pu=40,drift=TRUE)
unemp.bk3 <- bkfilter(unemp, pl=2,pu=60,drift=TRUE)
unemp.bk4 <- bkfilter(unemp, pl=2,pu=40,drift=TRUE)
par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(unemp.bk1$x,
     main="Baxter-King filter of unemployment: Trend, drift=TRUE",
     col=1, ylab="")
lines(unemp.bk1$trend,col=2)
lines(unemp.bk2$trend,col=3)
lines(unemp.bk3$trend,col=4)
lines(unemp.bk4$trend,col=5)
legend("topleft",legend=c("series", "pl=2, pu=32", "pl=8, pu=40",
                          "pl=2, pu=60", "pl=2, pu=40"), col=1:5, lty=rep(1,5), ncol=1)
plot(unemp.bk1$cycle,
     main="Baxter-King filter of unemployment: Cycle,drift=TRUE",
     col=2, ylab="", ylim=range(unemp.bk3$cycle,na.rm=TRUE))
lines(unemp.bk2$cycle,col=3)
lines(unemp.bk3$cycle,col=4)
lines(unemp.bk4$cycle,col=5)
legend("topleft",legend=c("pl=2, pu=32", "pl=8, pu=40", "pl=2, pu=60",
                          "pl=2, pu=40"), col=1:5, lty=rep(1,5), ncol=1)
par(opar)

# The Christiano-Fitzgerald Filter / Filtro Christiano-Fitzgerald

unemp.cf <- cffilter(unemp)
plot(unemp.cf)
unemp.cf1 <- cffilter(unemp, drift=TRUE, root=TRUE)
unemp.cf2 <- cffilter(unemp, pl=8,pu=40,drift=TRUE, root=TRUE)
unemp.cf3 <- cffilter(unemp, pl=2,pu=60,drift=TRUE, root=TRUE)
unemp.cf4 <- cffilter(unemp, pl=2,pu=40,drift=TRUE, root=TRUE,theta=c(.1,.4))
par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(unemp.cf1$x,
     main="Christiano-Fitzgerald filter of unemployment: Trend \n root=TRUE,drift=TRUE",
     col=1, ylab="")
lines(unemp.cf1$trend,col=2)
lines(unemp.cf2$trend,col=3)
lines(unemp.cf3$trend,col=4)
lines(unemp.cf4$trend,col=5)
legend("topleft",legend=c("series", "pl=2, pu=32", "pl=8, pu=40", "pl=2, pu=60",
                          "pl=2, pu=40, theta=.1,.4"), col=1:5, lty=rep(1,5), ncol=1)
plot(unemp.cf1$cycle,
     main="Christiano-Fitzgerald filter of unemployment: Cycle \n root=TRUE,drift=TRUE",
     col=2, ylab="", ylim=range(unemp.cf3$cycle))
lines(unemp.cf2$cycle,col=3)
lines(unemp.cf3$cycle,col=4)
lines(unemp.cf4$cycle,col=5)
legend("topleft",legend=c("pl=2, pu=32", "pl=8, pu=40", "pl=2, pu=60",
                          "pl=2, pu=40, theta=.1,.4"), col=2:5, lty=rep(1,4), ncol=2)
par(opar)

# The Hodrick-Prescott Filter / Filtro HP
unemp.hp <- hpfilter(unemp)
plot(unemp.hp)
unemp.hp1 <- hpfilter(unemp, drift=TRUE)
unemp.hp2 <- hpfilter(unemp, freq=800, drift=TRUE)
unemp.hp3 <- hpfilter(unemp, freq=12,type="frequency",drift=TRUE)
unemp.hp4 <- hpfilter(unemp, freq=52,type="frequency",drift=TRUE)
par(mfrow=c(2,1),mar=c(3,3,2,1),cex=.8)
plot(unemp.hp1$x, ylim=c(2,13),
     main="Hodrick-Prescott filter of unemployment: Trend, drift=TRUE",
     col=1, ylab="")
lines(unemp.hp1$trend,col=2)
lines(unemp.hp2$trend,col=3)
lines(unemp.hp3$trend,col=4)
lines(unemp.hp4$trend,col=5)
legend("topleft",legend=c("series", "lambda=1600", "lambda=800",
                          "freq=12", "freq=52"), col=1:5, lty=rep(1,5), ncol=1)
plot(unemp.hp1$cycle,
     main="Hodrick-Prescott filter of unemployment: Cycle,drift=TRUE",
     col=2, ylab="", ylim=range(unemp.hp4$cycle,na.rm=TRUE))
lines(unemp.hp2$cycle,col=3)
lines(unemp.hp3$cycle,col=4)
lines(unemp.hp4$cycle,col=5)
legend("topleft",legend=c("lambda=1600", "lambda=800",
                          "freq=12", "freq=52"), col=1:5, lty=rep(1,5), ncol=1)

# Let's compare the filters / Vamos comparar os filtros

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,1),mar=c(3,3,2,1))
plot(unemp,main="Unemployment Series & Estimated Trend",col=1,ylab="")
lines(unemp.hp$trend,col=2)
lines(unemp.bk$trend,col=3)
lines(unemp.cf$trend,col=4)
legend("topleft",legend=c("series", "HP","BK","CF"),col=1:4,
       lty=rep(1,4),ncol=2)
plot(unemp.hp$cycle,main="Estimated Cyclical Component",col=2,
     ylim=c(-2,2),ylab="")
lines(unemp.bk$cycle,col=3)
lines(unemp.cf$cycle,col=4)
legend("topleft",legend=c("HP","BK","CF"),col=2:4,lty=rep(1,3),ncol=2)
par(opar)
