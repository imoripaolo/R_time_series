# Let's set a working directory and load packages
# Vamos definir uma área de trabalho e carregar pacotes
library(neverhpfilter)

# For the Hamilton filter, we'll check the GDPC1 database
# Para o filtro de Hamilton, vamos utilizar a base GCPC1
data(GDPC1)
gdp_filter <- yth_filter(100*log(GDPC1), h = 8, p = 4)
knitr::kable(head(gdp_filter, 15), align = 'l')
plot(gdp_filter$GDPC1.trend ,main="Trend",col=2 ,ylab="")
plot(gdp_filter$GDPC1.cycle ,main="Cycle",col=2 ,ylab="")
plot(gdp_filter$GDPC1.random ,main="Random",col=2 ,ylab="")

