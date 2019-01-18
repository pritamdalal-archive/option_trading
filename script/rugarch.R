library(rugarch)



data(sp500ret)
cluster <- makePSOCKcluster(15)

spec <- ugarchspec()
show(spec)


nrow(expand.grid(GARCH = 1:14, VEX = 0:1, VT = 0:1, Mean = 0:1, ARCHM = 0:2, ARFIMA = 0:1, MEX = 0:1, DISTR = 1:10))



fit <-
    ugarchfit(spec, sp500ret[1:1000, , drop = FALSE], solver = 'hybrid')


summary(fit)

df_sp500ret <- as.tibble(sp500ret)

sd(sp500ret[,1])

df_sp500ret  %>% .$SP500RET %>% sd() * sqrt(252)

ugfore <- ugarchforecast(fit, n.ahead = 21)

ugfore@forecast$sigmaFor %>% mean() * sqrt(252)
