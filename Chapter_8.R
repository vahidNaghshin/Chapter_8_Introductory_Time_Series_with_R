lan <- "/home/vnagh/R_test/Introductory_Time_Series_with_R_datasets/LAN.txt"
lan <- read.table(lan, header=T)


png(
  "bitsLAN.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
plot(lan[,1], type='l')
dev.off()
browseURL("bitsLAN.png")

png(
  "Boxplot&histLAN.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 4.1, 1.1, 2.1))
boxplot(lan[,1], horizontal=TRUE,  outline=FALSE)
hist(lan[,1], prob=T, xlab='bits', main='Histogram of bits in LAN')
dev.off()
browseURL("Boxplot&histLAN.png")

library(e1071) 
skewness(lan[,1]) 
# Distributions with kurtosis greater than 3 are said to be leptokurtic 
# (produces more outliers than the normal distribution)
kurtosis(lan[,1])

png(
  "Boxplot&histLAN_log.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 4.1, 1.1, 2.1))
boxplot(log(lan[,1]+1), horizontal=TRUE,  outline=FALSE)
hist(log(lan[,1]+1), prob=T, xlab='bits', main='Histogram of ln(bits+1) in LAN')
dev.off()
browseURL("Boxplot&histLAN_log.png")

library(e1071) 
skewness(log(lan[,1]+1)) 
# Distributions with kurtosis greater than 3 are said to be leptokurtic 
# (produces more outliers than the normal distribution)
kurtosis(log(lan[,1]+1)) 

library(fracdiff)
fds.fit <- fracdiff(log(lan[,1]+1), nar=1)

x <- log(lan[,1]+1)
n <- length(x)
L <- 30
d <- fds.fit$d
fdc <- d
fdc[1] <- fdc
for (k in 2:L) fdc[k] <- fdc[k-1] * (d+1-k) / k
y <- rep(0, L)
for (i in (L+1):n) {
  csm <- x[i]
  for (j in 1:L) csm <- csm + ((-1)^j) * fdc[j] * x[i-j]
  y[i] <- csm
}
y <- y[(L+1):n]
z.ar <- ar(y)
print(z.ar$order)
ns <- 1 + z.ar$order
z <- z.ar$res [ns:length(y)]


png(
  "Boxplot&hist_AR_res_LAN.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 4.1, 1.1, 2.1))
boxplot(z, horizontal=TRUE,  outline=FALSE)
hist(z, prob=T, xlab='bits', main='Histogram of residual error after AR on fracdiff')
dev.off()
browseURL("Boxplot&hist_AR_res_LAN.png")

library(fracdiff)
fds.fit <- fracdiff(log(lan[,1]+1), nar=1)

x <- log(lan[,1]+1)
n <- length(x)
L <- 30
d <- fds.fit$d
fdc <- d
fdc[1] <- fdc
for (k in 2:L) fdc[k] <- fdc[k-1] * (d+1-k) / k
y <- rep(0, L)
for (i in (L+1):n) {
  csm <- x[i]
  for (j in 1:L) csm <- csm + ((-1)^j) * fdc[j] * x[i-j]
  y[i] <- csm
}
y <- y[(L+1):n]
z.arma <- arima(y, order=c(13,0,13))
# ns <- 1 + z.ar$order
# z <- z.ar$res [ns:length(y)]


png(
  "Boxplot&hist_ARMA_res_LAN.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 4.1, 1.1, 2.1))
boxplot(resid(z.arma), horizontal=TRUE,  outline=FALSE)
hist(resid(z.arma), prob=T, xlab='bits', main='Histogram of residual error after ARMA on fracdiff')
dev.off()
browseURL("Boxplot&hist_ARMA_res_LAN.png")


library(fracdiff)
fds.fit <- fracdiff(log(lan[,1]+1), nar=1)

x <- log(lan[,1]+1)
n <- length(x)
L <- 30
d <- fds.fit$d
fdc <- d
fdc[1] <- fdc
for (k in 2:L) fdc[k] <- fdc[k-1] * (d+1-k) / k
y <- rep(0, L)
for (i in (L+1):n) {
  csm <- x[i]
  for (j in 1:L) csm <- csm + ((-1)^j) * fdc[j] * x[i-j]
  y[i] <- csm
}
y <- y[(L+1):n]
z.ar <- ar(y, order.max = 2)
print(z.ar$order)
ns <- 1 + z.ar$order
z <- z.ar$res [ns:length(y)]


png(
  "Boxplot&hist_AR(2)_res_LAN.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 4.1, 1.1, 2.1))
boxplot(z, horizontal=TRUE,  outline=FALSE)
hist(z, prob=T, xlab='bits', main='Histogram of residual error after AR(2) on fracdiff')
dev.off()
browseURL("Boxplot&hist_AR(2)_res_LAN.png")



lan <- "/home/vnagh/R_test/Introductory_Time_Series_with_R_datasets/LAN.txt"
lan <- read.table(lan, header=T)

lan_20_ms <- rep(0, length(lan[,1])/2)
for (k in 1:length(lan[,1])/2) lan_20_ms[k] <-lan[,1][2*k-1]+lan[,1][2*k]


lan_40_ms <- rep(0, length(lan[,1])/4)
for (k in 1:length(lan[,1])/4) lan_40_ms[k] <-lan[,1][4*k-3]+lan[,1][4*k-2]+lan[,1][4*k-1]+lan[,1][4*k]

library(e1071) 
skewness(log(lan_20_ms+1)) 
kurtosis(log(lan_20_ms+1)) 

library(fracdiff)
fds.fit <- fracdiff(log(lan_20_ms+1), nar=1)

x <- log(lan_20_ms+1)
n <- length(x)
L <- 30
d <- fds.fit$d
fdc <- d
fdc[1] <- fdc
for (k in 2:L) fdc[k] <- fdc[k-1] * (d+1-k) / k
y <- rep(0, L)
for (i in (L+1):n) {
  csm <- x[i]
  for (j in 1:L) csm <- csm + ((-1)^j) * fdc[j] * x[i-j]
  y[i] <- csm
}
y <- y[(L+1):n]
z.ar <- ar(y)
print(z.ar$order)
ns <- 1 + z.ar$order
z <- z.ar$res [ns:length(y)]


png(
  "Boxplot&hist_AR_res_LAN_20ms.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 4.1, 1.1, 2.1))
boxplot(z, horizontal=TRUE,  outline=FALSE)
hist(z, prob=T, xlab='bits', main='Histogram of residual error of 20ms LAN bit after AR on fracdiff')
dev.off()
browseURL("Boxplot&hist_AR_res_LAN_20ms.png")


library(fracdiff)
fds.fit <- fracdiff(log(lan_40_ms+1), nar=1)

x <- log(lan_40_ms+1)
n <- length(x)
L <- 30
d <- fds.fit$d
fdc <- d
fdc[1] <- fdc
for (k in 2:L) fdc[k] <- fdc[k-1] * (d+1-k) / k
y <- rep(0, L)
for (i in (L+1):n) {
  csm <- x[i]
  for (j in 1:L) csm <- csm + ((-1)^j) * fdc[j] * x[i-j]
  y[i] <- csm
}
y <- y[(L+1):n]
z.ar <- ar(y)
print(z.ar$order)
ns <- 1 + z.ar$order
z <- z.ar$res [ns:length(y)]


png(
  "Boxplot&hist_AR_res_LAN_40ms.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 4.1, 1.1, 2.1))
boxplot(z, horizontal=TRUE,  outline=FALSE)
hist(z, prob=T, xlab='bits', main='Histogram of residual error of 40ms LAN bit after AR on fracdiff')
dev.off()
browseURL("Boxplot&hist_AR_res_LAN_40ms.png")



fed <- "/home/vnagh/R_test/Introductory_Time_Series_with_R_datasets/mprime.txt"
fed <- read.table(fed, header=T)

print(mean(fed[,1]))
png(
  "rand_walk_fed.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
acf(diff(fed[,1]-mean(fed[,1])))
dev.off()
browseURL("rand_walk_fed.png")

best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:5) for (j in 0:5) {fit.aic <- AIC(arima(fed[,1], order = c(i, 0, j)))}
if (fit.aic < best.aic) {
  best.order <- c(i, 0, j)
  best.arma <- arima(fed[,1], order = best.order)
  best.aic <- fit.aic
}

print(mean(fed[,1]))
png(
  "ARMA_fed.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
acf(resid(best.arma))
dev.off()
browseURL("ARMA_fed.png")



nile <- "/home/vnagh/R_test/Introductory_Time_Series_with_R_datasets/Nilemin.txt"
nile <- read.table(nile, header=T)

print(length(nile[,1]))
png(
  "nile.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
plot(nile[,1], type='l')
dev.off()
browseURL("nile.png")



R_m <- rep(0, length(nile[,1]))
for (i in 0:length(nile[,1]))
{
  print(i)
  mu <- mean(nile[1:i,1])
  sigma <- sqrt(var(nile[1:i,1]))
  s_k <- rep(0, i)
  for (k in 1:i) {
    for (z in 1:k) s_k[k] <- s_k[k] + nile[,1][z]
    s_k[k] <- s_k[k] - (k*mu)
  }  
  R_m[i] <- (max(s_k) - min(s_k))/sigma
}

png(
  "rescaledAdjRange.png",
  width     = 5.25,
  height    = 3.75,
  units     = "in",
  res       = 700,
  pointsize = 4
)
plot(log(1:length(nile[,1])), log(R_m), type='l')
dev.off()
browseURL("rescaledAdjRange.png")