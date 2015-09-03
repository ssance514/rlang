getSymbols("FB")
getSymbols("DIA")

difFB <- diff(FB)
difDIA <- diff(DIA)

difFB40 <- tail(difFB, n=40)
difDIA40 <- tail(difDIA, n=40)

FB40 <- tail(FB, n=40)
DIA40 <- tail(DIA, n=40)

difQQQ <- diff(QQQ)
difQQQ40 <- tail(difQQQ, n=40)
QQQ40 <- tail(QQQ, n=40)

val40 <- function(sym) {

difstk <- diff(sym)
dif40 <- tail(difstk, n=40)

stk40 <- tail(sym, n=40)

### chartSeries(dif40)
### chartSeries(stk40)

return(stk40)

}

dif40 <- function(sym) {

difstk <- diff(sym)
dif40 <- tail(difstk, n=40)

stk40 <- tail(sym, n=40)

### chartSeries(dif40)
### chartSeries(stk40)

return(dif40)

}


chartSeries(FB40)
chartSeries(DIA40)

chartSeries(difFB40)
chartSeries(difDIA40)



summary(FB40)
sd(FB40[,4])
mean(FB40[,4])
median(FB40[,4])


cor(FB40)
cor(difFB40)
cov(FB40)
cov(difFB40)


cor(DIA40)
cor(difDIA40)
cov(DIA40)
cov(difDIA40)

hist(FB40[,4], prob=T)
lines(density(FB40[,4]))

hist(difFB40[,4], prob=T)
lines(density(difFB40[,4]))

hist(DIA40[,4], prob=T)
lines(density(DIA40[,4]))

hist(difDIA40[,4], prob=T)
lines(density(difDIA40[,4]))


t.test(FB40[,4])
t.test(difFB40[,4])
t.test(difFB40[,1])
t.test(difFB40[,2])
t.test(difFB40[,3])


t.test(DIA40[,4])
t.test(difDIA40[,4])



shapiro.test(as.tx(FB40))
shapiro.test(as.ts(difFB40))

shapiro.test(as.tx(DIA40))
shapiro.test(as.ts(difDIA40))

acf(FB40)
acf(FB40[,4])
acf(difFB40[,4])
acf(difFB40)

pacf(FB40[,4])
pacf(difFB40[,4])

ccf(as.ts(FB40[,4]), as.ts(FB40[,5]))
ccf(as.ts(difFB40[,4]), as.ts(difFB40[,5]))

Box.test(FB40[,4])
Box.test(as.xts(FB40[,4]))
Box.test(difFB40[,4])
Box.test(as.xts(difFB40[,4]))

cor.test(as.ts(FB40[,4]), as.ts(FB40[,5]))
cor.test(as.ts(difFB40[,4]), as.ts(difFB40[,5]))
cor.test(as.ts(difFB40[,4]), as.ts(FB40[,5]))
cor.test(as.ts(FB40[,4]), as.ts(difFB40[,5]))

cor.test(as.ts(DIA40[,4]), as.ts(DIA40[,5]))
cor.test(as.ts(difDIA40[,4]), as.ts(difDIA40[,5]))
cor.test(as.ts(difDIA40[,4]), as.ts(DIA40[,5]))
cor.test(as.ts(DIA40[,4]), as.ts(difDIA40[,5]))


chartSeries(DIA40)
chartSeries(FB40)

mean(as.ts(FB40[,4] > 80))
mean(as.ts(difFB40[,4]) > 0.0)
mean(as.ts(difFB40[,4]) > 0.5)
mean(as.ts(difFB40[,4]) < -0.5)


### 31st of August, 2015

appleface  <- c("AAPL", "FB")

getSymbols(appleface, src='google', from="2014-01-1", to=Sys.Date())
### s1  <- data.frame(as.xts(AAPL, FB))
s2  <- data.frame(as.xts(merge(AAPL, FB)))

s2[1:40,1:10]

s2[50:60,1:10]


yearlyReturn(FB)
monthlyReturn(FB)
weeklyReturn(FB)
dailyReturn(FB)
allReturn(FB)

FB[1:20,]
FB[20:40,]

cor(FB[40:60,])

fb <- as.xts(FB)

aapl <- read.csv("C:\\rlang\\stocks\\aapl.csv")

cor(aapl[1:40, 2:6])

dim(aapl)
dim(FB)

corarr <- 0

for (i in 1:(dim(FB)[1] - 9)) {

cortenday <- cor(FB[i:(i+9), 4:5])
print(cortenday[1,2])

corarr <- c(corarr, cortenday[1,2])

}
