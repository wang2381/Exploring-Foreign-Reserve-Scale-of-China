
require(urca)
require(TSA)
require(CADFtest)
ecodata <- read.csv("I:/data/ecodata.csv")
ecodata<- ecodata[,c("reserve","EX","IM","FDI","SD","exrate")]
ecodata<- apply(ecodata,2,log)
ecodatats<- ts(ecodata,start = 1985,end = 2015,frequency = 1)
reserve<- ecodatats[,"reserve"]
reserve<- exp(reserve)
colnames(ecodatats)<- c("lnreserve","lnEX", "lnIM","lnFDI", "lnSD", "lnexrate")
oldpar=par
par(mfrow=c(1,2))
plot(reserve,type="o",main="reserve plot")
plot(diff(reserve),type="o",main=" diff reserve plot")
par=oldpar
CADFtest(reserve,type = "trend",max.lag.y = 8,criterion = "BIC")
CADFtest(diff(reserve),type = "trend",max.lag.y = 8,criterion = "BIC")
oldpar=par
par(mfrow=c(2,2))
acf(reserve)
pacf(reserve)
acf(diff(reserve))
pacf(diff(reserve))
par=oldpar
eacf(diff(reserve),ar.max=5,ma.max=5)


fit_reserve_1<- arima(reserve,order = c(1,1,0))
fit_reserve_1
fit_reserve_2<- arima(reserve,order = c(0,1,3))
fit_reserve_2
fit_reserve_3<- arima(reserve,order = c(2,1,1))
fit_reserve_3
# arima model arima(1,1,0)
fit_reserve<- arima(reserve,order = c(1,1,0))
fit_reserve
oldpar=par
tsdiag(fit_reserve)
par=oldpar
fitresid<- resid(fit_reserve)
runs.test(factor(fitresid>0))
par(mfrow=c(1,1))
qqnorm(fitresid)
qqline(fitresid)
shapiro.test(fitresid)
# predictions
predict(fit_reserve,n.ahead=5)$pred
plot(fit_reserve,n.ahead=5,pch=19,main="prediction plot")
# reserve
par(mfrow=c(1,2))
plot(ecodatats[,1])
plot(diff(ecodatats[,1]))
CADFtest(reserve,type = "trend",max.lag.y = 8,criterion = "BIC")
CADFtest(diff(reserve),type = "trend",max.lag.y = 8,criterion = "BIC")
# EX
par(mfrow=c(1,2))
plot(ecodatats[,2])
plot(diff(ecodatats[,2]))
CADFtest(ecodatats[,"lnEX"],type = "trend",max.lag.y = 8,criterion = "BIC")
CADFtest(diff(ecodatats[,"lnEX"]),type = "trend",max.lag.y = 8,criterion = "BIC")
# IM
par(mfrow=c(1,2))
plot(ecodatats[,3])




plot(diff(ecodatats[,3]))
CADFtest(ecodatats[,"lnIM"],type = "trend",max.lag.y = 8,criterion = "BIC")
CADFtest(diff(ecodatats[,"lnIM"]),type = "none",max.lag.y = 8,criterion = "BIC")
# FDI
par(mfrow=c(1,2))
plot(ecodatats[,4])
plot(diff(ecodatats[,4]))
CADFtest(ecodatats[,"lnFDI"],type = "trend")
CADFtest(diff(ecodatats[,"lnFDI"]),type = "trend")
# SD
par(mfrow=c(1,2))
plot(ecodatats[,5])
plot(diff(ecodatats[,5]))
CADFtest(ecodatats[,"lnSD"],type = "trend",max.lag.y = 8,criterion = "BIC")
CADFtest(diff(ecodatats[,"lnSD"]),type = "trend",max.lag.y = 8,criterion = "BIC")
# exrate
par(mfrow=c(1,2))
plot(ecodatats[,6])
plot(diff(ecodatats[,6]))
CADFtest(ecodatats[,"lnexrate"],type = "none",max.lag.y = 8,criterion = "BIC")
CADFtest(diff(ecodatats[,"lnexrate"]),type = "none",max.lag.y = 8,criterion = "BIC")
par(mfrow=c(1,1))
fit_lm<- lm(lnreserve ~lnEX+lnIM+lnFDI,data = ecodatats)
summary(fit_lm)
qqnorm(fit_lm$residuals)
qqline(fit_lm$residuals)
shapiro.test(fit_lm$residuals)
CADFtest(fit_lm$residuals,type = "none",max.lag.y = 8,criterion = "BIC")
dataFull<- data.frame(apply(ecodatats,2,diff),res=fit_lm$residuals[-31])
colnames(dataFull)<- c("lnreserve.d","lnEX.d","lnIM.d","lnFDI.d","lnSD.d","lnexrate.d","res.l1")
fit_ecm<- lm(lnreserve.d ~lnEX.d+lnIM.d+lnFDI.d+res.l1,data = dataFull)
summary(fit_ecm)

