library(forecast)
library(ggplot2)
Caposele_fill_Date <- read.csv("C:/Dati/RNNQ/Caposele_fill_Date.csv")
cfl<-Caposele_fill_Date
cfl$X<-as.Date(cfl$X)

cfl.ts1<-ts(cfl$Portate, freq=365.89011368571722328919844818685)
cfl.ts2<-ts(cfl$Portate, freq=1958.5883905064870406081965357272)

aic_vals<-NULL
aic_vals_temp<-NULL
for(i in 1:5)
{
  for(j in 1:5)
  {
    xreg1<-fourier(cfl.ts1,K = i)
    xreg2<-fourier(cfl.ts2,K = j)
    xtrain<-cbind(xreg1, xreg2)
    fitma1<-auto.arima(cfl$Portate, D = 0, max.P = 0, max.Q = 0, xreg = xtrain)
    aic_vals_temp<-cbind(i, j, fitma1$aic)
    aic_vals<-rbind(aic_vals, aic_vals_temp)
  }
  
}
colnames(aic_vals)<-c("FourierTerms365", "FourierTerms1959", "AicValue")
aic_vals<-data.frame(aic_vals)
minAICVal<-min(aic_vals$AICValue)
minvals<-aic_vals[which(aic_vals$AICValue == minAICVal),]
ggplot(data = aic_vals, aes(x = aic_vals$FourierTerms365, y = aic_vals$FourierTerms1959)) + geom_tile(aes(fill = aic_vals$AicValue))
cfl_pred<-predict(fitma1, newxreg = xtrain)