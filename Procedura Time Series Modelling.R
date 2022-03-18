Caposele_fill_Date <- read.csv("C:/Users/Giovanni/PycharmProjects/RNNQ/Caposele_fill_Date.csv")
cfl<-Caposele_fill_Date
cfl$X<-as.Date(cfl$X)
#Trasformata di Fourier della serie temporale
cfou<-fft(cfl$Portate)
#partiziono i dati per la costruzione del periodogramma
cfou <- cfou[2:((length(cfou) / 2) + 1)]
#calcolo i valori delle ampiezze dei valori della trasformata
ampl <- Mod(cfou) / length(cfou)
#calcolo i valori delle frequenze dei valori della trasformata
freq <- (1:(33296/ 2))/33296
#calcolo i valori delle Fasi dei valori della trasformata
phase<-Arg(cfou)
#Plotto il periodogramma
plot(freq[0:100], ampl[0:100], type='l')
t<-(1:33296)
#Aggungo al grafico le linee corrispondenti alle armoniche di un determinato periodo
abline(v=1/(1*365), lty='dotted')
#calcolo il segnale per una particolare frequenza scelta dal periodogramma
sig<-ampl[91]*cos(2*pi*freq[91]*t+phase[91])
cfoui<-fft(cfl$Portate)
#Indici relativi alle requenze individuate tramite il periodogramma
index<-c(4,6,17,25,40,91,33296-2,33296-4,33296-15,33296-23,33296-38,33296-89)
#index2<-c(4,6,17,22,25,28,30,38,40,45,53,74,91,33296-2,33296-4,33296-15,33296-20,33296-23,33296-26,33296-28,33296-36,33296-38,33296-43,33296-51,33296-72,33296-89)
#index3<-c(17,91,33296-15,33296-89)
#index4=c(4,5,6,7,9,10,22,30,45,91, 33296-2, 33296-3, 33296-4, 33296-5, 33296-7, 33296-8, 33296-20, 33296-28, 33296-43, 33296-89)
#Costruisco una srie con i valori delle sole frequenze individuate
cfou.freq<-cfoui*0
cfou.freq[index]<-cfoui[index]
#Ricostruisco il segnale tramite l'antitrasformata di fourier
cfreq<-fft(cfou.freq, inverse = TRUE)/length(cfoui)
#Plotto il grafico del segnale risultante
plot(Re(cfreq))

#Dopo ver individuati i periodi caratterizzanti le stagionalità della serie cerco di individuare i parametri per una 
#modellizazione tramite ARIMA con parametri esterni (le frequenze, e le loro armoniche, individuate) per la regressione.

#Costruisco due serie temporali(ts) con periodi diversi
cfl.ts1<-ts(cfl$Portate, freq=365.89011368571722328919844818685)
cfl.ts2<-ts(cfl$Portate, freq=1958.5883905064870406081965357272)

#1-Individuo i coefficienti di fourier per i diversi periodi (e per diversi ordini k(armoniche)) utilizzando fourier()
#2-auto.arima per la determinazione del modello ARIMA utilizzando come regressori esterni i valori di fourier derivati in precedenza
#3-Calcolo i valori dell'AIC e li plotto per determinare il modello con il valore più basso

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

#FORECAST