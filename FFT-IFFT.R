Caposele_fill_Date <- read.csv("C:/Users/Giovanni/PycharmProjects/RNNQ/Caposele_fill_Date.csv")
cfl<-Caposele_fill_Date
cfl$X<-as.Date(cfl$X)
cfou<-fft(cfl$Portate)
cfou <- cfou[2:((length(cfou) / 2) + 1)]
ampl <- Mod(cfou) / length(cfou)
freq <- (1:(33296/ 2))/33296
phase<-Arg(cfou)
plot(freq[0:100], ampl[0:100], type='l')
t<-(1:33296)
abline(v=1/(1*365), lty='dotted')
sig<-ampl[91]*cos(2*pi*freq[91]*t+phase[91])
cfoui<-fft(cfl$Portate)
index<-c(4,6,17,25,40,91,33296-2,33296-4,33296-15,33296-23,33296-38,33296-89)
#index2<-c(4,6,17,22,25,28,30,38,40,45,53,74,91,33296-2,33296-4,33296-15,33296-20,33296-23,33296-26,33296-28,33296-36,33296-38,33296-43,33296-51,33296-72,33296-89)
#index3<-c(17,91,33296-15,33296-89)
#index4=c(4,5,6,7,9,10,22,30,45,91, 33296-2, 33296-3, 33296-4, 33296-5, 33296-7, 33296-8, 33296-20, 33296-28, 33296-43, 33296-89)
cfou.freq<-cfoui*0
cfou.freq[index]<-cfoui[index]
cfreq<-fft(cfou.freq, inverse = TRUE)/length(cfoui)
plot(Re(cfreq))

cfou.per.tap = spectrum(cfl$Portate, spans=c(7,7), taper=0.1, log='no', plot=FALSE)
plot(cfou.per.tap$freq[0:100], cfou.per.tap$spec[0:100], type = 'l', lwd=2, cex.axis=1.5, xlab= 'frequency', ylab="spectrum")
k = kernel("hamming", 4)
cfou.per.smooth = spec.pgram(cfl$Portate, k, taper=0, log='no', plot=FALSE)
plot(cfou.per.smooth$freq[0:100], cfou.per.smooth$spec[0:100], type = 'l', lwd=2, cex.axis=1.5)
