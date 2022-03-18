Totali_Medie_Pluriannue <- read.csv("~/Esercitazioni/R/Dottorato/Cluster Analysis/Totali_Medie_Pluriannue.csv", sep=";")
medie<-Totali_Medie_Pluriannue[,2:11]
pairs(medie)
medie.soil<- medie[,1:9]
medie.st<-scale(medie.soil)
distEucl<-dist(medie.st, method="euclidean")
clustCompl<-hclust(distEucl, method = "complete")
plot(clustCompl, main = "Analisi Gerarchica con Legame Completo")

clustComplk5<-cutree(clustCompl, h = 5.0908177)
rect.hclust(clustCompl, h = 5.0908177, border="red")

clustComplk5<-as.vector(clustComplk5)

for(i in 1:6){
  assign(paste("group", i, sep = ""), which(clustComplk5==i))    
}
group1
group2
group3
group4
group5
group6

plot(medie.st[,2], medie[,1], col=clustComplk5, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Completo")
legend('topleft', legend=c(1,2,3,4,5,6), col=unique(clustComplk5), pch=19,cex=1.5)
plot(medie.st[,3], medie[,1], col=clustComplk5, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Completo")
legend('topleft', legend=c(1,2,3,4,5,6), col=unique(clustComplk5), pch=19,cex=1.5)
plot(medie.st[,4], medie[,1], col=clustComplk5, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Completo")
legend('topleft', legend=c(1,2,3,4,5,6), col=unique(clustComplk5), pch=19,cex=1.5)
plot(medie.st[,5], medie[,1], col=clustComplk5, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Completo")
legend('topleft', legend=c(1,2,3,4,5,6), col=unique(clustComplk5), pch=19,cex=1.5)
plot(medie.st[,6], medie[,1], col=clustComplk5, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Completo")
legend('topleft', legend=c(1,2,3,4,5,6), col=unique(clustComplk5), pch=19,cex=1.5)
plot(medie.st[,7], medie[,1], col=clustComplk5, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Completo")
legend('topleft', legend=c(1,2,3,4,5,6), col=unique(clustComplk5), pch=19,cex=1.5)
plot(medie.st[,8], medie[,1], col=clustComplk5, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Completo")
legend('topleft', legend=c(1,2,3,4,5,6), col=unique(clustComplk5), pch=19,cex=1.5)
plot(medie.st[,9], medie[,1], col=clustComplk5, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Completo")
legend('topleft', legend=c(1,2,3,4,5,6), col=unique(clustComplk5), pch=19,cex=1.5)

clustAv<-hclust(distEucl, method = "ave")
plot(clustAv, main = "Analisi Gerarchica con Legame Medio")
clustAvk8<-cutree(clustAv, h = 3.4593161)
rect.hclust(clustAv, h = 3.4593161, border="red")
clustAvk8<-as.vector(clustAvk8)

for(i in 1:8){
  assign(paste("groupAv", i, sep = ""), which(clustAvk8==i))    
}
groupAv1
groupAv2
groupAv3
groupAv4
groupAv5
groupAv6
groupAv7
groupAv8

plot(medie.st[,2], medie.st[,1], col=clustAvk8, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Medio")
legend('topleft', legend=c(1,2,3,4,5,6,7,8), col=unique(clustAvk8), pch=19,cex=1.5)
plot(medie.st[,3], medie.st[,1], col=clustAvk8, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Medio") 
legend('topleft', legend=c(1,2,3,4,5,6,7,8), col=unique(clustAvk8), pch=19,cex=1.5)
plot(medie.st[,4], medie.st[,1], col=clustAvk8, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Medio") 
legend('topleft', legend=c(1,2,3,4,5,6,7,8), col=unique(clustAvk8), pch=19,cex=1.5)
plot(medie.st[,5], medie.st[,1], col=clustAvk8, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Medio") 
legend('topleft', legend=c(1,2,3,4,5,6,7,8), col=unique(clustAvk8), pch=19,cex=1.5)
plot(medie.st[,6], medie.st[,1], col=clustAvk8, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Medio") 
legend('topleft', legend=c(1,2,3,4,5,6,7,8), col=unique(clustAvk8), pch=19,cex=1.5)
plot(medie.st[,7], medie.st[,1], col=clustAvk8, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Medio") 
legend('topleft', legend=c(1,2,3,4,5,6,7,8), col=unique(clustAvk8), pch=19,cex=1.5)
plot(medie.st[,8], medie.st[,1], col=clustAvk8, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Medio") 
legend('topleft', legend=c(1,2,3,4,5,6,7,8), col=unique(clustAvk8), pch=19,cex=1.5)
plot(medie.st[,9], medie.st[,1], col=clustAvk8, pch=19,cex=1.5,main = "Analisi Gerarchica con Legame Medio") 
legend('topleft', legend=c(1,2,3,4,5,6,7,8), col=unique(clustAvk8), pch=19,cex=1.5)
