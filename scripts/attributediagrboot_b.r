attributediagrboot <- function(data1,data2,nbins=10,nboot=1000,maintitle="") {
# Plot attribute diagram 
#
# Usage: attributediagrboot(data1,data2, nbins, nboot, maintitle)
#
# Inputs:
#
# data1: Vector of forecast probabilities for the event of 
#        interest (e.g. precip. in the upper tercile)
#
# data2: Vector of binary observations for the event of
#        interest (e.g. precip. in the upper tercile)
#
#    nbins: number of probability bins 
#
#    nboot: number of bootstrap samples 
#
#    maintitle: String containing the text for the reliability diagram title
#
# Author: Caio Coelho <caio.coelho@cptec.inpe.br>

pf<-data1
probfcsts<-as.vector(pf)

aux<-probfcsts
probfcsts<-probfcsts[!is.na(aux)]

binobs<-data2 
binobs <- as.vector(binobs)
binobs <- binobs[!is.na(aux)]

aux1<-binobs
binobs <- binobs[!is.na(aux1)]
probfcsts <- probfcsts[!is.na(aux1)]
                                             
obar <- mean(binobs, na.rm=TRUE)
n<-length(probfcsts)                                            

h1<-hist(probfcsts,breaks=seq(0,1,1/nbins),plot=F)$counts        
#h1<-h1[h1>0]

g1<-hist(probfcsts[binobs==1],breaks=seq(0,1,1/nbins),plot=F)$counts
#g1<-g1[g1>0]

obari <- g1/h1                                                    
obari[is.na(obari)]<-0
  
# Computes reliability,resolution and uncertainty components of the 
# Brier score 
yi <- seq((1/nbins)/2,1,1/nbins)
  
reliab <- sum(h1*((yi-obari)^2), na.rm=TRUE)/n
resol <- sum(h1*((obari-obar)^2), na.rm=TRUE)/n
uncert<-obar*(1-obar)

bs<-reliab-resol+uncert

#bs1<-mean((probfcsts-binobs)^2)

par(mar=c(3.5,4,2,1),mgp=c(2.4,0.8,0), cex=1.2,las=1,lwd=2)
plot(yi*100,h1/n*100,xlim=c(0,1)*100,ylim=c(0,1)*100,type='h',main=paste("",(modl)," - Week ",as.character(round(ii,2)),sep="")
,xlab=
"Forecast Probability (%)",ylab="Observed or Forecast Relative Frequency (%)",col="blue",lwd=10, cex=1.5,font=2,cex.axis=1.2, cex.lab=1.2,cex.main=1.3)
#a linha da frequencia da probalidade  do yi
lines(yi*100,obari*100,lwd=2,lty=5,col="black")
#marcar os pontos sobre a linha 
#points(yi*100,obari*100,pch=1,col="black")
#linha diagonal 
abline(0,1)
#linha da climatologia do obar (observacao) na horizontal
abline(h=obar*100,col="black")
#linha da climatologia do obar (observacao) na vertical 
#abline(v=obar*100,col="black")
#abline(lsfit(yi*100,obari*100,wt=h1),lty=2) #nao descomentar essa linha 
#para verificar quais valores são pontos sao fortes ou fracos no conjunto de prev
#lines(seq(-10,110),(seq(-10,110)+(obar*100))/2,lty=3,lwd=3)
#escrever na tela os valores dos indices abaixo 
#text(20,100,paste("Rel: ",as.character(round(reliab,2)),sep=""))
#text(20,90,paste("Res: ",as.character(round(resol,2)),sep=""))
#text(20,80,paste("Unc: ",as.character(round(uncert,2)),sep=""))
#text(20,70,paste("BS: ",as.character(round(bs,2)),sep=""))

############################################################################
#Boot bootstrap resampling to estiamte sampling uncertainty in obs frequency
############################################################################

bk<-seq(0,1,1/nbins)

hboot<-matrix(NA,nrow=nboot,ncol=10)
gboot<-matrix(NA,nrow=nboot,ncol=10)
obariboot<-matrix(NA,nrow=nboot,ncol=10)

for (i in 1:nboot){
index<-sample(seq(1,length(probfcsts)),length(probfcsts),replace=T)
hboot[i,]<-hist(probfcsts[index],breaks=bk,plot=F)$counts        
pf<-probfcsts[index]
bo<-binobs[index]
gboot[i,]<-hist(pf[bo==1],breaks=bk,plot=F)$counts
obariboot[i,] <- gboot[i,]/hboot[i,]                                                    
#obari[is.na(obari)]<-0 #this line may be needed when using large number of bins
}
#yi <- seq((1/nbins)/2,1,1/nbins)

low<-apply(obariboot,2,function(x){quantile(x,0.025)})
upp<-apply(obariboot,2,function(x){quantile(x,0.975)})

########################################################################
#Plot estiamted sampling uncertainty
#on observed frequency (95% confidence interval, shown as grey vertical bar)
########################################################################
for (i in 1:10){
segments(yi[i]*100,low[i]*100,yi[i]*100,upp[i]*100,col="black",lwd=4)
}

}

