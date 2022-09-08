library(verification)

#DIMENSOES DOS DADOS DE ENTRADA
ylat<-180
xlon<-360
ntim<-120

#LOOP REFERENTE AOS LEAD TIMES (WEEK1,WEE2,WEEK3 E WEEK4)
ii<-1
while(ii<=4){

#ABRE ARQUIVOS DAS PROBABILIDADES PREVISTAS (MODELO)
   arq<-file(paste("BAM12_WEEK0",(ii),".dat", sep = "", collapse = "; "),"rb")
   prob<-array(NA,c(xlon,ylat,ntim))
	   for (j in 1:ntim) {
	   aux1<-readBin(arq, double(), xlon*ylot, size=4,endian="little")
	   prob[,,j]<-matrix(aux1,ncol=ylat,nrow=xlon)}
	close(arq)

#ABRE ARQUIVOS DAS OBSERVACOES BINARIAS (1 P/ OCORRENCIA E 0 P/ NAO OCORRENCIA)
   arq<-file(paste("GPCP_WEEK0",(ii),".dat", sep = "", collapse = "; "),"rb")
   obs<-array(NA,c(xlon,ylat,ntim))
	   for (j in 1:ntim) {
	   aux2<-readBin(arq, double(), xlon*ylot, size=4,endian="little")
	   obs[,,j]<-matrix(aux2,ncol=ylat,nrow=xlon)}
	close(arq)

   prob[prob==-99999.0000]<-NA
   obs[obs==-99999]<-NA

#FUNCAO PARA CALCULAR E PLOTAR A CURVA ROC. 
   aroc<-roc.area(obs,prob)$A

   png(paste("AROC-BAM12-WEEK0",(ii),".png", sep = "", collapse = "; "))
   roc.plot(obs,prob,alpha = 0.05, tck = 0.01, CI = TRUE, n.boot = 1000, plot.thres = seq(0.1,.9, 0.1), show.thres = FALSE, main=paste((modl)," - Week ",as.character(round(ii,2)),sep=""),
   xlab ="False Alarm Rate",
   ylab="Hit Rate",cex=1.5,font=2,cex.axis=1.2,cex.lab=1.5,cex.main=1.3,lwd=2)
   text(0.7,0.15,paste("ROC Score =",as.character(round(aroc,2))),cex=1.5,font=2)
   dev.off()

   ii<-ii+1
}

