source("attributediagrboot_b.r")

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

#NOME DA FIGURA DO DIAGRAMA DE CONFIABILIDADE
   png(paste("RELIABILITY-BAM12-WEEK0",(ii),".png", sep = "", collapse = "; "))

#FUNCAO PARA PLOTAR O DIAGRAMA
   attributediagrboot(prob,obs)

   dev.off()

   ii<-ii+1
}

