source("attributediagrboot_b.r")
source("roc.area.eurobrisa.r")
library(verification)

monthl<-list("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
meml<-list("EIT", "01N", "02N", "03N", "04N", "05N", "01P", "02P", "03P", "04P", "05P")
prol<-list("WEEK0", "FORT0", "3WKS0", "MNTH0")
n<-80
ylat<-180
xlon<-360

#### LOOP PARA VARIAR OS PRODUTOS AVALIADOS ####
incrproi<-4
incrprof<-4
while(incrproi<=incrprof){

   if(incrproi==1){
      pro<-"week"
      ltf=4
   }
   if(incrproi==2){
      pro<-"fort"
      ltf=2
   }
   if(incrproi==3){
      pro<-"3wks"
      ltf=1
   }
   if(incrproi==4){
      pro<-"mnth"
      ltf=1
   }

   print (pro)

#### LOOP PARA VARIAR OS MESES AVALIADOS ####
   monthi<-6
   monthf<-6
   while(monthi<=monthf){
   
#### LOOP PARA VARIAR OS LEAD TIMES AVALIADOS ####
      lti<-1
      while(lti<=ltf){

         modb<-array(0,c(xlon,ylat,n))

#### LOOP PARA ABRIR TODOS OS MEMBROS DO ENSEMBLE ####
         memi<-1
         while(memi<=11){

            print(meml[memi])

##### ABRE AS ANOMALIAS DE PRECIPITAÇÃO DE CADA MEMBRO DO BAM-1.2 #####
            moda<-array(NA,c(xlon,ylat,n))
            arq<-file(paste("",(monthl[monthi]),"/BAM12_ANOMALY_",(meml[memi]),"_",(monthl[monthi]),"_",(prol[incrproi]),"",(lti),".bin", sep = "", collapse = "; "),"rb")
            for(ii in 1:n){
               aux<-readBin(arq, double(), xlon*ylat, size=4,endian="little")
               moda[,,ii]<-matrix(aux,ncol=ylat,nrow=xlon)}
            close(arq)

##### GERA O ACUMULADO DOS BINs DE CADA MEMBRO #####
            moda[moda==-99999.0000]<-NA
            moda[moda>=0]<-1
            moda[moda<=0]<-0

            modb<-modb+moda
 
         memi<-memi+1
         }

##### GERA AS PROBABILIDADES #####
         prob<-modb/11

##### ABRE AS ANOMALIAS DE PRECIPITAÇÃO DO GPCP #####
         bin<-array(NA,c(xlon,ylat,n))
         arq<-file(paste("",(monthl[monthi]),"/GPCP_ANOMALY_",(monthl[monthi]),"_",(prol[incrproi]),"",(lti),".bin", sep = "", collapse = "; "),"rb")
         for(ii in 1:n){
            aux<-readBin(arq, double(), xlon*ylat, size=4,endian="little") 
            bin[,,ii]<-matrix(aux,ncol=ylat,nrow=xlon)}
         close(arq)

##### GERA OS BINs DA GPCP #####
         bin[bin==-99999]<-NA
         bin[bin>=0]<-1
         bin[bin<=0]<-0

##### CÁLCULO DO DIAGRAMA DE CONFIABILIDADE (NECESSITA DO attributediagrboot_b.r) #####
      print("RELIABILITY DIAGRAM")
 	   png(paste("reliability_diagram_BAM-1.2_",(monthl[monthi]),"_",(prol[incrproi]),"",(lti),".png", sep = "", collapse = "; "))
      attributediagrboot(prob,bin)
	   dev.off()

##### CÁLCULO DA CURVA ROC (NECESSITA DO BIBLIOTECA VERIFICATION)#####
      print("AROC")
      aroc<-roc.area(bin,prob)$A
      png(paste("CURVA_ROC_BAM-1.2_",(monthl[monthi]),"_",(prol[incrproi]),"",(lti),".png", sep = "", collapse = "; "))
      roc.plot(bin,prob,alpha = 0.05, tck = 0.01, CI = TRUE, n.boot = 1000, plot.thres = seq(0.1,.9, 0.1), show.thres = FALSE, main=paste((prol[incrproi]),"",as.character(round(lti,2)),sep=""),
      xlab ="False Alarm Rate",
      ylab="Hit Rate",cex=1.5,font=2,cex.axis=1.2,cex.lab=1.5,cex.main=1.3,lwd=2)
      text(0.7,0.15,paste("ROC Score =",as.character(round(aroc,2))),cex=1.5,font=2)
      dev.off()

##### CÁLCULO A ÁREA SUB A CURVA ROC PARA CADA PONTO DE GRADE (NECESSITA DO roc.area.eurobrisa.r) #####
      print("MAPA ROC")
      maparoc<-matrix(NA,ncol=ylat,nrow=xlon)
      for (ilon in 1:xlon) {
         for (ilat in 1:ylat) {
            maparoc[ilon,ilat]<-roc.area.eurobrisa(bin[ilon,ilat,],prob[ilon,ilat,])$A
         }
      } 

##### SALVA O MAPA GERADO COM OS PONTOS DE GRADE DA ÁREA SUB A CURVA ROC roc.area.eurobrisa.r #####
      maparoc<-as.vector(maparoc)
      aux2=file(paste("AROC_BAM-1.2_",(monthl[monthi]),"_",(prol[incrproi]),"",(lti),".bin", sep = "", collapse = "; "),"wb")
      writeBin(maparoc, aux2, size = 4, endian="little")
      close(aux2)

         lti<-lti+1
      }

      monthi<-monthi+1
   }

   incrproi<-incrproi+1
}

