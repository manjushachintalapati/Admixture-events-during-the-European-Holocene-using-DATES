# Figure3 supplementary figures

# Figure3- Supplement1 
#decay curves for samples studied in European Holocene
#NRMSD function
rm(list=ls());
NRMSD <- function(y, yfit,na.rm = TRUE) {
  # y is the vector of empirical values
  # yfit is the vector of fitted values
  if (length(y) != length(yfit)) stop('y and yfit should have the same length')
  if (na.rm) {
    isna = is.na(y) | is.na(yfit)
    y = y[!isna]
    yfit = yfit[!isna]
  }
  nrmsd = sqrt(mean( (yfit - y)**2, na.rm = na.rm )) * (max(yfit, na.rm = na.rm) - min(yfit, na.rm = na.rm))**(-1)
  return(nrmsd)
}
#Usage: nmrds=round(NRMSD(data$V2,data$V3),4)
admix_dates=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement1_admixture_dates")
head(admix_dates)
pdf(file="~/Desktop/DATES_figures/results/Figure3_supplement1.pdf",height=(6*3), width =(6*2))
par(mfrow=c(6,4),oma=c(0,0,3.5,0))
layout(matrix(seq(1,24,1), nrow = 6), heights=c(1,1))
# HG decay curves
dates_files=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement1_HG_curves")
for (i in 1:nrow(dates_files)) 
{
  i=1
  file=dates_files$V1[i]
  path="~/Desktop/DATES_figures/data/Figure3_supplement1_decay_files/" ; var=paste(path,file,sep = "/")
  mm=unlist(strsplit(as.character(file),'/',fixed=TRUE))[1]
  mm1=unlist(strsplit(as.character(mm),'.',fixed=TRUE))[1]
  data=read.table(file = var)
  jout=gsub("fit", "jout", file); time=paste(path,jout,sep = "/")
  estimate=read.table(file = time)
  name=gsub("estimate_","",gsub(".fit", "", file))
  len=paste("Estimate:",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "+/-"),sep = " ")
  title_name=gsub("EHG_WHG_","",mm1)
  main_t=paste("Target:",title_name,"\n","References:EHG-WHG")
  nmrsd=round(NRMSD(data$V2,data$V3),4)
  if(nmrsd>0.7) {kol="grey20"} else {kol="deepskyblue"};
  plot(x=data$V1,y=data$V2,type="p",col=kol,pch="*",xlim = c(0,20),xlab = "Genetic Distance(cM)",
       ylab = "Ancestry covariance",las=1,main = main_t,cex=1)
  lines(x=data$V1,y=data$V3,type = "l",col=kol,lty=2)
  bce=admix_dates[admix_dates$V1==title_name,]
  len=paste(paste(paste("DATES estimate (gen)",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "±"),sep = ":"),
                  paste("DATES estimate (BCE)",paste(bce$V9,bce$V10,sep=" ± "),sep=":"),sep = "\n"),
            paste("NRMSD",nmrsd,sep="="),sep = "\n")
  legend("topright",legend = len,col = kol,bty='n',cex=0.7)
}
### Farmer formation 
data=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement1_decay_files/AAF.fit",header = F)
jout=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement1_decay_files/AAF.jout",header = F)

plot(x=data$V1,y=data$V2,type="p",col="darkorchid",pch="*",xlim = c(0,20),xlab = "Genetic Distance(cM)",
     las=1,main = "Early Anatolian farmers",cex=1,ylab ="")
title(ylab = "Ancestry covariance", mgp = c(4.2, 1, 0)) 
lines(x=data$V1,y=data$V3,type = "l",col="darkorchid",lty=2)
nmrsd=round(NRMSD(data$V2,data$V3),4)
len=paste(paste(paste("DATES estimate (gen)",paste(round(jout$V2,0),round(jout$V5,0),sep = " ± "),sep = ": "),
                paste("DATES estimate (BCE)",paste(round(jout$V2,0)*28+8071,round(jout$V5,0)*28,sep = " ± "),sep = ": "),sep = "\n"),
          paste("NMRSD",nmrsd,sep="="),sep = "\n")
legend("topright",legend = len,col = "darkorchid",lty=c(2,-1,-1),cex=0.7,bty='n')

dates_files=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement1_FF_curves")
mt=c("Iran_N-Anatolian_N","Iran_N-Anatolian_N")
for (i in 1:nrow(dates_files)) 
{
  file=dates_files$V1[i]
  path="~/Desktop/DATES_figures/data/Figure3_supplement1_decay_files/" ; var=paste(path,file,sep = "/")
  mm=unlist(strsplit(as.character(file),'/',fixed=TRUE))[1]
  mm1=unlist(strsplit(as.character(mm),'-',fixed=TRUE))[1]
  data=read.table(file = var)
  jout=gsub("fit", "jout", file); time=paste(path,jout,sep = "/")
  estimate=read.table(file = time)
  name=gsub("estimate_","",gsub(".fit", "", file))
  title_name=gsub("published_merged","Anatolian Farmer",gsub("Anatolia_N_Serbia_Iron_Gates_HG_","",mm1))
  main_t=paste("Target:",title_name,"\n",mt[i])
  len=paste("Estimate:",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "+/-"),sep = " ")
  nmrsd=round(NRMSD(data$V2,data$V3),4)
  if(nmrsd>0.7) {kol="grey20"} else {kol="firebrick"};
  plot(x=data$V1,y=data$V2,type="p",col=kol,pch="*",xlim = c(0,20),xlab = "Genetic Distance(cM)",
       ylab = "Ancestry covariance",las=1,main =main_t ,cex=1)
  lines(x=data$V1,y=data$V3,type = "l",col=kol,lty=1)
  bce=admix_dates[admix_dates$V1==title_name,]
  len=paste(paste(paste("DATES estimate (gen)",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "±"),sep = ":"),
                  paste("DATES estimate (BCE)",paste(bce$V9,bce$V10,sep=" ± "),sep=":"),sep = "\n"),
            paste("NRMSD",nmrsd,sep="="),sep = "\n")
  legend("topright",legend = len,col = kol,bty='n',cex=0.7)
}
### Farmer spread
dates_files=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement1_Neolithic_curves")
for (i in 1:nrow(dates_files)) 
{
  file=dates_files$V1[i]
  path="~/Desktop/DATES_figures/data/Figure3_supplement1_decay_files/" ; var=paste(path,file,sep = "/")
  mm=unlist(strsplit(as.character(file),'/',fixed=TRUE))[1]
  mm1=unlist(strsplit(as.character(mm),'-',fixed=TRUE))[1]
  refm=paste(unlist(strsplit(as.character(mm),'-',fixed=TRUE))[2],"-",unlist(strsplit(as.character(mm),'-',fixed=TRUE))[3])
  data=read.table(file = var)
  jout=gsub("fit", "jout", file); time=paste(path,jout,sep = "/")
  estimate=read.table(file = time)
  name=gsub("estimate_","",gsub(".fit", "", file))
  main_t=paste("Target:",mm1,"\n","References:",gsub("Turkey_N","AnatolianN",refm))
  len=paste("Estimate:",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "+/-"),sep = " ")
  nmrsd=round(NRMSD(data$V2,data$V3),4)
  if(nmrsd>0.7) {kol="grey20"} else {kol="orange2"};
  plot(x=data$V1,y=data$V2,type="p",col=kol,pch="*",xlim = c(0,20),xlab = "Genetic Distance(cM)",
       ylab = "Ancestry covariance",las=1,main = main_t,cex=1)
  lines(x=data$V1,y=data$V3,type = "l",col=kol,lty=1)
  bce=admix_dates[admix_dates$V1==mm1,]
  len=paste(paste(paste("DATES estimate (gen)",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "±"),sep = ":"),
                  paste("DATES estimate (BCE)",paste(bce$V9,bce$V10,sep=" ± "),sep=":"),sep = "\n"),
            paste("NRMSD",nmrsd,sep="="),sep = "\n")
  legend("topright",legend = len,col = kol,bty='n',cex=0.7)
}

### Steppe formation- EBA
dates_files=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement1_Steppe_formation")
for (i in 1:nrow(dates_files)) 
{
  #  i=5
  file=dates_files$V1[i]
  path="~/Desktop/DATES_figures/data/Figure3_supplement1_decay_files/" ; var=paste(path,file,sep = "/")
  mm=unlist(strsplit(as.character(file),'/',fixed=TRUE))[1]
  mm1=unlist(strsplit(as.character(mm),'-',fixed=TRUE))[1]
  data=read.table(file = var)
  jout=gsub("fit", "jout", file); time=paste(path,jout,sep = "/")
  estimate=read.table(file = time)
  name=gsub("estimate_","",gsub(".fit", "", file))
  main_t=gsub("_pub","",paste("Target:",mm1,"\n","References:Iran_N_pooled-EHG_pooled"))
  len=paste("Estimate:",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "+/-"),sep = " ")
  nmrsd=round(NRMSD(data$V2,data$V3),4)
  if(nmrsd>0.7) {kol="grey20"} else {kol="deeppink"};
  plot(x=data$V1,y=data$V2,type="p",col=kol,pch="*",xlim = c(0,20),xlab = "Genetic Distance(cM)",
       ylab = "Ancestry covariance",las=1,main = main_t,cex=1.2)
  lines(x=data$V1,y=data$V3,type = "l",col=kol,lty=1)
  bce=admix_dates[admix_dates$V1==mm1,]
  len=paste(paste(paste("DATES estimate (gen)",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "±"),sep = ":"),
                  paste("DATES estimate (BCE)",paste(bce$V9,bce$V10,sep=" ± "),sep=":"),sep = "\n"),
            paste("NRMSD",nmrsd,sep="="),sep = "\n")
  legend("topright",legend = len,col = kol,bty='n',cex=0.7)
}
### Steppe formation - MLBA
dates_files=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement1_Steppe_MLBA")
for (i in 1:nrow(dates_files)) 
{
  #  i=4
  file=dates_files$V1[i]
  path="/~/Desktop/DATES_figures/data/Figure3_supplement1_decay_files/" ; var=paste(path,file,sep = "/")
  mm=unlist(strsplit(as.character(file),'/',fixed=TRUE))[1]
  mm1=unlist(strsplit(as.character(mm),'-',fixed=TRUE))[1]
  data=read.table(file = var)
  jout=gsub("fit", "jout", file); time=paste(path,jout,sep = "/")
  estimate=read.table(file = time)
  name=gsub("estimate_","",gsub(".fit", "", file))
  main_t=paste("Target:",mm1,"\n","References:Steppe EBA-Neolithic groups")
  len=paste("Estimate:",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "+/-"),sep = " ")
  nmrsd=round(NRMSD(data$V2,data$V3),4)
  if(nmrsd>0.7) {kol="grey20"} else {kol="lightpink"};
  plot(x=data$V1,y=data$V2,type="p",col=kol,pch="*",xlim = c(0,20),xlab = "Genetic Distance(cM)",
       ylab = "Ancestry covariance",las=1,main = main_t,cex=1)
  lines(x=data$V1,y=data$V3,type = "l",col=kol,lty=1)
  bce=admix_dates[admix_dates$V1==mm1,]
  len=paste(paste(paste("DATES estimate (gen)",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "±"),sep = ":"),
                  paste("DATES estimate (BCE)",paste(bce$V9,bce$V10,sep=" ± "),sep=":"),sep = "\n"),
            paste("NRMSD",nmrsd,sep="="),sep = "\n")
  legend("topright",legend = len,col = kol,bty='n',cex=0.7)
}
### Steppe spread
dates_files=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement1_Steppe_spread")
for (i in 1:nrow(dates_files)) 
{
  file=dates_files$V1[i]
  path="~/Desktop/DATES_figures/data/Figure3_supplement1_decay_files/" ; var=paste(path,file,sep = "/")
  mm=unlist(strsplit(as.character(file),'/',fixed=TRUE))[1]
  mm1=unlist(strsplit(as.character(mm),'-',fixed=TRUE))[1]
  data=read.table(file = var)
  jout=gsub("fit", "jout", file); time=paste(path,jout,sep = "/")
  estimate=read.table(file = time)
  name=gsub("estimate_","",gsub(".fit", "", file))
  title_name=gsub("Afanasievo_Anatolia_N_","",mm1)
  main_t=paste("Target:",title_name,"\n","References:Steppe groups-(WHG+AnatoliaN)")
  len=paste("Estimate:",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "+/-"),sep = " ")
  nmrsd=round(NRMSD(data$V2,data$V3),4)
  if(nmrsd>0.7) {kol="grey20"} else {kol="green3"};
  plot(x=data$V1,y=data$V2,type="p",col=kol,pch="*",xlim = c(0,20),xlab = "Genetic Distance(cM)",
       ylab = "Ancestry covariance",las=1,main = main_t,cex=1)
  lines(x=data$V1,y=data$V3,type = "l",col=kol,lty=1)
  bce=admix_dates[admix_dates$V1==title_name,]
  len=paste(paste(paste("DATES estimate (gen)",paste(round(estimate$V2,0),round(estimate$V5,0),sep = "±"),sep = ":"),
                  paste("DATES estimate (BCE)",paste(bce$V9,bce$V10,sep=" ± "),sep=":"),sep = "\n"),
            paste("NRMSD",nmrsd,sep="="),sep = "\n")
  legend("topright",legend = len,col = kol,bty='n',cex=0.7)
}
dev.off()

# Figure3- supplement2
# Admixture dates in Iron Gates samples
rm(list=ls());
jpeg(file ="~/Desktop/DATES_figures/results/Figure3_supplement2.jpeg",width = 9,height = 6,units="in",res =1000)
dd=read.table(file = "~/Desktop/DATES_figures/data/Figure3_supplement2")
str(dd)
plot(seq(1,NROW(dd$V1),1),dd$V8,col=as.character(dd$V10),pch=15, 
     ylim =c(3000,19000),las=1, xaxt='n',xlab="",ylab="Admixture time in years BCE",
     main = "Admixture dates in Iron_Gates samples grouped by c14 age in bins of 500 years"); grid()
segments(x0 =seq(1,NROW(dd$V1),1),x1 = seq(1,NROW(dd$V1),1),y0 =(dd$V8+2*dd$V9),
         y1 = (dd$V8-2*dd$V9),col=as.character(dd$V10),lty = 1)
segments(x0 =seq(1,NROW(dd$V1),1)-0.1,x1 = seq(1,NROW(dd$V1),1)+0.1,y0 =(dd$V8+2*dd$V9),
         y1 = (dd$V8+2*dd$V9),col=as.character(dd$V10))
segments(x0 =seq(1,NROW(dd$V1),1)-0.1,x1 = seq(1,NROW(dd$V1),1)+0.1,y0 =(dd$V8-2*dd$V9),
         y1 = (dd$V8-2*dd$V9),col=as.character(dd$V10))
points(seq(1,NROW(data$V1),1),data$V7)
segments(x0 =seq(1,NROW(dd$V1),1),x1 = seq(1,NROW(dd$V1),1),y0 =(dd$V2-1950),
         y1 = (dd$V3-1950))
segments(x0 =seq(1,NROW(dd$V1),1)-0.1,x1 = seq(1,NROW(dd$V1),1)+0.1,y0 =(dd$V2-1950),
         y1 = (dd$V2-1950))
segments(x0 =seq(1,NROW(dd$V1),1)-0.1,x1 = seq(1,NROW(dd$V1),1)+0.1,y0 =(dd$V3-1950),
         y1 = (dd$V3-1950))
axis(1, 1:NROW(data$V1), lty = 1,col = "black",tck="y",labels = rep('', NROW(data$V1)))
text(1:NROW(data$V1), rep(2500, NROW(data$V4)),
     labels= gsub("IronGates-","",data$V1), col="black", srt=25, xpd=TRUE, adj=1,cex=1)
legend("topright",legend = c("WHG-EHG admixture per c14 bin","WHG-EHG admixture in all samples","average c14 ages"),
       col = c("blue","cyan2","grey40"),lty=c(1,1,1),pch=c(15,15,1))
dev.off()
