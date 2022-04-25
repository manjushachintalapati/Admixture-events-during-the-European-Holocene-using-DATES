#Figure2. Formation of ancestral groups- Anatolian farmers and Steppe Yamanaya

# NMRSD function #Usage: nmrds=round(NRMSD(data$V2,data$V3),4)
rm(list=ls());
require(ggplot2)
require(maps)
require(ggrepel)

library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

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
# Maps 
jpeg(file ="~/Desktop/DATES_figures/data/Figure2_map.jpeg",width = 12,height =6,units="in",res =1000)
dd=read.table(file = "~/Desktop/DATES_figures/data/Figure2_Data_map",header = T)
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(world) + geom_sf() + coord_sf(xlim = c(-25,120), ylim = c(35,80), expand = FALSE) + 
  geom_point(data = dd, aes(x=long,y=lat),col=dd$col,inherit.aes = FALSE,pch=18,cex=4,show.legend = F) +
  geom_text_repel(data=dd,aes(x=long+10,y=lat),label=dd$sample,cex=4)+
  theme_bw()
dev.off()
# decay curves
png(file ="~/Desktop/DATES_figures/results/Figure2_decay_curves.png"
    ,width = 12,height = 6,units = "in",res = 500)
par(mai = c(1,1.1,0.5,0.5), mfrow=c(1,2))
data=read.table(file = "~/Desktop/DATES_figures/data/Figure2_Data_AnatoliaFarmer.fit",header = F)
jout=read.table(file = "~/Desktop/DATES_figures/data/Figure2_Data_AnatoliaFarmer.jout",header = F)
plot(x=data$V1,y=data$V2,type="p",col="darkorchid",pch="*",xlim = c(0,20),xlab = "Genetic Distance(cM)",
     las=1,main = "Early Anatolian farmers",cex=2,cex.main=1.5,cex.axis=1.2,cex.lab=1.2,
     ylab ="")
title(ylab = "Ancestry covariance", mgp = c(4.2, 1, 0)) 
lines(x=data$V1,y=data$V3,type = "l",col="darkorchid",lty=2)
nmrsd=round(NRMSD(data$V2,data$V3),4)
len=paste(paste(paste("DATES estimate (gen)",paste(round(jout$V2,0),round(jout$V5,0),sep = " ± "),sep = ": "),
                paste("DATES estimate (BCE)",paste(round(jout$V2,0)*28+8071,round(jout$V5,0)*28,sep = " ± "),sep = ": "),sep = "\n"),
          paste("NMRSD",nmrsd,sep="="),sep = "\n")
legend("topright",legend = len,col = "darkorchid",lty=c(2,-1,-1), cex=1,bty='n')

data=read.table(file = "~/Desktop/DATES_figures/data/Figure2_Data_SteppeFarmer.fit",header = F)
jout=read.table(file = "~/Desktop/DATES_figures/data/Figure2_Data_SteppeFarmer.jout",header = F)
plot(x=data$V1,y=data$V2,type="p",col="deeppink",pch="*",xlim = c(0,20),xlab = "Genetic Distance(cM)",
     las=1,main = "Early Steppe Pastoralists",cex=2,cex.main=1.5,cex.axis=1.2,cex.lab=1.2,ylab="")
title(ylab = "Ancestry covariance", mgp = c(4.2, 1, 0)) 
lines(x=data$V1,y=data$V3,type = "l",col="deeppink",lty=2)
nmrsd=round(NRMSD(data$V2,data$V3),4)
len=paste(paste(paste("DATES estimate (gen)",paste(round(jout$V2,0),round(jout$V5,0),sep = " ± "),sep = ": "),
                paste("DATES estimate (BCE)",paste(round(jout$V2,0)*28+2881,round(jout$V5,0)*28,sep = " ± "),sep = ": "),sep = "\n"),
          paste("NMRSD",nmrsd,sep="="),sep = "\n")
legend("topright",legend = len,col = "deeppink",lty=c(2,-1,-1), cex=1,bty='n')

dev.off()






