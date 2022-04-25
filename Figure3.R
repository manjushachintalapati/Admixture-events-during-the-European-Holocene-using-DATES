# Figure3. Admixture time in Ancient Europe- HG mixture, Neolithization and Pastoralits spread
rm(list=ls());
require(ggplot2)
require(maps)
require(ggrepel)

library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

data=read.table(file = "data/Figure3_data_map",header = T)
head(data)
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
# Hunter Gatherer distribution panel
jpeg(file ="results/Figure3_map_HG_distribution.jpeg",width = 4,height =4,units="in",res =1000)
dd=data[data$col=="deepskyblue",]
ggplot(Europe) + geom_sf() + coord_sf(xlim = c(-15,40), ylim = c(35,70), expand = FALSE) + 
  geom_point(data = dd, aes(x=long,y=lat),col=dd$col,inherit.aes = FALSE,pch=dd$pc,cex=4,show.legend = F) +
  scale_fill_manual(values ="deepskyblue") +
  theme(plot.title = element_text(colour = "black"))
dev.off()
#Neolithic Farmer spread
jpeg(file ="results/Figure3_map_ANF_distribution.jpeg",width =4,height = 4,units="in",res =1000)
dd=data[data$col=="orange2",]
ggplot(Europe) + geom_sf() + coord_sf(xlim = c(-15,40), ylim = c(35,70), expand = FALSE) + 
  geom_point(data = dd, aes(x=long,y=lat),col=dd$col,inherit.aes = FALSE,pch=dd$pc,cex=4,show.legend = F) + 
  theme(plot.title = element_text(colour = "black"))
dev.off()
# MLBA bronze age samples
jpeg(file ="results/Figure3_map_SP_distribution.jpeg",width = 5,height = 4,units="in",res =1000)
dd=rbind(data[data$col=="green3",],data[data$col=="lightpink2",])
ggplot(world) + geom_sf() + coord_sf(xlim = c(-15,90), ylim = c(30,80), expand = FALSE) + 
  geom_point(data = dd, aes(x=long,y=lat),col=dd$col,inherit.aes = FALSE,pch=dd$pc,cex=3,show.legend = F) + 
  theme(plot.title = element_text(colour = "black"))
dev.off()

# Admixture time plots 
jpeg(file ="results/Figure3_map_HG_AdmixtureDates.jpeg",width = 16,height = 7,units="in",res =1000)
par(mai = c(1,1,0.1,3))
data=read.table(file = "data/Figure3_data_admixturetimes")
fossil_range=read.table(file = "data/Figure3_data_fossilages",header = F)
# Hunter Gatherer mixture panel
dd=rbind(data[data$V8=="deepskyblue",])
head(dd)
max(dd$V2); min(dd$V2)
target_fossil=fossil_range[fossil_range$V2%in%as.character(dd$V1),]
group_names=unique(target_fossil$V2)
head(target_fossil)
target_fossil$V2 <- as.character(target_fossil$V2)
target_fossil$V6 <- as.numeric(as.character(target_fossil$V3))-1950
for(i in group_names)
{
  k=which(dd$V1==i)
  dd[k,9]=min(as.numeric(target_fossil[target_fossil$V2==i,]$V6))
  dd[k,10]=max((as.numeric(target_fossil[target_fossil$V2==i,]$V6)))
  dd[k,11]=mean(as.numeric(target_fossil[target_fossil$V2==i,]$V6))
}
head(dd)
min(dd$V9)
plot(x=seq(1,length(dd$V1),1),y=dd$V6,type = "p",
     col=as.character(dd$V8),pch=20,cex=2, ylim = c(4000,12200), xlim = c(0.5,length(dd$V1)+0.5),
     xaxt='n',yaxt='n',xlab = "",ylab = "Estimated admixture time (BCE)", main="");
grid(nx=8,ny=10);
segments(x0 =seq(1,length(dd$V1),1),x1 = seq(1,length(dd$V1),1),y0 =(dd$V6+dd$V7),
         y1 = (dd$V6-dd$V7),col=as.character(dd$V8),lty = 1)
segments(x0 =seq(1,length(dd$V1),1)-0.1,x1 = seq(1,length(dd$V1),1)+0.1,y0 =(dd$V6+dd$V7),
         y1 = (dd$V6+dd$V7),col=as.character(dd$V8))
segments(x0 =seq(1,length(dd$V1),1)-0.1,x1 = seq(1,length(dd$V1),1)+0.1,y0 =(dd$V6-dd$V7),
         y1 = (dd$V6-dd$V7),col=as.character(dd$V8))
axis(1, 1:length(dd$V1), lty = 1,col = "black",tck="y",labels = rep('', length(dd$V1)))
name_lab=gsub(".SG","",dd$V1)
points(x=seq(1,length(dd$V1),1),y=dd$V11,type = "p",col="grey20",pch=1,cex=1.1)
text(1:length(dd$V4), rep(3500, length(dd$V4)),
     labels= name_lab, col="black", srt=25, xpd=TRUE, adj=1,cex=1)
axis(2, seq(4000,12000,1000), lty = 1,col = "black",tck="y",labels = rep('',length(seq(4000,12000,1000))))
text(rep(-0.2,max(4000,12000,500)), seq(4000,12000,1000),labels= seq(4000,12000,1000),
     col="grey30", srt=0, xpd=TRUE, adj=-0.1,cex=0.8)
abline(v=3.5,lty=3,col="grey20")
abline(v=6.5,lty=3,col="grey20")
text(c(1,4,7),rep(12000,5), labels= c("Scandinavian HG","Baltic region HG","Central European HG"),
     col="grey30", srt=0, xpd=TRUE, adj=-0.1,cex=1)
legend(8.9,12000,legend = c("DATES in BCE ± 1SE","Average C14 radiocarbon age"),col=c("grey20","grey20"), 
       pch=c(20,1),lty=c(1,-1),xpd=T,bty='n')
dev.off()

# Neolithic Farmer mixture
jpeg(file ="results/Figure3_map_ANF_AdmixtureDates.jpeg",width = 16,height = 7,units="in",res =1000)
par(mai = c(1.4,1,0.1,0.5))
dd=data[data$V8=="orange2",]
target_fossil=fossil_range[fossil_range$V2%in%as.character(dd$V1),]
group_names=unique(target_fossil$V2)
head(target_fossil)
target_fossil$V2 <- as.character(target_fossil$V2)
target_fossil$V6 <- as.numeric(as.character(target_fossil$V3))-1950
for(i in group_names)
{
  k=which(dd$V1==i)
  dd[k,9]=min(as.numeric(target_fossil[target_fossil$V2==i,]$V6))
  dd[k,10]=max((as.numeric(target_fossil[target_fossil$V2==i,]$V6)))
  dd[k,11]=mean(as.numeric(target_fossil[target_fossil$V2==i,]$V6))
}
max(dd$V6+dd$V7)
plot(x=seq(1,length(dd$V1),1),y=dd$V6,type = "p",
     col=as.character(dd$V8),pch=20,cex=2, ylim = c(2000,6800), xlim = c(1,length(dd$V1)+0.5),
     xaxt='n',yaxt='n',xlab = "",ylab = "Estimated admixture time (BCE)", main="");
grid(nx=8,ny=10);
segments(x0 =seq(1,length(dd$V1),1),x1 = seq(1,length(dd$V1),1),y0 =(dd$V6+dd$V7),
         y1 = (dd$V6-dd$V7),col=as.character(dd$V8),lty = 1)
segments(x0 =seq(1,length(dd$V1),1)-0.1,x1 = seq(1,length(dd$V1),1)+0.1,y0 =(dd$V6+dd$V7),
         y1 = (dd$V6+dd$V7),col=as.character(dd$V8))
segments(x0 =seq(1,length(dd$V1),1)-0.1,x1 = seq(1,length(dd$V1),1)+0.1,y0 =(dd$V6-dd$V7),
         y1 = (dd$V6-dd$V7),col=as.character(dd$V8))
axis(1, 1:length(dd$V1), lty = 1,col = "black",tck="y",labels = rep('', length(dd$V1)))
points(x=seq(1,length(dd$V1),1),y=dd$V11,type = "p",col="grey20",pch=1,cex=1)
name_lab=gsub("_published","",gsub("_published.DG","",gsub(".SG","",dd$V1)))
text(1:length(dd$V4), rep(1800, length(dd$V4)),
     labels= name_lab, col="black", srt=35, xpd=TRUE, adj=1,cex=0.7, cex.lab=0.7)
axis(2, seq(2000,6800,500), lty = 1,col = "black",tck="y",labels = rep('',length(seq(2000,6800,500))))
text(rep(-3.5,max(2000,6800,500)), seq(2000,6800,500),labels= seq(2000,6800,500),
     col="grey30", srt=0, xpd=TRUE, adj=-0.1,cex=0.8)
abline(v=2.5,lty=3,col="grey20")
abline(v=20.5,lty=3,col="grey20")
abline(v=23.5,lty=3,col="grey20")
abline(v=28.5,lty=3,col="grey20")
abline(v=36.5,lty=3,col="grey20")
abline(v=42.5,lty=3,col="grey20")
abline(v=47.5,lty=3,col="grey20")
abline(v=56.5,lty=3,col="grey20")
abline(v=63.5,lty=3,col="grey20")
text(c(-1.3,10,20.6,23.8,28.6,31.8,37,43,48,53,57,63.53),rep(6500,5), 
     labels= c("Balkans","Hungary","Czech","Germany","Poland","Ukraine",
               "France","Italy","Spain","Portugal","Britain countries","Scandinavia"),col="grey30", srt=0, xpd=TRUE, adj=-0.1,cex=0.8)
text(c(20.6,23.6),rep(6200,5), labels= c("Republic","Austria"),
     col="grey30", srt=0, xpd=TRUE, adj=-0.1,cex=0.8)
dev.off()

# Bronze age steppe mixture
jpeg(file ="results/Figure3_map_SP_AdmixtureDates.jpeg",width = 16,height = 7,units="in",res =1000)
par(mai = c(2.6,1,0.1,2.8))
dd=rbind(data[data$V8=="green3",],data[data$V8=="lightpink2",])
dd$V8=gsub("springgreen","green3",dd$V8)
target_fossil=fossil_range[fossil_range$V2%in%as.character(dd$V1),]
group_names=unique(target_fossil$V2)
head(target_fossil)
target_fossil$V2 <- as.character(target_fossil$V2)
target_fossil$V6 <- as.numeric(as.character(target_fossil$V3))-1950
pch_str=as.numeric(dd$V9)
for(i in group_names)
{
  k=which(dd$V1==i)
  dd[k,10]=min(as.numeric(target_fossil[target_fossil$V2==i,]$V6))
  dd[k,11]=max((as.numeric(target_fossil[target_fossil$V2==i,]$V6)))
  dd[k,12]=mean(as.numeric(target_fossil[target_fossil$V2==i,]$V6))
}
plot(x=seq(1,length(dd$V1),1),y=dd$V6,type = "p",
     col=dd$V8,pch=dd$V9,cex=1.5, ylim = c(500,5000), xlim = c(1,length(dd$V1)+0.5),
     xaxt='n',yaxt='n',xlab = "",ylab = "Estimated admixture time (BCE)", main="");
grid(nx=8,ny=10);
segments(x0 =seq(1,length(dd$V1),1),x1 = seq(1,length(dd$V1),1),y0 =(dd$V6+dd$V7),
         y1 = (dd$V6-dd$V7),col=dd$V8,lty = 1)
segments(x0 =seq(1,length(dd$V1),1)-0.1,x1 = seq(1,length(dd$V1),1)+0.1,y0 =(dd$V6+dd$V7),
         y1 = (dd$V6+dd$V7),col=dd$V8)
segments(x0 =seq(1,length(dd$V1),1)-0.1,x1 = seq(1,length(dd$V1),1)+0.1,y0 =(dd$V6-dd$V7),
         y1 = (dd$V6-dd$V7),col=dd$V8)
axis(1, 1:length(dd$V1), lty = 1,col = "black",tck="y",labels = rep('', length(dd$V1)))
points(x=seq(1,length(dd$V1),1),y=dd$V12,type = "p",col="grey20",pch=1,cex=0.8)
name_lab=gsub(".SG","",dd$V1)
text(1:length(dd$V4), rep(350, length(dd$V4)),
     labels= name_lab, col="black", srt=45, xpd=TRUE, adj=1,cex=1)
axis(2, seq(500,5000,500), lty = 1,col = "black",tck="y",labels = rep('',length(seq(500,5000,500))))
text(rep(-2,max(500,5000,500)), seq(500,5000,500),labels = c(seq(500,5000,500)),
     col="grey30", srt=0, xpd=TRUE, adj=-0.1,cex=1)
abline(v=7.5,lty=3,col="grey20")
abline(v=26.5,lty=3,col="grey20")
text(c(1.6,15,29),rep(4900,5),
     labels= c("Late Neolithic","Chacolithic to Bronze Age","Middle to Late Bronze Age"),col="grey30", srt=0, xpd=TRUE, adj=-0.1,cex=0.8)
legend(35.5,5000,legend = c("Corded Ware complex","Bell Beaker complex"),
       col=c(rep("green3",2)),pch=c(15,17),lty=c(1,1),xpd=T,cex=1)
dev.off()

