#Figure1. Simulations to test DATES

rm(list=ls())
jpeg(file ="results/Figure1_Maintext.jpeg",width = 10,height = 5,units="in",res = 500)
par(mfrow=c(1,2))
DIR <- "DATES_EuropeanHolocene"
# Panel A
data=read.table(file = "data/Figure1_data_panelA", header = T)
dd=data[data$n==1,]
plot(x=dd$admixture_time,y=dd$d_mean, col="darkorange2",pch=16,xlab = "True admixture time (gen)",
     ylab = "Estimated admixture time (gen)", ylim = c(0,max(dd$d_mean+dd$d_se)),
     main="(A) Variation in sample size",cex.axis=1.2,cex.lab=1.2); grid ();
segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean+dd$d_se),col="darkorange2");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean+dd$d_se),y1 = (dd$d_mean+dd$d_se),col="darkorange2");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean-dd$d_se),col="darkorange2");

dd=data[data$n==10,]
points(x=dd$admixture_time,y=dd$d_mean, col="green3",pch=16,xlab = "True admixture time (gen)",
     ylab = "Estimated admixture time (gen)", ylim = c(0,max(dd$d_mean+dd$d_se)),
     main="(A) Variation in sample size",cex.axis=1.2,cex.lab=1.2); grid ();
segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean+dd$d_se),col="green3");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean+dd$d_se),y1 = (dd$d_mean+dd$d_se),col="green3");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean-dd$d_se),col="green3");
lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
legend("topleft",legend = c("n=1","n=20"),col=c("darkorange2","green3"),pch = 16,bg = "white");

#panel B
data=read.table(file = "data/Figure1_data_panelB" ,header = T)
kol=c("tomato","darkviolet","forestgreen")
len1=c("missing proportion=10%","missing proportion=30%","missing proportion=60%")
len=c(10,30,60)
c=1;
dd=data[data$V2==10,]
k=which(i==len);
plot(x=dd$V1,y=dd$V3, col="tomato",pch=15,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
     ylim = c(min(dd$V3-dd$V4),max(dd$V3+dd$V4)),
     main="(B) Effect of data quality",cex.axis=1.2,cex.lab=1.2); grid ();
lines(x=dd$V1,y=dd$V5, col=dd$V8,pch=6,type="p")
segments(x0 =dd$V1,x1 = dd$V1,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col="tomato");
segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col="tomato");
segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col="tomato");
c=2;
dd=data[data$V2==30,]
k=which(i==len);
points(x=dd$V1+2,y=dd$V3, col="darkviolet",pch=16,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
       ylim = c(min(dd$V3-dd$V4),max(dd$V3+dd$V4))); grid ();
segments(x0 =dd$V1+2,x1 = dd$V1+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col="darkviolet");
segments(x0 =dd$V1+2-2,x1 = dd$V1+2+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col="darkviolet");
segments(x0 =dd$V1+2-2,x1 = dd$V1+2+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col="darkviolet");
dd=data[data$V2==60,]
k=which(i==len);
points(x=dd$V1+4,y=dd$V3, col="forestgreen",pch=17); grid ();
segments(x0 =dd$V1+4,x1 = dd$V1+4,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col="forestgreen");
segments(x0 =dd$V1+4-2,x1 = dd$V1+4+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col="forestgreen");
segments(x0 =dd$V1+4-2,x1 = dd$V1+4+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col="forestgreen");
lines(x=dd$V1,y=dd$V1,col="darkgrey",lty=2)
legend("topleft",legend = len1,col=kol,pch = c(15,16,17),bg = "white");
dev.off()
