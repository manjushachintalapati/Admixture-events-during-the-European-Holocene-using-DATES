#Figure1. Simulations to test DATES

rm(list=ls())
jpeg(file ="results/Figure1_Maintext.jpeg",width = 10,height = 5,units="in",res = 500)
par(mfrow=c(1,2))
DIR <- "Admixture-events-during-the-European-Holocene-using-DATES"
# Panel A
data=read.table(file = paste(DIR,"data/Figure1_data_panelA", sep=""), header = T)
dd=data[data$n==1,]
plot(x=dd$admixture_time,y=dd$d_mean, col="darkorange2",pch=16,xlab = "True admixture time (gen)",
     ylab = "Estimated admixture time (gen)", ylim = c(0,max(dd$d_mean+dd$d_se)),
     main="(A) Variation in sample size",cex.axis=1.2,cex.lab=1.2); grid ();
segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean+dd$d_se),col="darkorange2");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean+dd$d_se),y1 = (dd$d_mean+dd$d_se),col="darkorange2");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean-dd$d_se),col="darkorange2");
dd=data[data$n==20,]
points(x=dd$admixture_time,y=dd$d_mean, col="green3",pch=16,xlab = "True admixture time (gen)",
     ylab = "Estimated admixture time (gen)", ylim = c(0,max(dd$d_mean+dd$d_se)),
     main="(A) Variation in sample size",cex.axis=1.2,cex.lab=1.2); grid ();
segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean+dd$d_se),col="green3");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean+dd$d_se),y1 = (dd$d_mean+dd$d_se),col="green3");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean-dd$d_se),col="green3");
lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
legend("topleft",legend = c("n=1","n=20"),col=c("darkorange2","green3"),pch = 16,bg = "white");
#panel B
data=read.table(file = paste(DIR,"data/Figure1_data_panelB", sep="") ,header = T)
dd=data[data$missing==10,]
plot(x=dd$admixture_time,y=dd$d_mean, col="tomato",pch=17,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
     ylim = c(min(dd$d_mean-dd$d_se),max(dd$d_mean+dd$d_se)),
     main="(B) Ancient DNA mimic with missing sites",cex.axis=1.2,cex.lab=1.2); grid ();
segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$d_mean+dd$d_se),y1 = (dd$d_mean-dd$d_se),col="tomato");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean+dd$d_se),y1 = (dd$d_mean+dd$d_se),col="tomato");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean-dd$d_se),col="tomato");
dd=data[data$missing==60,]
points(x=dd$admixture_time,y=dd$d_mean, col="darkviolet",pch=17,cex.axis=1.2,cex.lab=1.2); grid ();
#lines(x=dd$admixture_time,y=dd$admixture_time, col="tomato",pch=6,type="p")
segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$d_mean+dd$d_se),y1 = (dd$d_mean-dd$d_se),col="darkviolet");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean+dd$d_se),y1 = (dd$d_mean+dd$d_se),col="darkviolet");
segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$d_mean-dd$d_se),y1 = (dd$d_mean-dd$d_se),col="darkviolet");
lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
legend("topleft",legend = c("missing proportion=10%","missing proportion=60%"),
       col=c("tomato","darkviolet"),pch = 17,bg = "white");
dev.off()

