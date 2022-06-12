#### Figure1 supplement figures 
# supplement1
rm(list=ls())
DIR <- "DATES_EuropeanHolocene"

# Figure 1 supplement1
#variation in admixture time
png("results/Figure 1 - figure supplement 1.png",width = 8, height = 6, units = 'in', res = 200) 
data=read.table(file = "data/Figure1_Supplement1")
plot(x=data$V1,y = data$V3,col="deepskyblue",pch=15,
     xlab = "True admixture time (generations)",ylab = "Estimated admixture time (generations)",
     main = "Varying time of admixture up to 300 generations",xlim=c(0,300), ylim = c(0,max(data$V3[1:30]))); grid (nx=5,ny=4)
segments(x0 =data$V1,x1 = data$V1,y0 =(data$V3+data$V6),
         y1 = (data$V3-data$V6),col="deepskyblue")
segments(x0 =data$V1-2,x1 = data$V1+2,y0 =(data$V3+data$V6),
         y1 = (data$V3+data$V6),col="deepskyblue")
segments(x0 =data$V1-2,x1 = data$V1+2,y0 =(data$V3-data$V6),
         y1 = (data$V3-data$V6),col="deepskyblue")
lines(x=data$V1,y=data$V1,col="darkgrey",lty=2)
dev.off()

# Figure 1: Supplement 2
#Impact of sample size of (a) target population (b) reference population 
# panel A - Target sample size
rm(list=ls())
DIR <- "DATES_EuropeanHolocene"
png("results/Figure 1 - figure supplement 2.png",width = 10, height = 6, units = 'in', res = 200)
par(mfrow=c(2,4))
data=read.table(file = "data/Figure1_Supplement2A")
kol=c("darkorange2","green3","violetred1","royalblue1","coral","yellow3","cyan2","darkgoldenrod1","darkorchid1","dodgerblue1",
      "paleturquoise4");
len=c("n=1","n=5","n=10","n=20")
arr=c(1,5,10,20)
c=1;
for(i in arr)
{
  k=which(arr==i);
  dd=data[data$V2==i,]
  plot(x=dd$V1,y=dd$V3, col=kol[c],pch=16,xlab = "True admixture time (gen)",
       ylab = "Estimated admixture time (gen)", ylim = c(0,max(dd$V3+dd$V4))); grid ();
  segments(x0 =dd$V1,x1 = dd$V1,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  lines(x=dd$V1,y=dd$V1,col="darkgrey",lty=2)
  legend("topleft",legend = len[k],col=kol[c],pch = 16,bg = "white");
  c=c+1
}
# Panel B - reference sample size
data=read.table(file = "data/Figure1_Supplement2B")
kol=c("darkorange","green3","cyan2","violetred1","royalblue1")
len1=c("reference pop size=1","reference pop size=5","reference pop size=10","reference pop size=20")
len=c(1,5,10,20)
#par(mfrow=c(3,2))
c=1;
for(i in len)
{
  #len=1
  k=which(i==len);
  dd=data[data$V1==i,]
  plot(x=dd$V2,y=dd$V3, col=kol[c],pch=17,xlab = "True admixture time (gen)",
       ylab = "Estimated admixture time (gen)", ylim = c(min(dd$V3-dd$V4),max(dd$V3+dd$V4))); grid ();
  segments(x0 =dd$V2,x1 = dd$V2,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col=kol[c]);
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  lines(x=dd$V2,y=dd$V2,col="darkgrey",lty=2)
  legend("topleft",legend = len1[k],col=kol[c],pch = 17,bg = "white");
  c=c+1;
}
title("(A) Effect of sample size of target population", line = -1.5, outer = TRUE,cex=1.3)
title("(B) Effect of sample size of reference populations", line = -25, outer = TRUE,cex=1.3)
dev.off()


# Figure 1: Supplement 3
#Impact of admixture proportion on the inference of (a) time of gene flow (b) estimated admixture proportion
# panel A- Time of gene flow
rm(list=ls())
DIR <- "DATES_EuropeanHolocene"
png("results/Figure 1 - figure supplement 3.png",width = 10, height = 6, units = 'in', res = 200)
par(mfrow=c(2,4))
data=read.table(file = "data/Figure1_Supplement3A")
len=c("α=0.01","α=0.05","α=0.2","α=0.4")
arr=c(0.01,0.05,2,4)
kol=c("green","darkorchid","yellowgreen","blue","deeppink","red2","purple")
print (arr)
c=1
for(i in arr)
{
  k=which(arr==i);
  dd=data[data$V2==i,]
  plot(x=dd$V1,y=dd$V3, col=kol[c],pch=18,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)", 
       ylim = c(0,max(dd$V3+dd$V4)),cex=1.5); grid ();
  #main="Admixture time for varying θ"
  segments(x0 =dd$V1,x1 = dd$V1,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  lines(x=dd$V1,y=dd$V1,col="darkgrey",lty=2)
  legend("topleft",legend = len[k],col=kol[c],pch = 18,bg = "white");
  c=c+1;  
}
title("(A) Impact of admixture proportion on the estimated time of admixture", line = -1.5, outer = TRUE,cex=1.3)
# panel B- proportions of admixture
data=read.table(file = "data/Figure1_Supplement3B")
len=c("α=0.01","α=0.05","α=0.1","α=0.2","α=0.4")
c=1;
for(i in arr)
{
  #i=5
  if(i<1) {rr=i} else {rr=i/10}
  k=which(arr==i);
  dd=data[data$V2==i,]
  lim=max(dd$V6)+rr/4
  plot(x=dd$V1,y=dd$V6, col=kol[c],pch=19,xlab = "True admixture time (gen)",
       ylab = "Estimated Theta (θ)",las=1,ylim = c(0,lim)); grid ();
  #main="Admixture prorportion inference"
  segments(x0 =dd$V1,x1 = dd$V1,y0 =(dd$V6+dd$V10),y1 = (dd$V6-dd$V10),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V6+dd$V10),y1 = (dd$V6+dd$V10),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V6-dd$V10),y1 = (dd$V6-dd$V10),col=kol[c]);
  lines(x=dd$V1,y=rep(rr,NROW(dd)),col="red",lty=2)
  legend("bottomright",legend = len[k],col=kol[c],pch = 19,bg = "white");
  c=c+1;
}
title("(B) Impact of admixture proportion on estimated ancestry proportion", line = -25, outer = TRUE,cex=1.3)
dev.off()


# Figure 1: Supplement 4
#Impact of divergence between the ancestral population and reference used in DATES
rm(list=ls())
DIR <- "DATES_EuropeanHolocene"
png("results/Figure 1 - figure supplement 4.png",width = 9, height = 9, units = 'in', res = 700)
par(mfrow=c(2,2))
data=read.table(file = "data/Figure1_Supplement4")
kol=c("darkorange","green3","cyan","violetred1")
len1=c("Fst(Yoruba-Yoruba)=0.000","Fst(Yoruba-BantuKenya)=0.009",
       "Fst(Yoruba-San)=0.103")
arr=c("Yoruba","BantuKenya","San")
c=1;
for(i in arr)
{
  dd=data[data$V2==i,]
  plot(x=dd$V3,y=dd$V4, col=kol[c],pch=15,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
       ylim = c(min(dd$V4-dd$V5),220)); grid ();
  segments(x0 =dd$V3,x1 = dd$V3,y0 =(dd$V4+dd$V5),y1 = (dd$V4-dd$V5),col=kol[c]);
  segments(x0 =dd$V3-2,x1 = dd$V3+2,y0 =(dd$V4+dd$V5),y1 = (dd$V4+dd$V5),col=kol[c]);
  segments(x0 =dd$V3-2,x1 = dd$V3+2,y0 =(dd$V4-dd$V5),y1 = (dd$V4-dd$V5),col=kol[c]);
  lines(x=dd$V3,y=dd$V3,col="darkgrey",lty=2)
  legend("topleft",legend = len1[c],col=kol[c],pch = 15,bg = "white");
  c=c+1;
}
title("Impact of divergence between the ancestral population and reference populations used in DATES", line = -1.5, outer = TRUE,cex=1.3)
dev.off()

# Figure 1: Supplement 5
#Impact of divergence between the two source populations that contribute to the target populations
# panel A- for target n=10
rm(list=ls())
DIR <- "DATES_EuropeanHolocene"
png("results/Figure 1 - figure supplement 5.png",width = 12, height = 6, units = 'in', res = 700)
par(mfrow=c(2,4))
data=read.table(file = "data/Figure1_Supplement5A")
len1=c("CEU/YRI", "CEU/CHB","CEU/MXL","CEU/TSI")
len2=c("French/Yoruba(0.154)", "French/Tujia(0.110)",
       "French/Maya(0.037)","French/Italian(0.004)")
len=c("YRI","CHB","MXL","TSI")
for(i in len)
{
  # i="ITU"
  dd=data[data$V1==i,]
  k=which(i==len);
  plot(x=dd$V2,y=dd$V3, col="navy",pch=19,xlab = "True admixture time (gen)",
       ylab = "Estimated admixture time (gen)",ylim = c(0,250),
       main=paste("True source populations:",len1[k],sep=" ")); grid ();
  segments(x0 =dd$V2 ,x1 = dd$V2, y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col="navy");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3+dd$V4),y1 =(dd$V3+dd$V4),col="navy");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3-dd$V4),y1 =(dd$V3-dd$V4),col="navy");
  lines(x=dd$V2,y=dd$V2,col="darkgrey")
  kol=c("white",dd$V7[1])
  legend("topleft",legend = paste(" References",len2[k],sep="="),
         col="navy",pch = c(19),bg = "white");
}
title("(A) Impact of divergence between the two source populations (n=10)", line = -1, outer = TRUE,cex=1.3)
# panel B- for target n=1
data=read.table(file = "data/Figure1_Supplement5B")
for(i in len)
{
  dd=data[data$V1==i,]
  k=which(i==len);
  plot(x=dd$V2,y=dd$V3, col="red",pch=16,xlab = "True admixture time (gen)",
       ylab = "Estimated admixture time (gen)",ylim = c(0,250),
       main=paste("True source populations:",len1[k],sep=" ")); grid ();
  segments(x0 =dd$V2 ,x1 = dd$V2, y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col="red");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3+dd$V4),y1 =(dd$V3+dd$V4),col="red");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3-dd$V4),y1 =(dd$V3-dd$V4),col="red");
  lines(x=dd$V2,y=dd$V2,col="darkgrey")
  kol=c("white",dd$V8[1])
  legend("topleft",legend = paste(" References",len2[k],sep="="),
         col="red",pch = c(16),bg = "white");
}
title("(B) Impact of divergence between the two source populations (n=1)", line = -23, outer = TRUE,cex=1.3)
dev.off()

# Figure 1: Supplement 6
#Using admixed population itself as one of the reference groups in DATES
rm(list=ls())
DIR <- "DATES_EuropeanHolocene"
png("results/Figure 1 - figure supplement 6.png",
    width = 11, height = 11, units = 'in', res = 200)
par(mfrow=c(2,2))
data=read.table(file = "data/Figure1_Supplement6")
len=c(2,4,6,8)
a=c("A","B","C","D")
data
par(mfrow=c(2,2))
for(i in len)
{
  dd=data[data$V1==i,]
  k=which(i==len)
  rr=paste("(",paste(a[k],paste("Admixed pop (as target and reference):",i*10,"%CEU+",(10-i)*10,"%YRI"),sep=") "),sep="")
  
  plot(x=dd$V2,y=dd$V4, col="deepskyblue",pch=15,xlab = "True admixture time (gen)",
       ylab = "Estimated admixture time (gen)",
       ylim = c(0,max(data$V4+data$V5)),main=rr); grid ()
  lines(x=dd$V2,y=dd$V8, col="orange2",pch=19,type="p")
  segments(x0 =dd$V2,x1 = dd$V2,y0 =(dd$V4+dd$V5),y1 = (dd$V4-dd$V5),col="deepskyblue");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V4+dd$V5),y1 = (dd$V4+dd$V5),col="deepskyblue");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V4-dd$V5),y1 = (dd$V4-dd$V5),col="deepskyblue");
  segments(x0 =dd$V2,x1 = dd$V2,y0 =(dd$V8+dd$V9),y1 = (dd$V8-dd$V9),col="orange2");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V8+dd$V9),y1 = (dd$V8+dd$V9),col="orange2");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V8-dd$V9),y1 = (dd$V8-dd$V9),col="orange2");
  
  lines(x=dd$V2,y=dd$V2,col="darkgrey",lty=2)
  legend("topleft",legend = c("Refpops:French and Admixed","Refpops:Yoruba and Admixed"),text.col = c("deepskyblue","orange2"),pch = c(15,19),
         col = c("deepskyblue","orange2"),bg = "white");
  
}
dev.off()


# Figure 1: Supplement 7
#Impact of features of ancient DNA (a) missing rate (b) missing data and use of pseudo-haploid genotypes 
#(c) missing data and use of pseudo-haploid genotypes for 1 target individual
# panelA
rm(list=ls())
DIR <- "DATES_EuropeanHolocene"
png("results/Figure 1 - figure supplement 7.png",width = 12, height = 9, units = 'in', res = 700)
par(mfrow=c(3,4))
data=read.table(file = "data/Figure1_Supplement7A")
kol=c("darkorange","green3","cyan","violetred1","royalblue1","orangered1","darkorchid1",
      "yellowgreen","slateblue2","tomato","palevioletred2","red","orange")
len1=c("missing prop=10%","missing prop=20%","missing prop=40%","missing prop=60%")
len=c(10,20,40,60)
c=1;
for(i in len)
{
  dd=data[data$V2==i,]
  k=which(i==len);
  plot(x=dd$V1,y=dd$V3, col=kol[k],pch=17,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
       ylim = c(0,220)); grid ();
  lines(x=dd$V1,y=dd$V5, col=dd$V8,pch=6,type="p")
  segments(x0 =dd$V1,x1 = dd$V1,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col=kol[k]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col=kol[k]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col=kol[k]);
  lines(x=dd$V1,y=dd$V1,col="darkgrey",lty=2)
  legend("topleft",legend = len1[k],col=kol[k],pch = c(17,15),bg = "white");
  c=c+1;
}
title("(A) Diploid genotypes with missing data for n=10 admixed individuals", line = -1.5, outer = TRUE,cex=1.3)

data=read.table(file = "data/Figure1_Supplement7B")
data1=read.table(file = "data/Figure1_Supplement7C")
kol=c("darkorange","green3","cyan","violetred1","royalblue1","orangered1","darkorchid1","yellowgreen","slateblue2","tomato","palevioletred2")
c=1;
for(i in len)
{
  dd=data[data$V2==i,]
  k=which(i==len);
  plot(x=dd$V1,y=dd$V3, col=kol[c],pch=16,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
       ylim = c(min(dd$V3-dd$V4),max(dd$V3+dd$V4,250))); grid ();
  segments(x0 =dd$V1,x1 = dd$V1,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  lines(x=dd$V1,y=dd$V1,col="darkgrey",lty=2)
  legend("topleft",legend = c(len1[k]),col=kol[c],pch = 16,bg = "white");
  c=c+1;
}
title("(B) Pseudo-haploid genotypes with missing data for n=10 admixed individuals", line = -25, outer = TRUE,cex=1.3)

c=1;
for(i in len)
{
  dd1=data1[data1$V2==i,]
  k=which(i==len);
  plot(x=dd1$V1,y=dd1$V3, col=kol[c],pch=22,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
       ylim = c(min(dd1$V3-dd1$V4),max(dd1$V3+dd1$V4,250))); grid ();
  segments(x0 =dd1$V1,x1 = dd1$V1,y0 =(dd1$V3+dd1$V4),y1 = (dd1$V3-dd1$V4),col=kol[c],lty=2);
  segments(x0 =dd1$V1-2,x1 = dd1$V1+2,y0 =(dd1$V3+dd1$V4),y1 = (dd1$V3+dd1$V4),col=kol[c],lty=2);
  segments(x0 =dd1$V1-2,x1 = dd1$V1+2,y0 =(dd1$V3-dd1$V4),y1 = (dd1$V3-dd1$V4),col=kol[c],lty=2);
  lines(x=dd1$V1,y=dd1$V1,col="darkgrey",lty=2)
  legend("topleft",legend = c(len1[k]),col=kol[c],pch = 16,bg = "white");
  c=c+1;
}
title("(C) Pseudo-haploid genotypes with missing data for n=1 admixed individuals", line = -47, outer = TRUE,cex=1.3)
dev.off()


###### Figure1 supplement 8
# Impact of sample size and data quality of target and reference populations as a function of divergence between true and reference populations used in DATES
# for 10 individuals
rm(list=ls())
DIR <- "DATES_EuropeanHolocene"
png("results/Figure 1 - figure supplement 8.png",width = 10, height = 10, units = 'in', res = 700)
par(mfrow=c(3,3))
data=read.table(file = "data/Figure1_Supplement8",header=T)
kol=c("darkorange","darkorchid1","cyan","deeppink")
ref_used=c("FY","FB","FS")
len1=c("Reference missing prop=0%","Reference missing prop=20%","Reference missing prop=40%")
len=c(20,40,60)
rlen=c(20,40)
for(w in ref_used)
{
  sub=data[data$references==w,]
  for(r in len)
  {
    c=1;
    rd=sub[sub$missing_target==r,]
    w=which(r==len);
    dd=rd[rd$missing_ref==0,]
    plot(x=dd$admixture_time,y=dd$dates_n10, col=kol[c],pch=15,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",main=paste("Missing proportion in the target: ",paste(len[w],"%",sep=""),sep = ""),xlim=c(0,210),ylim = c(0,250)); grid ();
    segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$dates_n10+dd$dates_n10_se),y1 = (dd$dates_n10-dd$dates_n10_se),col=kol[c]);
    segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$dates_n10+dd$dates_n10_se),y1 = (dd$dates_n10+dd$dates_n10_se),col=kol[c]);
    segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$dates_n10-dd$dates_n10_se),y1 = (dd$dates_n10-dd$dates_n10_se),col=kol[c]);
    lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
    c=c+1; err=3
    for(i in rlen)
    {
      dd=rd[rd$missing_ref==i,]
      k=which(i==len);
      points(x=dd$admixture_time+err,y=dd$dates_n10, col=kol[c],pch=15); grid ();
      segments(x0 =dd$admixture_time+err,x1 = dd$admixture_time+err,y0 =(dd$dates_n10+dd$dates_n10_se),y1 = (dd$dates_n10-dd$dates_n10_se),col=kol[c]);
      segments(x0 =dd$admixture_time+err-2,x1 = dd$admixture_time+err+2,y0 =(dd$dates_n10+dd$dates_n10_se),y1 = (dd$dates_n10+dd$dates_n10_se),col=kol[c]);
      segments(x0 =dd$admixture_time+err-2,x1 = dd$admixture_time+err+2,y0 =(dd$dates_n10-dd$dates_n10_se),y1 = (dd$dates_n10-dd$dates_n10_se),col=kol[c]);
      lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
      c=c+1; err=err+5
    }
    legend("topleft",legend = c(len1),col=kol[1:4],pch = 15,bg = "white");
  }
}
title("(a) Reference populations of French and Yoruba (FST(true, reference) ~ 0) for target population n=10 ", line = -1, outer = TRUE,cex=1.3)
title("(b) Reference populations of French and Bantu Kenya ( FST(true, reference) ~ 0.009) for target population n=10", line = -26, outer = TRUE,cex=1.3)
title("(c) Reference populations of French and San ( FST(true, reference) ~ 0.103) for target population n=10", line = -51, outer = TRUE,cex=1.3)
dev.off()

###### Figure1 supplement 9
# Impact of sample size and data quality of target and reference populations as a function of divergence between true and reference populations used in DATES
# for single target individual
png("results/Figure 1 - figure supplement 9.png",
    width = 10, height = 10, units = 'in', res = 700)
par(mfrow=c(3,3))
data=read.table(file = "data/Figure1_Supplement9",header=T)
kol=c("darkorange","darkorchid1","cyan","deeppink")
ref_used=c("FY","FB","FS")
len1=c("Reference missing prop=0%","Reference missing prop=20%","Reference missing prop=40%")
len=c(20,40,60)
rlen=c(20,40)
for(w in ref_used)
{
  sub=data[data$references==w,]
  for(r in len)
  {
    c=1;
    rd=sub[sub$missing_target==r,]
    w=which(r==len);
    dd=rd[rd$missing_ref==0,]
    plot(x=dd$admixture_time,y=dd$dates_n1, col=kol[c],pch=15,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
         main=paste("Missing proportion in the target: ",paste(len[w],"%",sep=""),sep = ""),xlim=c(0,210),
         ylim = c(0,250)); grid ();
    segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$dates_n1+dd$dates_n1_se),y1 = (dd$dates_n1-dd$dates_n1_se),col=kol[c]);
    segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$dates_n1+dd$dates_n1_se),y1 = (dd$dates_n1+dd$dates_n1_se),col=kol[c]);
    segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$dates_n1-dd$dates_n1_se),y1 = (dd$dates_n1-dd$dates_n1_se),col=kol[c]);
    lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
    c=c+1; err=3
    for(i in rlen)
    {
      dd=rd[rd$missing_ref==i,]
      k=which(i==len);
      points(x=dd$admixture_time+err,y=dd$dates_n1, col=kol[c],pch=15); grid ();
      segments(x0 =dd$admixture_time+err,x1 = dd$admixture_time+err,y0 =(dd$dates_n1+dd$dates_n1_se),y1 = (dd$dates_n1-dd$dates_n1_se),col=kol[c]);
      segments(x0 =dd$admixture_time+err-2,x1 = dd$admixture_time+err+2,y0 =(dd$dates_n1+dd$dates_n1_se),y1 = (dd$dates_n1+dd$dates_n1_se),col=kol[c]);
      segments(x0 =dd$admixture_time+err-2,x1 = dd$admixture_time+err+2,y0 =(dd$dates_n1-dd$dates_n1_se),y1 = (dd$dates_n1-dd$dates_n1_se),col=kol[c]);
      lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
      c=c+1; err=err+5
    }
    legend("topleft",legend = c(len1),col=kol[1:4],pch = 15,bg = "white");
  }
}
title("(a) Reference populations of French and Yoruba (FST(true, reference) ~ 0) for target population n=1 ", line = -1, outer = TRUE,cex=1.3)
title("(b) Reference populations of French and Bantu Kenya ( FST(true, reference) ~ 0.009) for target population n=1 ", line = -26, outer = TRUE,cex=1.3)
title("(c) Reference populations of French and San ( FST(true, reference) ~ 0.103) for target population n=1 ", line = -51, outer = TRUE,cex=1.3)
dev.off()
