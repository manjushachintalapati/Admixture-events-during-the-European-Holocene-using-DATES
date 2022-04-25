# Figure 1 Supplementary Figures

rm(list=ls())
# Figure 1: Supplement 1
# Varying time of admixture up to 300 generations
DIR <- "Admixture-events-during-the-European-Holocene-using-DATES"
png(paste(DIR,"results/Figure1_Supplement1.png", sep=""),width = 8, height = 6, units = 'in', res = 200) 
data=read.table(file = paste(DIR,"data/Figure1_Supplement1", sep=""))
plot(x=data$V1,y = data$V3,col="deepskyblue",pch=15,
     xlab = "True admixture time (generations)",ylab = "Estimated admixture time (generations)",
     main = "Admixture time from DATES ",xlim=c(0,300), ylim = c(0,max(data$V3[1:30]))); grid (nx=5,ny=4)
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
# A (Target sample size)
png(paste(DIR,"results/Figure1_Supplement2A.png", sep=""),width = 7, height = 10, units = 'in', res = 200)
par(mfrow=c(4,3))
data=read.table(file = paste(DIR,"data/Figure1_Supplement2A", sep="") )
kol=c("darkorange2","green3","violetred1","royalblue1","coral","yellow3","cyan2","darkgoldenrod1","darkorchid1","dodgerblue1",
      "paleturquoise4");
len=c("n=1","n=5","n=10","n=15","n=20","n=25","n=30","n=35","n=40","n=45","n=50")
arr=c(1,5,10,15,20,25,30,35,40,45,50)
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
title("Varying sample size of the target", line = -1.5, outer = TRUE,cex=1.3)
dev.off()

# B (reference sample size)
png(paste(DIR,"results/Figure1_Supplement2B.png", sep=""),width = 6, height = 8, units = 'in', res = 200)
par(mfrow=c(3,2))
data=read.table(file = paste(DIR,"data/Figure1_Supplement2B", sep=""))
kol=c("darkorange","green3","cyan2","violetred1","royalblue1")
len1=c("reference pop size=1","reference pop size=5","reference pop size=10","reference pop size=15","reference pop size=20")
len=c(1,5,10,15,20)
par(mfrow=c(3,2))
c=1;
for(i in len)
{
  k=which(i==len);
  dd=data[data$V1==i,]
  plot(x=dd$V2,y=dd$V3, col=kol[c],pch=17,xlab = "True admixture time (gen)",
       ylab = "Estimated admixture time (gen)", ylim = c(min(dd$V3-dd$V4),max(dd$V3+dd$V4))); grid ();
  #main="varying sample size of references"
  segments(x0 =dd$V2,x1 = dd$V2,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col=kol[c]);
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  lines(x=dd$V2,y=dd$V2,col="darkgrey",lty=2)
  legend("topleft",legend = len1[k],col=kol[c],pch = 17,bg = "white");
  c=c+1;
}
title("Varying sample size of the reference populations", line = -1.5, outer = TRUE,cex=1.3)
dev.off()

# Figure 1: Supplement 3
#Impact of admixture proportion on the inference of (a) time of gene flow (b) estimated admixture proportion
# A (Time of gene flow)
data=read.table(file = paste(DIR,"data/Figure1_Supplement3A", sep=""))
head(data)
len=c("α=0.01","α=0.05","α=0.1","α=0.2","α=0.3","α=0.4","α=0.5")
arr=c(0.01,0.05,1,2,3,4,5)
kol=c("green","darkorchid","yellowgreen","blue","deeppink","red2","purple")
png(paste(DIR,"results/Figure1_Supplement3A.png", sep=""),width = 6, height = 10, units = 'in', res = 200)
print (arr)
par(mfrow=c(4,2))
c=1
for(i in arr)
{
  k=which(arr==i);
  dd=data[data$V2==i,]
  plot(x=dd$V1,y=dd$V3, col=kol[c],pch=18,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)", 
       ylim = c(0,max(dd$V3+dd$V4)),cex=1.5); grid ();
  segments(x0 =dd$V1,x1 = dd$V1,y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3+dd$V4),y1 = (dd$V3+dd$V4),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V3-dd$V4),y1 = (dd$V3-dd$V4),col=kol[c]);
  lines(x=dd$V1,y=dd$V1,col="darkgrey",lty=2)
  legend("topleft",legend = len[k],col=kol[c],pch = 18,bg = "white");
  c=c+1;  
}
title("A) Admixture time inference for varying admixture proportions", line = -1.5, outer = TRUE,cex=1.3)
dev.off()
# B (Proportions of admixture)
png(paste(DIR,"results/Figure1_Supplement3B.png", sep=""),width = 6, height = 10, units = 'in', res = 500)
par(mfrow=c(4,2))
data=read.table(file = paste(DIR,"data/Figure1_Supplement3B", sep=""))
kol=c("green","darkorchid","yellowgreen","blue","coral","violetred2","deepskyblue","orange2","green4","red2","purple")
len=c("α=0.01","α=0.05","α=0.1","α=0.2","α=0.3","α=0.4","α=0.5")
arr=c(0.01,0.05,1,2,3,4,5)
c=1;
for(i in arr)
{
  if(i<1) {rr=i} else {rr=i/10}
  k=which(arr==i);
  dd=data[data$V2==i,]
  lim=max(dd$V6)+rr/4
  plot(x=dd$V1,y=dd$V6, col=kol[c],pch=19,xlab = "True admixture time (gen)",
       ylab = "Estimated Theta (θ)",las=1,ylim = c(0,lim)); grid ();
  segments(x0 =dd$V1,x1 = dd$V1,y0 =(dd$V6+dd$V10),y1 = (dd$V6-dd$V10),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V6+dd$V10),y1 = (dd$V6+dd$V10),col=kol[c]);
  segments(x0 =dd$V1-2,x1 = dd$V1+2,y0 =(dd$V6-dd$V10),y1 = (dd$V6-dd$V10),col=kol[c]);
  lines(x=dd$V1,y=rep(rr,NROW(dd)),col="red",lty=2)
  legend("bottomright",legend = len[k],col=kol[c],pch = 19,bg = "white");
  c=c+1;
}
title("B) Admixture proportion inference in admixed group with varying admixture proportions", line = -1.5, outer = TRUE,cex=1.3)
dev.off()


# Figure 1: Supplement 4
#Impact of divergence between the ancestral population and reference used in DATES
png(paste(DIR,"results/Figure1_Supplement4.png", sep=""),width = 7, height = 7, units = 'in', res = 700)
par(mfrow=c(2,2))
data=read.table(file = paste(DIR,"data/Figure1_Supplement4", sep=""))
kol=c("darkorange","green3","cyan","violetred1","royalblue1","orangered1","darkorchid1")
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
title("Using divergent references for admixture time inference", line = -1.5, outer = TRUE,cex=1.3)
dev.off()

# Figure 1: Supplement 5
#Impact of divergence between the two source populations that contribute to the target populations
png(paste(DIR,"results/Figure1_Supplement5A.png", sep=""),width = 7, height = 9, units = 'in', res = 700)
par(mfrow=c(3,2))
data=read.table(file = paste(DIR,"data/Figure1_Supplement5A", sep=""))
len1=c("CEU/YRI", "CEU/LWK", "CEU/CHB","CEU/MXL","CEU/TSI")
len2=c("French/Yoruba(0.154)", "French/BantuKenya(0.142)", "French/Tujia(0.110)",
       "French/Maya(0.037)","French/Italian(0.004)")
len=c("YRI","LWK","CHB","MXL","TSI")
for(i in len)
{
  dd=data[data$V1==i,]
  k=which(i==len);
  plot(x=dd$V2,y=dd$V3, col="navy",pch=19,xlab = "True admixture time (gen)",
       ylab = "Estimated admixture time (gen)",ylim = c(0,250),
       main=paste("Ancestrals admixing",len1[k],sep=" ")); grid ();
  segments(x0 =dd$V2 ,x1 = dd$V2, y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col="navy");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3+dd$V4),y1 =(dd$V3+dd$V4),col="navy");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3-dd$V4),y1 =(dd$V3-dd$V4),col="navy");
  lines(x=dd$V2,y=dd$V2,col="darkgrey")
  kol=c("white",dd$V7[1])
  legend("topleft",legend = paste(" References",len2[k],sep="="),
         col="navy",pch = c(19),bg = "white");
}
title("Divergent ancestrals admixing to form target group (n=10)", line = -1, outer = TRUE,cex=1.3)
dev.off()

png(paste(DIR,"results/Figure1_Supplement5B.png", sep=""),width = 7, height = 9, units = 'in', res = 700)
par(mfrow=c(3,2))
data=read.table(file = paste(DIR,"data/Figure1_Supplement5B", sep=""))
par(mfrow=c(3,2))
for(i in len)
{
  dd=data[data$V1==i,]
  k=which(i==len);
  plot(x=dd$V2,y=dd$V3, col="red",pch=16,xlab = "True admixture time (gen)",
       ylab = "Estimated admixture time (gen)",ylim = c(0,250),
       main=paste("Ancestrals admixing",len1[k],sep=" ")); grid ();
  segments(x0 =dd$V2 ,x1 = dd$V2, y0 =(dd$V3+dd$V4),y1 = (dd$V3-dd$V4),col="red");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3+dd$V4),y1 =(dd$V3+dd$V4),col="red");
  segments(x0 =dd$V2-2,x1 = dd$V2+2,y0 =(dd$V3-dd$V4),y1 =(dd$V3-dd$V4),col="red");
  lines(x=dd$V2,y=dd$V2,col="darkgrey")
  
  kol=c("white",dd$V8[1])
  legend("topleft",legend = paste(" References",len2[k],sep="="),
         col="red",pch = c(16),bg = "white");
}
title("Divergent ancestrals admixing to form target group (n=1)", line = -1, outer = TRUE,cex=1.3)
dev.off()

# Figure 1: Supplement 6
#Using admixed population itself as one of the reference groups in DATES
png(paste(DIR,"results/Figure1_Supplement6.png", sep=""),width = 10, height = 10, units = 'in', res = 200)
par(mfrow=c(2,2))
data=read.table(file = paste(DIR,"data/Figure1_Supplement6", sep=""))
len=c(2,4,6,8)
data
par(mfrow=c(2,2))
for(i in len)
{
  dd=data[data$V1==i,]
  k=which(i==len)
  head(dd)
  rr=paste("Admixed pop (as target and reference):",i*10,"%CEU+",(10-i)*10,"%YRI")
  
  plot(x=dd$V2,y=dd$V4, col="deepskyblue",pch=15,xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
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
png(paste(DIR,"results/Figure1_Supplement7A.png", sep=""),width = 9, height = 10, units = 'in', res = 700)
par(mfrow=c(4,3))
data=read.table(file = paste(DIR,"data/Figure1_Supplement7A", sep=""))
kol=c("darkorange","green3","cyan","violetred1","royalblue1","orangered1","darkorchid1",
      "yellowgreen","slateblue2","tomato","palevioletred2","red","orange")
len1=c("missing prop=10%","missing prop=15%","missing prop=20%","missing prop=25%",
       "missing prop=30%","missing prop=35%","missing prop=40%","missing prop=45%",
       "missing prop=50%","missing prop=55%","missing prop=60%")
len=c(10,15,20,25,30,35,40,45,50,55,60)
c=1;
for(i in len)
{
  dd=data[data$V2==i,]
  k=which(i==len);
  dd1=data_alder[data_alder$V2==i,]
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
title("Varying missing genotype proportions in the target genomes", line = -1.5, outer = TRUE,cex=1.3)
dev.off()

# panel B - for 10 target individuals
png(paste(DIR,"results/Figure1_Supplement7B.png", sep=""),width = 8, height = 11, units = 'in', res = 700)
par(mfrow=c(4,3))
data=read.table(file = paste(DIR,"data/Figure1_Supplement7B", sep=""))
data1=read.table(file = paste(DIR,"data/Figure1_Supplement7C", sep=""))
head(data1)
kol=c("darkorange","green3","cyan","violetred1","royalblue1","orangered1","darkorchid1","yellowgreen","slateblue2","tomato","palevioletred2")
len1=c("missing prop=10%","missing prop=15%","missing prop=20%","missing prop=25%",
       "missing prop=30%","missing prop=35%","missing prop=40%","missing prop=45%",
       "missing prop=50%","missing prop=55%","missing prop=60%")
len=c(10,15,20,25,30,35,40,45,50,55,60)
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
title("Varying missing genotype proportions for pseudo-haploid admixed individuals (n=10)", line = -1.5, outer = TRUE,cex=1.3)
dev.off()

# panel C - for 1 target individuals
png(paste(DIR,"results/Figure1_Supplement7C.png", sep=""),width = 8, height = 11, units = 'in', res = 700)
par(mfrow=c(4,3))
kol=c("darkorange","green3","cyan","violetred1","royalblue1","orangered1","darkorchid1","yellowgreen","slateblue2","tomato","palevioletred2")
len1=c("missing_prop=10%","missing_prop=15%","missing_prop=20%","missing_prop=25%","missing_prop=30%","missing_prop=35%","missing_prop=40%","missing_prop=45%","missing_prop=50%","missing_prop=55%","missing_prop=60%")
len=c(10,15,20,25,30,35,40,45,50,55,60)
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
title("Varying missing genotype proportions for pseudo-haploid admixed individuals (n=1)", line = -1.5, outer = TRUE,cex=1.3)
dev.off()


# Figure 1: Supplement 8
#Impact of combination of ancient DNA features. (A) Target 10 individuals (B) Target 1 individual

# A) Target 10 individuals
png(paste(DIR,"results/Figure1_Supplement8A.png", sep=""),width = 8, height = 11, units = 'in', res = 700)
par(mfrow=c(3,2))
data=read.table(file = paste(DIR,"data/Figure1_Supplement8A", sep=""), header = T)
head(data)
str(data)
kol=c("darkorange","darkorchid1","cyan","deeppink","orangered1","darkorchid1","yellowgreen","slateblue2","tomato","palevioletred2")
pp=c(21,22,23,24,25)
len1=c("Reference missing prop=20%","Reference missing prop=40%",
       "Reference missing prop=60%","Reference missing prop=80%")
rlen=c(40,60,80)
len=c(10,20,30,40,50,60)
for(r in len)
{
  c=1;
  rd=data[data$missing_target==r,]
  w=which(r==len);
  
  dd=rd[rd$missing_ref==20,]
  plot(x=dd$admixture_time,y=dd$dates_n10, col=kol[c],pch=pp[c],xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
       main=paste("Missing proportion in the target: ",paste(len[w],"%",sep=""),sep = ""),xlim=c(0,210),
       ylim = c(min(data$dates_n10-data$dates_n10_se),max(data$dates_n10+data$dates_n10_se))); grid ();
  segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$dates_n10+dd$dates_n10_se),y1 = (dd$dates_n10-dd$dates_n10_se),col=kol[c]);
  segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$dates_n10+dd$dates_n10_se),y1 = (dd$dates_n10+dd$dates_n10_se),col=kol[c]);
  segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$dates_n10-dd$dates_n10_se),y1 = (dd$dates_n10-dd$dates_n10_se),col=kol[c]);
  lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
  c=c+1; err=3
  for(i in rlen)
  {
    dd=rd[rd$missing_ref==i,]
    k=which(i==len);
    points(x=dd$admixture_time+err,y=dd$dates_n10, col=kol[c],pch=pp[c],
           ylim = c(min(dd$dates_n10-dd$dates_n10_se),max(dd$dates_n10+dd$dates_n10_se,250))); grid ();
    segments(x0 =dd$admixture_time+err,x1 = dd$admixture_time+err,y0 =(dd$dates_n10+dd$dates_n10_se),y1 = (dd$dates_n10-dd$dates_n10_se),col=kol[c]);
    segments(x0 =dd$admixture_time+err-2,x1 = dd$admixture_time+err+2,y0 =(dd$dates_n10+dd$dates_n10_se),y1 = (dd$dates_n10+dd$dates_n10_se),col=kol[c]);
    segments(x0 =dd$admixture_time+err-2,x1 = dd$admixture_time+err+2,y0 =(dd$dates_n10-dd$dates_n10_se),y1 = (dd$dates_n10-dd$dates_n10_se),col=kol[c]);
    lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
    c=c+1; err=err+5
  }
  legend("topleft",legend = c(len1),col=kol[1:4],pch = pp,bg = "white");
  
}
title("Varying missing genotype proportions in target and references for pseudo-haploid admixed individuals (n=10)", line = -1.5, outer = TRUE,cex=1.3)
dev.off()

#B) Target 1 individual
png(paste(DIR,"results/Figure1_Supplement8B.png", sep=""),width = 8, height = 11, units = 'in', res = 700)
par(mfrow=c(3,2))
for(r in len)
{
  c=1;
  rd=data[data$missing_target==r,]
  w=which(r==len);
  
  dd=rd[rd$missing_ref==20,]
  plot(x=dd$admixture_time,y=dd$dates_n1, col=kol[c],pch=pp[c],xlab = "True admixture time (gen)",ylab = "Estimated admixture time (gen)",
       main=paste("Missing proportion in the target: ",paste(len[w],"%",sep=""),sep = ""),xlim=c(0,210),
       ylim = c(min(data$dates_n1-data$dates_se_n1),max(data$dates_n1+data$dates_se_n1))); grid ();
  segments(x0 =dd$admixture_time,x1 = dd$admixture_time,y0 =(dd$dates_n1+dd$dates_se_n1),y1 = (dd$dates_n1-dd$dates_se_n1),col=kol[c]);
  segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$dates_n1+dd$dates_se_n1),y1 = (dd$dates_n1+dd$dates_se_n1),col=kol[c]);
  segments(x0 =dd$admixture_time-2,x1 = dd$admixture_time+2,y0 =(dd$dates_n1-dd$dates_se_n1),y1 = (dd$dates_n1-dd$dates_se_n1),col=kol[c]);
  lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
  c=c+1; err=3
  for(i in rlen)
  {
    dd=rd[rd$missing_ref==i,]
    k=which(i==len);
    points(x=dd$admixture_time+err,y=dd$dates_n1, col=kol[c],pch=pp[c],
           ylim = c(min(dd$dates_n1-dd$dates_se_n1),max(dd$dates_n10+dd$dates_se_n1,250))); grid ();
    segments(x0 =dd$admixture_time+err,x1 = dd$admixture_time+err,y0 =(dd$dates_n1+dd$dates_se_n1),y1 = (dd$dates_n1-dd$dates_se_n1),col=kol[c]);
    segments(x0 =dd$admixture_time+err-2,x1 = dd$admixture_time+err+2,y0 =(dd$dates_n1+dd$dates_se_n1),y1 = (dd$dates_n1+dd$dates_se_n1),col=kol[c]);
    segments(x0 =dd$admixture_time+err-2,x1 = dd$admixture_time+err+2,y0 =(dd$dates_n1-dd$dates_se_n1),y1 = (dd$dates_n1-dd$dates_se_n1),col=kol[c]);
    lines(x=dd$admixture_time,y=dd$admixture_time,col="darkgrey",lty=2)
    c=c+1; err=err+5
  }
  legend("topleft",legend = c(len1),col=kol[1:4],pch = pp,bg = "white");
  
}
title("Varying missing genotype proportions in target and references for pseudo-haploid admixed individuals (n=1)", line = -1.5, outer = TRUE,cex=1.3)
dev.off()

