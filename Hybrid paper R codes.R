#R codes for Hybrid paper

require("ggplot2")

#reading data
data=read.table("./Test_data/200911-bMF1-ATPgS1mM-10kfps_C001H001S0008_36_0-50000-forcp.txt",header=F)
colnames(data)=c("frame","x","y","angle","accum","revolutions")

#xyplot for center drifting#
n=dim(data)[1]   #sample size
l=n/4     #size of sections compared
plotxy_=data[c(1:l,(l*((n/l)-1)):(l*(n/l))),c(1:3)]
cl=matrix()
cl[1:l]="b"
cl[(l+1):(2*l+1)]="e"
plotxy_=data.frame(plotxy_,cl)
ggplot(plotxy_ ,aes(x=x,y=y,color=cl))+ geom_point(show.legend = FALSE,size=0.9)+ theme_bw()+scale_color_manual(values = c("b" = "#56B4E9", "e" = "red"))+ggtitle("XY plot of different time intervals")


#for some data, frame does not start with 1. Therefore, we subtract minimum
data$frame=data$frame-min(data$frame)

#Change Points
cp=read.table("./Test_data/200911-bMF1-ATPgS1mM-10kfps_C001H001S0008_36_0-50000-forcp-cp.txt",header=F)

#Trace with change points
#trace of whole data
ggplot(data,aes(frame,revolutions))+ geom_line(color="#56B4E9")+ theme_bw()+xlab("Time steps")+ylab("Revolutions")

#Trace as segments (frame and revolutions) with CPs as red dashed lines
#j=1  #from 0:29 (divided to 30 sections)
for(j in 0:29){
  dt=data[(1+floor(dim(data)[1]/30)*j):(floor(dim(data)[1]/30)*(j+1)),]
  print(ggplot(dt,aes(frame,revolutions))+ geom_line(color="#56B4E9")+ theme_bw()+xlab("Time steps")+ylab("Revolutions")+
          geom_vline(xintercept = cp$V2, size=0.35,color="red",lty=2)+xlim(data$frame[1+floor(dim(data)[1]/30)*j],data$frame[floor(dim(data)[1]/30)*(j+1)]))
}


##Angle histogram (one count for each CP interval)#
mtemp=vector("numeric")
for (i in 1:(dim(cp)[1]-1)) {
  dtemp=vector("numeric")
  dtemp=data$accum[cp$V1[i]:(cp$V1[i+1]-1)]  #observations in each CP interval
  mtemp[i]=((mean(dtemp))%%360)  #mean angle of each CP interval
}
#plotting histogram 
ggplot(as.data.frame(mtemp), aes(x=mtemp)) + geom_histogram(binwidth=3, fill="darkorange1")+
  scale_x_continuous(breaks=seq(0, 360, 120))+theme_bw()+ylab("Occurence")+ xlab("Angle")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) 




#Cleaned-up Change Points
cleaned_cp=read.csv("./Test_data/200911-bMF1-ATPgS1mM-10kfps_C001H001S0008_36cleancp.csv",header=F)

##Angle histogram (one count for each CP interval)  (after clean-up)#
mtemp=vector("numeric")
dwell=vector("numeric")
for (i in 1:(dim(cleaned_cp)[1]-1)) {
  dtemp=vector("numeric")
  dtemp=data$accum[cleaned_cp$V1[i]:(cleaned_cp$V1[i+1]-1)] #observations in each CP interval (after clean-up)
  mtemp[i]=((mean(dtemp))%%360) #mean angle of each CP interval (after clean-up)
  dwell[i]=length(dtemp)     #length of CP Interval
}
ggplot(as.data.frame(mtemp), aes(x=mtemp)) + geom_histogram(binwidth=3, fill="darkorange1")+
  scale_x_continuous(breaks=seq(0, 360, 60))+theme_bw()+ylab("Occurence")+ xlab("Angle")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) 



#Further clean-up to sharpen the peaks
#removing CP intervals with length shorter than 7 points
cpn=cleaned_cp$V1[-(which(dwell<7)+1)]
##Angle histogram (one count for each CP interval)#
mtemp=vector("numeric")
medtemp=vector("numeric")
dwell=vector("numeric")
for (i in 1:(length(cpn)-1)) {
  dtemp=vector("numeric")
  dtemp=data$accum[cpn[i]:(cpn[i+1]-1)]  #CP Interval
  mtemp[i]=median(dtemp%%360)   #median of CPInt, angle
  dwell[i]=length(dtemp)     #length of CPInt
  medtemp[i]=median(dtemp)   #median of CPInt as cumulative angle to use in the next step
}
ggplot(as.data.frame(mtemp), aes(x=mtemp)) + geom_histogram(binwidth=3, fill="darkorange1")+
  scale_x_continuous(breaks=seq(0, 360, 120),limits=c(0,360))+theme_bw()+ylab("Count")+ xlab("Angle")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) 


#comparing the median angles of each CP interval to its previous and next CP intervals 
#If the difference between them was smaller than 40 and the medians do not progressively increase, the CP interval was removed
rmv=vector("numeric")
for (i in 2:(length(medtemp)-1)) {
  if((medtemp[i+1]-medtemp[i-1])<40){  #checking the difference between consecutive intervals
    rmv[i]=i
    rmv[i+1]=i+1}
}
rmv=rmv[!is.na(rmv)]  #checking if undesired intervals exist
cpn2=cpn[-rmv]  #undesired intervals removed
##Angle histogram (one count for each CP interval)#
mtemp=vector("numeric")
dwell=vector("numeric")
for (i in 1:(length(cpn2)-1)) {
  dtemp=vector("numeric")
  dtemp=data$accum[cpn2[i]:(cpn2[i+1]-1)]  #CP Int
  mtemp[i]=median(dtemp%%360)   #median of CPInt, angle
  dwell[i]=length(dtemp)     #length of CPInt
}
#final histogram of CP intervals
ggplot(as.data.frame(mtemp), aes(x=mtemp)) + geom_histogram(binwidth=3, fill="darkorange1")+
  scale_x_continuous(breaks=seq(0, 360, 120),limits=c(0,360))+theme_bw()+ylab("Count")+ xlab("Angle")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) 


############################################
#Angular distance ratios of the current test data

#divisions are decided according to histogram of cleaned-up CP intervals
div=c(75,130,200,250,310,355) 

#angles of divisions in the whole data
div_=rep((min(data$accum)%/%360):(max(data$accum)%/%360),each=6)*360+div
for (i in 1:length(div_)) {
  if(div_[i]<min(data$accum) || div_[i]>max(data$accum)){
    div_[i]=NA}
}
div_=div_[complete.cases(div_)]

#creating a matrix for the median of each divided section
med_peak=matrix(,length(div_)-1,4)
for (i in 1:(length(div_)-1)) {
  med_peak[i,1]=median(data[data$accum>div_[i] & data$accum<div_[i+1],4]) #median
  med_peak[i,2]=length(data[data$accum>div_[i] & data$accum<div_[i+1],4]) #length
}
n=length(med_peak[,1])
#labeling the sections according to steps 1-6
labl=rep(1:6,ceiling(n/6))
med_peak[1,3]=6 #decided by looking at the median angles
med_peak[2:n,3]=labl[1:(n-1)]

#median of CP interval corresponding to the section of the angles
for (i in 1:(length(medtemp)-1)) {
  med_peak[sum(div_<medtemp[i]),4]=mtemp[i]
}

#angular distance ratios for the test data
ratios_=vector("numeric")
if(med_peak[1,3]%%2==0)
  #if first short pause
  for (i in seq(3,(n-1),2)) {
    if(!is.na(med_peak[i,3]))
      ratios_[i/2]=abs((med_peak[i,4]-med_peak[i-1,4]+ 180) %% 360 - 180)/abs((med_peak[i+1,4]-med_peak[i,4]+ 180) %% 360 - 180)
    #ratios_[i/2]=distances[i]/distances[i+1]
  }
if(med_peak[1,3]%%2==1)
  #if second short pause
  for (i in seq(2,(n-1),2)) {
    if(!is.na(med_peak[i,3]))
      ratios_[i/2]=abs((med_peak[i,4]-med_peak[i-1,4]+ 180) %% 360 - 180)/abs((med_peak[i+1,4]-med_peak[i,4]+ 180) %% 360 - 180)
    #ratios_[i/2]=distances[i]/distances[i+1]
  }



###########################################################################
#All angular distance ratios can be loaded from the folder "ratios_all.txt"
ratios_=read.table("./Test_data/ratios_all.txt")
colnames(ratios_)=c("ang_diff_ratio","type")
ratios_bmf=ratios_[which(ratios_[,2]=="bMF1"),]
ratios_bmfabt=ratios_[which(ratios_[,2]=="bMF1(abT)"),]
ratios_bmfbetat=ratios_[which(ratios_[,2]=="bMF1(betaT)"),]
ratios_bmfbgt=ratios_[which(ratios_[,2]=="bMF1(bgT)"),]
ratios_bmfgt=ratios_[which(ratios_[,2]=="bMF1(gT)"),]
ratios_tf=ratios_[which(ratios_[,2]=="TF1"),]

#removing outliers
ratios_bmf=ratios_bmf[-which(ratios_bmf[,1] %in% c(boxplot.stats(ratios_bmf[,1])$out)),]
ratios_bmfabt=ratios_bmfabt[-which(ratios_bmfabt[,1] %in% c(boxplot.stats(ratios_bmfabt[,1])$out)),]
ratios_bmfbetat=ratios_bmfbetat[-which(ratios_bmfbetat[,1] %in% c(boxplot.stats(ratios_bmfbetat[,1])$out)),]
ratios_bmfbgt=ratios_bmfbgt[-which(ratios_bmfbgt[,1] %in% c(boxplot.stats(ratios_bmfbgt[,1])$out)),]
ratios_bmfgt=ratios_bmfgt[-which(ratios_bmfgt[,1] %in% c(boxplot.stats(ratios_bmfgt[,1])$out)),]
ratios_tf=ratios_tf[-which(ratios_tf[,1] %in% c(boxplot.stats(ratios_tf[,1])$out)),]

#function to find error bar
error_bar=function(ratio_){
  upper_=mean(ratio_)+sd(ratio_)/sqrt(length(ratio_))
  lower_=mean(ratio_)-sd(ratio_)/sqrt(length(ratio_))
  err_list=list("lower"=lower_,"upper"=upper_)
  return(err_list)
}

#error bar for each hybrid
error_bmf=error_bar(ratios_bmf[,1])
error_tf=error_bar(ratios_tf[,1])
error_bmfabt=error_bar(ratios_bmfabt[,1])
error_bmfbetat=error_bar(ratios_bmfbetat[,1])
error_bmfgt=error_bar(ratios_bmfgt[,1])
error_bmfbgt=error_bar(ratios_bmfbgt[,1])
error_data=data.frame("lower_line"=c(error_bmfbgt$lower,error_tf$lower,error_bmfgt$lower,error_bmfabt$lower,error_bmf$lower,error_bmfbetat$lower),
                      "upper_line"=c(error_bmfbgt$upper,error_tf$upper,error_bmfgt$upper,error_bmfabt$upper,error_bmf$upper,error_bmfbetat$upper),
                      "mean"=c(mean(ratios_bmfbgt[,1]),mean(ratios_tf[,1]),mean(ratios_bmfgt[,1]),mean(ratios_bmfabt[,1]),mean(ratios_bmf[,1]),mean(ratios_bmfbetat[,1])),
                      "type"=c("bMF1(bgT)","TF1","bMF1(gT)","bMF1(abT)","bMF1","bMF1(betaT)"))
error_data$type <- factor(error_data$type , levels=c("bMF1(bgT)","TF1","bMF1(gT)","bMF1(abT)","bMF1","bMF1(betaT)"))

#Angular distnace ratio box-plot by each hybrid
ggplot(error_data, aes(x=as.factor(type), y=mean),show.legend = FALSE) +  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_point(aes(x=type,y=mean),show.legend = FALSE,pch=3,size=1.5,stroke=2)+  geom_point(aes(x=type,y=upper_line),show.legend = FALSE)+theme_bw()+  
  geom_point(aes(x=type,y=lower_line),show.legend = FALSE)+  geom_segment(aes(x=type, y=lower_line,xend=type,yend=upper_line),size=2,show.legend = FALSE,lineend = "square")+
  theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) 


