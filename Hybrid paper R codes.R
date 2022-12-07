#R codes for Hybrid paper

require("ggplot2")

#reading data
data=read.table("./Test_data/200927-TF1-ATPgS1mM-10kfps_C001H001S0001_25_98000-140000-forcp.txt",header=F)
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


#for some data, frame doesnot start with 1. Therefore, we subtract minimum
data$frame=data$frame-min(data$frame)

#Change Points
cp=read.table("./Test_data/200927-TF1-ATPgS1mM-10kfps_C001H001S0001_25_98000-140000-forcp-cp.txt",header=F)

#Trace with change points
#trace of whole data
ggplot(data,aes(frame,revolutions))+ geom_line(color="#56B4E9")+ theme_bw()+xlab("Time steps")+ylab("Revolutions")
#trace with CPs
ggplot(data,aes(frame,revolutions))+ geom_line(color="#56B4E9")+ theme_bw()+xlab("Time steps")+ylab("Revolutions")+geom_vline(xintercept = cp$V2, size=0.35,color="red",lty=2)

#Trace as segments (frame and revolutions) with CPs as red dashed lines
#j=1  #from 0:29
for(j in 0:29){
  dt=data[(1+floor(dim(data)[1]/30)*j):(floor(dim(data)[1]/30)*(j+1)),]
  print(ggplot(dt,aes(frame,revolutions))+ geom_line(color="#56B4E9")+ theme_bw()+xlab("Time steps")+ylab("Revolutions")+
          geom_vline(xintercept = cp$V2, size=0.35,color="red",lty=2)+xlim(data$frame[1+floor(dim(data)[1]/30)*j],data$frame[floor(dim(data)[1]/30)*(j+1)]))
}


##Angle histogram (one count for each CP interval)#
mtemp=vector("numeric")
for (i in 1:(dim(cp)[1]-1)) {
  dtemp=vector("numeric")
  dtemp=data$accum[cp$V1[i]:(cp$V1[i+1]-1)]
  mtemp[i]=((mean(dtemp))%%360)
}

ggplot(as.data.frame(mtemp), aes(x=mtemp)) + geom_histogram(binwidth=3, fill="darkorange1")+
  ggtitle("Angle histogram (one count for each CP interval)")+  scale_x_continuous(breaks=seq(0, 360, 60))+theme_bw()+ylab("Occurence")+ xlab("Angle")








#Cleaned-up Change Points
cleaned_cp=read.csv("./Test_data/200927-TF1-ATPgS1mM-10kfps_C001H001S0001_25cleancp.csv",header=F)

##Angle histogram (one count for each CP interval)#
mtemp=vector("numeric")
for (i in 1:(dim(cleaned_cp)[1]-1)) {
  dtemp=vector("numeric")
  dtemp=data$accum[cleaned_cp$V1[i]:(cleaned_cp$V1[i+1]-1)]
  mtemp[i]=((mean(dtemp))%%360)
}

ggplot(as.data.frame(mtemp), aes(x=mtemp)) + geom_histogram(binwidth=3, fill="darkorange1")+
  ggtitle("Angle histogram (one count for each CP interval)")+  scale_x_continuous(breaks=seq(0, 360, 60))+theme_bw()+ylab("Occurence")+ xlab("Angle")



