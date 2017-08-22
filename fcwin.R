library(quantmod) 
library(car) 

dec <- function(clo,ss1,ss2,ss3,ss4){
  if(clo<ss1)return("exLow")
  else if(clo>ss1 & clo<ss2) return("Low")
  else if(clo>ss3 & clo<ss4) return("High")
  else if(clo>ss4) return("exHigh")
  else return("Normally")
}


stock<-function(code,backtime=3.5,pro1=95.44,pro2=68.26){
  s1<-(1-pro1/100)/2
  s2<-(1-pro2/100)/2
  s3<-1-s2
  s4<-1-s1
  st<-getSymbols(code,auto.assign=FALSE,from=Sys.Date()-365*backtime)
  data<-data.frame(num=seq(1,nrow(st),1),date<-row.names(as.matrix(st)),st)
  m<-lm(data[,6]~num,data)
  mm<-data$num*m$coefficients[2]+m$coefficients[1]
  ss1<-qnorm(s1,mm,summary(m)$s)
  ss2<-qnorm(s2,mm,summary(m)$s)
  ss3<-qnorm(s3,mm,summary(m)$s)
  ss4<-qnorm(s4,mm,summary(m)$s)
  data<-data.frame(data,mm,ss1,ss2,ss3,ss4)
  write.table(data[,2:13],paste0("C:/Users/CHIA/Google ¶³ºÝµwºÐ/STOCK/summary/",code,".txt"),col.names=T,row.names=F)#¤£­nrow
  Close<-tail(data,1)[6]
  Price<-dec(tail(data,1)[6],tail(data,1)[10],tail(data,1)[11],tail(data,1)[12],tail(data,1)[13])
  Rsquared<-summary(m)$r.squared
  Slope<-m$coefficients[2]
  Sd<-summary(m)$s
  frame<-data.frame(Close,Price,Rsquared,Slope,Sd)
  return(frame)
}

save(dec,stock, file = "D:/R/STOCK/fcwin.RData")     
     
     
