library(quantmod) 
library(car) 
load("D:/R/STOCK/fcwin.RData")

code<-as.vector(t(read.table("D:/R/STOCK/name.txt",header=T)[,1]))
sdata<-matrix(0,length(code),5)

for(i in 1:length(code)){
  print(i)
  y<-try(stock(code[i]))
  if (class(y)[1]=="try-error") {next} else {sdata[i,]<-as.vector(t(stock(code[i])))}
}
name<-as.vector(t(read.table("D:/R/STOCK/name.txt",header=T)[,2]))
frame<-data.frame(code,name,sdata,Sys.time())
colnames(frame) <- c("Code","Name","Close","Price","Rsquared","Slope","Sd","Update_time")
write.table(frame,paste0("C:/Users/CHIA/Google ¶³ºÝµwºÐ/STOCK/sdata.txt"),row.names=F)

