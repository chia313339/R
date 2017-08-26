library(car) 
library(rvest)
load("D:/R/STOCK/fcwin.RData")

# �ɶ��ഫ�榡�ܧ� ��"Aug 22, 2017"�i�H�P�_"2017-08-22"
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")

code<-read.table("D:/R/STOCK/code_name.txt",header=T, stringsAsFactors=FALSE)
sdata <- data.frame() 

for(i in 1:nrow(code)){
  print(i)
  y<-try(stock(code[i,1]))
  if (class(y)[1]=="try-error") {next} else {
  y<-cbind(code[i,], y)
  sdata<-rbind(sdata, y)
  }
}

stock_df<-cbind(Code=sdata[,1],Name=sdata[,2],Close=sdata[,3],sdata[,4:7],Sys.Date())

write.table(stock_df,paste0("D:\\R\\STOCK\\sdata.txt"),row.names=F)