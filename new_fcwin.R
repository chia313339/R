library(car) 
library(rvest)

# 時間轉換格式變更 讓"Aug 22, 2017"可以判斷"2017-08-22"
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")

# 輸入股價 判斷目前價格高低
dec <- function(clo,ss1,ss2,ss3,ss4){
  if(clo<ss1)return("exLow")
  else if(clo>ss1 & clo<ss2) return("Low")
  else if(clo>ss3 & clo<ss4) return("High")
  else if(clo>ss4) return("exHigh")
  else return("Normally")
}

#建立函數 移除關鍵字
removekey <-function(s, keys){
  s.split=strsplit(s, keys)
  s =as.character(s.split)
  s
}


# 爬網函數 輸入一個代碼 可以抓取該歷史股價csv檔
catch_his <- function(code, backtime=3.5){
  stock_df <- data.frame()
  for(row in c(0,200,400,600,800)){
    s_df <- data.frame() 
    # 抓取股票連結
    link <- paste0('https://www.google.com/finance/historical?q=TPE%3A',code,'&startdate=',Sys.Date()-365*backtime,'&enddate=',Sys.Date(),'&num=200&start=',row)
    # 抓取HTML內容
    url <- read_html(link, encoding = 'UTF-8')
    # 抓取日期欄位內容 去除'\n' 轉化標準日期格式
    s_date <- url %>% html_nodes('tr') %>% html_nodes('.lm') %>% html_text()
    s_date <- removekey(s_date,'\n')
    # 第一個元素為'DATE' 因此從第二個開始抓
    s_date <- as.Date(s_date[2:length(s_date)] , format = "%B %d, %Y")
    s_date <- as.data.frame(s_date)
    # 抓取價格欄位內容 去除'\n' 
    s_price <- url %>% html_nodes('tr') %>% html_nodes('.rgt') %>% html_text()
    s_price <- removekey(s_price,'\n')
    # 前五個元素為標題 因此從第六個開始抓
    s_price_m <- as.data.frame(matrix(s_price[6:length(s_price)], nrow = length(s_price)/5-1, ncol = 5, byrow = TRUE))
    s_df<- cbind(s_date, s_price_m)
    # 最後整合一個 dataframe
    stock_df<-rbind(stock_df, s_df)
  }
  stock_df
}

# 輸入股票代碼 產出歷史股價 及模型運算
stock<-function(code,backtime=3.5,pro1=95.44,pro2=68.26){
  s1<-(1-pro1/100)/2
  s2<-(1-pro2/100)/2
  s3<-1-s2
  s4<-1-s1
  # 抓取股價資料
  st<-catch_his(code)
  # 建立序列 並依照時間排序
  data<-cbind(num=nrow(st):1,st)
  data<-data[order(data[,1]),]
  # factor轉換成數字 以利建模
  data[,1]<-as.numeric(as.character(data[,1]))
  data[,6]<-as.numeric(as.character(data[,6]))
  # 建立回規模型
  m<-lm(data[,6]~num,data)
  mm<-data$num*m$coefficients[2]+m$coefficients[1]
  ss1<-qnorm(s1,mm,summary(m)$s)
  ss2<-qnorm(s2,mm,summary(m)$s)
  ss3<-qnorm(s3,mm,summary(m)$s)
  ss4<-qnorm(s4,mm,summary(m)$s)
  data<-data.frame(data,mm,ss1,ss2,ss3,ss4)
  # 儲存歷史古假資料
  write.table(data[,2:12],paste0("C:/STOCK/summary/",code,".txt"),col.names=T,row.names=F)
  # 抓取最新一筆資料
  Close<-tail(data,1)[6]
  # 判斷目前股價所在高低
  Price<-dec(tail(data,1)[6],tail(data,1)[10],tail(data,1)[11],tail(data,1)[12],tail(data,1)[13])
  # 計算模型解釋率
  Rsquared<-summary(m)$r.squared
  # 計算斜率
  Slope<-m$coefficients[2]
  # 計算標準差
  Sd<-summary(m)$s
  frame<-data.frame(Close,Price,Rsquared,Slope,Sd)
  return(frame)
}

save(dec,stock, file = "D:/R/STOCK/fcwin.RData")     

