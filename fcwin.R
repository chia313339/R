library(car) 
library(rvest)

# �ɶ��ഫ�榡�ܧ� ��"Aug 22, 2017"�i�H�P�_"2017-08-22"
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")

# ��J�ѻ� �P�_�ثe���氪�C
dec <- function(clo,ss1,ss2,ss3,ss4){
  if(clo<ss1)return("exLow")
  else if(clo>ss1 & clo<ss2) return("Low")
  else if(clo>ss3 & clo<ss4) return("High")
  else if(clo>ss4) return("exHigh")
  else return("Normally")
}

#�إߨ�� ��������r
removekey <-function(s, keys){
  s.split=strsplit(s, keys)
  s =as.character(s.split)
  s
}


# ������� ��J�@�ӥN�X �i�H����Ӿ��v�ѻ�csv��
catch_his <- function(code, backtime=3.5){
  stock_df <- data.frame()
  for(row in c(0,200,400,600,800)){
    s_df <- data.frame() 
    # ����Ѳ��s��
    link <- paste0('https://www.google.com/finance/historical?q=TPE%3A',code,'&startdate=',Sys.Date()-365*backtime,'&enddate=',Sys.Date(),'&num=200&start=',row)
    # ���HTML���e
    url <- read_html(link, encoding = 'UTF-8')
    # ��������줺�e �h��'\n' ��ƼзǤ���榡
    s_date <- url %>% html_nodes('tr') %>% html_nodes('.lm') %>% html_text()
    s_date <- removekey(s_date,'\n')
    # �Ĥ@�Ӥ�����'DATE' �]���q�ĤG�Ӷ}�l��
    s_date <- as.Date(s_date[2:length(s_date)] , format = "%B %d, %Y")
    s_date <- as.data.frame(s_date)
    # ���������줺�e �h��'\n' 
    s_price <- url %>% html_nodes('tr') %>% html_nodes('.rgt') %>% html_text()
    s_price <- removekey(s_price,'\n')
    # �e���Ӥ��������D �]���q�Ĥ��Ӷ}�l��
    s_price_m <- as.data.frame(matrix(s_price[6:length(s_price)], nrow = length(s_price)/5-1, ncol = 5, byrow = TRUE))
    s_df<- cbind(s_date, s_price_m)
    # �̫��X�@�� dataframe
    stock_df<-rbind(stock_df, s_df)
  }
  stock_df
}

# ��J�Ѳ��N�X ���X���v�ѻ� �μҫ��B��
stock<-function(code,backtime=3.5,pro1=95.44,pro2=68.26){
  s1<-(1-pro1/100)/2
  s2<-(1-pro2/100)/2
  s3<-1-s2
  s4<-1-s1
  # ����ѻ����
  st<-catch_his(code)
  # �إߧǦC �è̷Ӯɶ��Ƨ�
  data<-cbind(num=nrow(st):1,st)
  data<-data[order(data[,1]),]
  # factor�ഫ���Ʀr �H�Q�ؼ�
  data[,1]<-as.numeric(as.character(data[,1]))
  data[,6]<-as.numeric(as.character(data[,6]))
  # �إߦ^�W�ҫ�
  m<-lm(data[,6]~num,data)
  mm<-data$num*m$coefficients[2]+m$coefficients[1]
  ss1<-qnorm(s1,mm,summary(m)$s)
  ss2<-qnorm(s2,mm,summary(m)$s)
  ss3<-qnorm(s3,mm,summary(m)$s)
  ss4<-qnorm(s4,mm,summary(m)$s)
  data<-data.frame(data,mm,ss1,ss2,ss3,ss4)
  # �x�s���v�j�����
  write.table(data[,2:12],paste0("C:/R/STOCK/summary/",code,".txt"),col.names=T,row.names=F)
  # ����̷s�@�����
  Close<-tail(data,1)[6]
  # �P�_�ثe�ѻ��Ҧb���C
  Price<-dec(tail(data,1)[6],tail(data,1)[10],tail(data,1)[11],tail(data,1)[12],tail(data,1)[13])
  # �p��ҫ������v
  Rsquared<-summary(m)$r.squared
  # �p��ײv
  Slope<-m$coefficients[2]
  # �p��зǮt
  Sd<-summary(m)$s
  frame<-data.frame(Close,Price,Rsquared,Slope,Sd)
  return(frame)
}



save.image("D:\\R\\STOCK\\fcwin.RData")    
 