library(rvest)

page<-c("00","01","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","64","65","66","68","74","80","81","82","83","84","89","91","98","99")

catch_code <- function(page){
  # 抓取代碼連結
  link <- paste0('https://goodinfo.tw/StockInfo/StockList.asp?MARKET_CAT=全部&STOCK_CODE=',page)
  # 抓取HTML內容
  url <- read_html(link, encoding = 'UTF-8')
  # 抓取股票代碼內容 
  code<-url %>% html_nodes('a.link_black') %>% html_text()
  code<-code[(max(which(as.vector(code) == '  '))+1):length(code)]
  # 將股票代碼及中文整理成矩陣 並去除不要的欄位資訊
  code2<- as.data.frame(matrix(code, nrow = length(code), ncol = 2, byrow = TRUE))
  ind <- which(with( code2, V1=="代號"  ))
  code2<-code2[-ind, ]
  code2
}

stock_code<- data.frame()
for(p in page){
  code_tmp<-catch_code(p)
  stock_code<-rbind(stock_code, code_tmp)
  print(p)
  Sys.sleep(3)
}
stock_code

write.table(stock_code,paste0("D:/R/STOCK/name.txt"),row.names=F)
