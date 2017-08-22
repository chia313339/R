



https://finance.yahoo.com/quote/2891.TW/history?period1=1471795200&period2=1503331200&interval=1d&filter=history&frequency=1d


b<-1503331200/86400
c<-1345564800/86400
a<-as.Date('1970-01-01')

p<-as.Date('2017-08-22')

date<-'2017-08-22'

1503273600

a<-'2017/8/19'


# 實作
# 讀取網頁
url <- read_html('http://news.cnyes.com/news/cat/headline', encoding = 'UTF-8')

# 取得列表 class="rtddt" 下的 a 連結
rtddt <- url %>% html_nodes('._2bF a')
rtddt

link   <- rtddt %>% html_attr('href')

# 建立連結
domain <- 'http://news.cnyes.com'
link   <-  paste(domain, link, sep = '')
detail <- read_html(link[1])


https://query1.finance.yahoo.com/v7/finance/download/APPL?period1=1500733074&period2=1503411474&interval=1d&events=history&crumb=Zx.U1TMwcl/



library(RCurl)
x <- getURL("https://query1.finance.yahoo.com/v7/finance/download/2891.TW?period1=1392984000&period2=1503360000&interval=1d&events=history&crumb=Zx.U1TMwcl/")

out <- read.csv(textConnection(x))

download.file("https://query1.finance.yahoo.com/v7/finance/download/2891.TW?period1=1392984000&period2=1503360000&interval=1d&events=history&crumb=Zx.U1TMwcl/",destfile="reviews.csv",method="libcurl")


url = "https://query1.finance.yahoo.com/v7/finance/download/2891.TW?period1=1392984000&period2=1503360000&interval=1d&events=history&crumb=Zx.U1TMwcl/"
x = read.csv(file=url)




download.file(url, "test.txt")







