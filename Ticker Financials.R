
library(quantmod)
library(plyr)
library(sqldf)
library(tidyverse)


r <- lapply(c("NYSE", "AMEX", "NASDAQ"), function(exch) {
  URL <- sprintf("https://api.nasdaq.com/api/screener/stocks?tableonly=true&limit=25&offset=0&exchange=%s&download=true", exch)
  d <- jsonlite::fromJSON(URL)$data$rows
  d$Exchange <- exch
  return(d)
})

symbols3 <- rbind(r[[1]], r[[2]], r[[3]])
symbols3 <- as.data.frame.matrix(symbols3)
symbols3$lastsale <- substring(symbols3$lastsale, 2)
symbols3$lastsale <- round(as.numeric(symbols3$lastsale),2)


symbolstech <- sqldf("select symbol from (select symbol, lastsale, volume*lastsale as atrisk, marketCap, exchange FROM (select symbol, volume, marketCap, lastsale, exchange from symbols3 where lastsale > 15 and sector IS NOT NULL
                     order by marketCap desc) LIMIT 1000) order by atrisk desc LIMIT 500")
symbols <- symbolstech[,1]

metrics <- yahooQF(c("Average Days Volume","Days High", "Days Low", "P/E Ratio","Earnings/Share","Price/Book","Dividend Yield","Dividend/Share"))

Target_Price_1yr <- getQuote(symbols, what=metrics)
index <- is.na(Target_Price_1yr)
Target_Price_1yr[index] <- 0
colnames(Target_Price_1yr) <- gsub(" ","",colnames(Target_Price_1yr))
colnames(Target_Price_1yr) <- gsub("/","",colnames(Target_Price_1yr))

Target_Price_1yr <- rownames_to_column(Target_Price_1yr, "Ticker")

Financials <- sqldf("select *, round(DividendShare/EarningsShare,2)*100 as DPR from Target_Price_1yr where PERatio > 0")

