
library(corrplot)
library(data.table)
library(RColorBrewer)
library(sqldf)
library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)
library(lares)


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

dataset<- xts() # Only run once

# cool progress bar to see the % of completion
n <- length(symbols)
pb <- txtProgressBar(min = 0, max = n, style=3)


# Actual loop: 
for(i in 1:length(symbols)) {
  symbols[i]-> symbol
  # specify the "from" date to desired start date
  tryit <- try(getSymbols(symbol,from="2014-01-01", src='yahoo'))
  if(inherits(tryit, "try-error")){
    i <- i+1
  } else {
    # specify the "from" date to desired start date
    data <- getSymbols(symbol, from="2014-01-01", src='yahoo')
    dataset <- merge(dataset, diff(log(Cl(get(symbols[i])))))
    rm(symbol)
  }
  setTxtProgressBar(pb, i)
}

dataset <- as.data.table(x = dataset)
dataset <- select(dataset, -1)
dataset = dataset[-1,]
DT <- as.data.frame.matrix(dataset)
index <- is.na(DT)
DT[index] <- 0
colnames(DT) <- gsub(".Close","",colnames(DT))
    
    cor.mtest <- function(mat, ...) {
      mat <- as.matrix(mat)
      n <- ncol(mat)
      p.mat<- matrix(NA, n, n)
      diag(p.mat) <- 0
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          tmp <- cor.test(mat[, i], mat[, j], ...)
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
      }
      colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
      p.mat
    }
    # matrix of the p-value of the correlation
    p.mat <- cor.mtest(DT)
    head(p.mat[, 1:5])
    
    corplot <- cor(DT)
    corplot <- as.data.frame.matrix(corplot)
    corplot2 <- apply(corplot, 2, function(x) ifelse(x > .1, '', x))
   
    
    corr_cross(DT, top=50, max_pvalue = .01)
    
    

    
        