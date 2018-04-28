rm(list=ls(all=T))
setwd("E:\Documents\Practice\PW")

library(dplyr)
library(data.table)

library(quantmod)

stocks <- function(ticker,from_date,to_date){
  getSymbols(ticker,src="FRED")
  
  stock_data <- as.data.frame(eval(parse(text = ticker)))
  
  stock_data$date <- rownames(stock_data)
  
  stock_data$date <- as.Date(stock_data$date)
  
  colnames(stock_data)[1] <- "price"
  
  stock_data_1 <- stock_data %>% filter(between(date,from_date,to_date)) %>%
    filter(!is.na(price)) %>% mutate(return = price - lag(price)) %>% filter(!is.na(return))
}

stock_data_1 <- stocks('CPIAUCNS','2010-01-01','2017-12-31')

plot(y=stock_data_1$return,x=stock_data_1$date,type='l')

mean(stock_data_1$return)

sd(stock_data_1$return)

plot(density(stock_data_1$return))

qqnorm(stock_data_1$return,datax = T)
qqline(stock_data_1$return)
