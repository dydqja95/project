library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)

setwd('d:/FastCampus/회귀분석 프로젝트/')

rm(list = ls())

# 다음 카페 ----

# 파일 불러오기
data <- readxl::read_excel(path = 'd:/FastCampus/회귀분석 프로젝트/해외_뉴스_블로그.xlsx')
data <- data.frame(data)

# data에 datevector 추가
data <- data %>% 
  dplyr::mutate(day = as.Date(x = (day = paste(data$개봉_년, '.', data$개봉_월, '.', data$개봉_일, sep = '')), format = '%Y.%m.%d'))






future_day <- c()
past_day   <- c()
for(i in 1:nrow(data)){
  future_day <- c(future_day, as.character((data$day[i] + 21)))
  past_day   <- c(past_day,as.character((data$day[i] -30)))
}

data <- data %>% 
  dplyr::mutate(future = future_day,
                past   = past_day)



writexl::write_xlsx(x = data, path = 'd:/FastCampus/회귀분석 프로젝트/임시.xlsx')
# 엑셀에서 날자벡터 삭제

data <- as.data.frame(readxl::read_excel(path = '임시.xlsx'))


View(data)

str(data)

cafe <- c()

for(i in 1:nrow(data)){
  res <- GET(url   = 'https://search.daum.net/search',
             query = list(q           = paste('영화', data$영화명[i]),
                          sd          = paste(data$past[i], '000000', sep = ''),
                          ed          = paste(data$future[i], '235959', sep = ''),
                          period      = 'u',
                          nil_suggest = 'btn',
                          w           = 'cafe',
                          DA          = 'STC',
                          lpp         = 10))
  
  want <- res %>% 
    read_html() %>% 
    html_nodes(css = '#cafeColl > div.coll_tit > div.sub_expander > span') %>% 
    html_text()
  
  
  cafe <- c(cafe, as.character(want))
  print(i)
}


data$cafe <- cafe
str(data)

data$future <- NULL
data$past   <- NULL
data$day <- NULL

data$영화명[1]

writexl::write_xlsx(x = data, path = 'd:/FastCampus/회귀분석 프로젝트/해외_뉴스_블로그_카페.xlsx')



# 다음 뉴스 ----
news <- c()
want <- c()
name <- c()
for(i in 1:nrow(data)){
  res <- GET(url   = 'https://search.daum.net/search',
             query = list(w            = 'news',
                          q            = paste('영화',data$영화명[i]),
                          sd           = paste(data$past_day[i],'000000', sep = ''),
                          ed           = paste(data$future_day[i], '235959', sep = '')))
  
  want <- as.character(res %>% 
                         read_html() %>% 
                         html_nodes(css = '#resultCntArea') %>% 
                         html_text())
  news <- c(news, want)
  print(i)
}


data$news <- news
str(data)

data$future_day <- NULL
data$past_day   <- NULL
data$day        <- NULL


writexl::write_xlsx(x = data, path = 'd:/FastCampus/회귀분석 프로젝트/국내_뉴스_블로그_카페.xlsx')

# 네이버 블로그 ----

blog <- c()

for(i in 1:nrow(data)){
  res <- GET(url   = 'https://search.daum.net/search',
             query = list(q           = paste('영화', data$영화명[i]),
                          sd          = paste(data$past[i], '000000', sep = ''),
                          ed          = paste(data$future[i], '235959', sep = ''),
                          period      = 'u',
                          nil_suggest = 'btn',
                          w           = 'blog',
                          DA          = 'STC'))
  
  want <- res %>% 
    read_html() %>% 
    html_nodes(css = '#blogColl > div.coll_tit > div.sub_expander > span') %>% 
    html_text()
  
  
  blog <- c(blog, as.character(want))
  print(i)
}


data$blog <- blog
str(data)

data$future <- NULL
data$past   <- NULL
data$day <- NULL

data$영화명[1]

writexl::write_xlsx(x = data, path = 'd:/FastCampus/회귀분석 프로젝트/국내_뉴스_블로그.xlsx')