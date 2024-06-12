setwd("C:/Users/sikul/OneDrive/PPE content/bachelor thesis/R directory 2")
library(quanteda)
require(quanteda.textstats)
library(readtext)
require(quanteda.textplots)
require(quanteda.textmodels)
library(dplyr)
library(stringr)
topic<-"selbst"
patterns <- "wahlrecht|wahlrechtsreform|überhangmandat|direktmandat"

faz_raw<-read.csv(paste0("newspaper articles/",topic,"/faz_google_",topic,".csv"))
faz_raw$date <- as.Date(faz_raw$date, format="%Y-%m-%dT%H:%M:%OS")
filtered_faz_raw <- faz_raw %>%
  filter(date >= as.Date("2023-01-01") & date <= as.Date("2023-05-31"))
filtered_faz_raw<-filtered_faz_raw[,c("title","url")]
filtered_faz_raw$title<-tolower(filtered_faz_raw$title)
filtered_faz_raw$title <- str_replace_all(filtered_faz_raw$title, "[-_]", " ")
filtered_faz<-filtered_faz_raw %>%
  filter(str_detect(title, patterns))%>%
  distinct(url, .keep_all = TRUE)
faz_string<-paste0('["', paste(filtered_faz$url, collapse = '","'), '"]')
cat(faz_string)


sz_raw<-read.csv(paste0("newspaper articles/",topic,"/sz_google_",topic,".csv"))
sz_raw$date <- as.Date(sz_raw$date, format="%Y-%m-%dT%H:%M:%OS")
filtered_sz_raw <- sz_raw %>%
  filter(date >= as.Date("2023-01-01") & date <= as.Date("2023-05-31"))
filtered_sz_raw<-filtered_sz_raw[,c("title","url")]
filtered_sz_raw$title<-tolower(filtered_sz_raw$title)
filtered_sz<-filtered_sz_raw %>%
  filter(str_detect(title, patterns))%>%
  distinct(url, .keep_all = TRUE)
sz_string<-paste0('["', paste(filtered_sz$url, collapse = '","'), '"]')
cat(sz_string)

zeit_raw<-read.csv(paste0("newspaper articles/",topic,"/zeit_",topic,".csv"))
colnames(zeit_raw)[3]<-"title"
colnames(zeit_raw)[4]<-"url"
filtered_zeit_raw<-zeit_raw[,c("title","url")]
filtered_zeit_raw$title<-tolower(filtered_zeit_raw$title)
filtered_zeit<-filtered_zeit_raw %>%
  filter(str_detect(title, patterns))%>%
  distinct(url, .keep_all = TRUE)
zeit_string<-paste0('["', paste(filtered_zeit$url, collapse = '","'), '"]')
cat(zeit_string)



nd_raw<-read.csv(paste0("newspaper articles/",topic,"/nd_",topic,".csv"),sep=",")
colnames(nd_raw)[5]<-"title"
colnames(nd_raw)[6]<-"url"
nd_raw$title<-paste(nd_raw$title,nd_raw$topic,sep=" ")
filtered_nd_raw<-nd_raw[,c("title","url")]
filtered_nd_raw$title<-tolower(filtered_nd_raw$title)
filtered_nd<-filtered_nd_raw %>%
  filter(str_detect(title, patterns))%>%
  distinct(url, .keep_all = TRUE)
nd_string<-paste0('["', paste(filtered_nd$url, collapse = '","'), '"]')
cat(nd_string)


tichy_raw<-read.csv(paste0("newspaper articles/",topic,"/tichy_",topic,".csv"),sep=",")
colnames(tichy_raw)[3]<-"title"
colnames(tichy_raw)[4]<-"url"
tichy_raw$title<-paste(tichy_raw$title,tichy_raw$topic,sep=" ")
Sys.setlocale("LC_TIME", "de_DE.UTF-8")
tichy_raw$date <- as.Date(tichy_raw$date, format="%d. %B %Y")
filtered_tichy_raw <- tichy_raw %>%
  filter(date >= as.Date("2023-01-01") & date <= as.Date("2023-05-31"))
filtered_tichy_raw<-filtered_tichy_raw[,c("title","url")]
filtered_tichy_raw$title<-tolower(filtered_tichy_raw$title)
filtered_tichy<-filtered_tichy_raw %>%
  filter(str_detect(title, patterns))%>%
  distinct(url, .keep_all = TRUE)
tichy_string <- paste0('["', paste(filtered_tichy$url, collapse = '","'), '"]')
cat(tichy_string)

topic<-"atom"

# import downloaded articles
sz_down<-read.csv(paste0("newspaper articles/",topic,"/sz_",topic,"_down.csv"))
faz_down<-read.csv(paste0("newspaper articles/",topic,"/faz_",topic,"_down.csv"))
zeit_down<-read.csv(paste0("newspaper articles/",topic,"/zeit_",topic,"_down.csv"))
tichy_down<-read.csv(paste0("newspaper articles/",topic,"/tichy_",topic,"_down.csv"))
nd_down<-read.csv(paste0("newspaper articles/",topic,"/nd_",topic,"_down.csv"))

length(rownames(nd_down))
length(rownames(sz_down))
length(rownames(zeit_down))
length(rownames(faz_down))
length(rownames(tichy_down))



sz_text <- paste(unlist(sz_down[, c("text1", "text2", "text3", "text4", "text5", "text6")]), collapse = " ")
faz_text <- paste(unlist(faz_down[, c("text1", "text2", "text3", "text4", "text5", "text6")]), collapse = " ")
zeit_text <- paste(unlist(zeit_down[, c("text1", "text2", "text3", "text4", "text5")]), collapse = " ")
tichy_text <- paste(unlist(tichy_down[, c("text1", "text2", "text3", "text4")]), collapse = " ")
nd_text <- paste(unlist(nd_down[, c("text1", "text2", "text3", "text4", "text5")]), collapse = " ")

df <- data.frame(
  docs = c("sz", "faz", "zeit", "tichy", "nd"),
  text = c(sz_text, faz_text, zeit_text, tichy_text, nd_text)
)

save(df,file=paste0("dfs/",topic,"_news_df.RData"))
