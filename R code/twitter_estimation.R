setwd("C:/Users/sikul/OneDrive/PPE content/bachelor thesis/R directory 2")
library(quanteda)
require(quanteda.textstats)
library(readtext)
require(quanteda.textplots)
require(quanteda.textmodels)
require(ggplot2)
require(ca)
library(dplyr)
library(lubridate)

topic<-"cann"
topic2<-"Cannabis"
register_df<-read.csv("EPINetz_TwitterPoliticians_2023.csv",sep=";")
query_results_df<-read.csv(paste0("query results/",topic,"_query_results.csv"),sep=";")

query_results_df1<-query_results_df[c("author.userName", "text","createdAt")]
#filtered dates
#query_results_df1$createdAt <- as.POSIXct(query_results_df1$createdAt, format="%a %b %d %H:%M:%S %z %Y", tz="UTC")
#cutoff_date <- as.POSIXct("2023-10-01", tz="UTC")
#filtered_df <- query_results_df1 %>%   filter(createdAt >= cutoff_date)
#rows_filtered_out <- nrow(query_results_df1) - nrow(filtered_df) 
#filtered_df2<-filtered_df[c("author.userName", "text")]

#merge by party
query_results_df2 <- merge(query_results_df1, register_df[, c("twitter_handle", "party")],by.x = "author.userName", by.y = "twitter_handle", all.x = TRUE)
query_results_df2_unique <- distinct(query_results_df2,text, .keep_all = TRUE)
#print(query_results_df2$text[96])

# Filter out rows where party is'no affiliation'
query_results_df3 <- query_results_df2_unique[!query_results_df2_unique$party %in% "no party affiliation", ]
query_results_df4 <- query_results_df3[!query_results_df3$author.userName %in% c("ali_aldailami","SevimDagdelen","ernst_klaus","AndrejHunko","christian_leye","Amira_M_Ali","ZaklinNastic","jessica_tatti","SWagenknecht"), ]


# Create a new data frame where each row is one party and all texts are concatenated into one character string
party_text_df <- aggregate(text ~ party, data=query_results_df4, FUN=function(x) paste(x, collapse=" "))
#merge CDU and CSU row into one CDU.CSU row
cdu_csu_text <- paste(party_text_df$text[party_text_df$party %in% c("CDU", "CSU")], collapse=" ")
new_row <- data.frame(party = "CDU.CSU", text = cdu_csu_text, stringsAsFactors = FALSE)
party_text_df_filtered <- subset(party_text_df, !(party %in% c("CDU", "CSU")))
party_text_df_merged <- rbind(party_text_df_filtered, new_row)
colnames(party_text_df_merged)[1]<-"docs"

#Remove all words starting with '#' from the 'text' column in party_text_df
party_text_df$text <- gsub("#\\w+", "", party_text_df$text)
emoji_pattern <- "[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F700-\U0001F77F\U0001F780-\U0001F7FF\U0001F800-\U0001F8FF\U0001F900-\U0001F9FF\U0001FA00-\U0001FA6F\U0001FA70-\U0001FAFF\U00002600-\U000026FF\U00002700-\U000027BF]"
party_text_df$text <- gsub(emoji_pattern, "", party_text_df$text, perl=TRUE)


tok_raw<-(tokens(corpus(party_text_df_merged), remove_punct = TRUE,remove_symbols = TRUE))
docnames(tok_raw)<-party_text_df_merged$docs
tok_raw

#sum(ntoken(tok_raw["GREENS",]))
#sum(ntoken(tok_raw["SPD",]))
#sum(ntoken(tok_raw["DIE.LINKE",]))
#sum(ntoken(tok_raw["FDP",]))
#sum(ntoken(tok_raw["CDU.CSU",]))  
#sum(ntoken(tok_raw["AFD",]))


#party_counts <- query_results_df4 %>%
#  group_by(party) %>%
#  summarise(count = n())
#
#print(party_counts)
#total_count <- sum(party_counts$count)
#total_count


source("prepro_func_plen_twit.R")
twit_dfm<-prepro_func(party_text_df_merged)
#tstat_key<-textstat_keyness(twit_dfm,"AFD")
#textplot_keyness(tstat_key)
View(kwic(tok_raw,pattern="steck*",separator = " ",window=6))

#wordfish
tmod_wf <- textmodel_wordfish(twit_dfm,dir=c(5,6))
summary(tmod_wf)
plot<-textplot_scale1d(tmod_wf)
modified_plot <- plot +   geom_point(size = 2.5) +   theme(
  axis.text = element_text(size = 11), 
  axis.title = element_text(size = 11),
  plot.title = element_text(face = "bold", size = 12)
)  +
  labs(title = paste0("Wordfish twitter: ",topic2))

modified_plot

ggsave(
  filename = paste0("pics/", topic, "/", topic, "_wordfish_twit_plot.png"),
  plot = modified_plot,
  dpi = 300,  # Adjust DPI if needed
  width = 1670 / 300,  # Width in inches (1670 pixels / 300 DPI)
  height = 1060 / 300  # Height in inches (1060 pixels / 300 DPI)
)


twit_fish_weights<-tmod_wf$beta
names(twit_fish_weights)<-tmod_wf$features
twit_fish_weights<-data.frame(twit_fish_weights)
doc_scores<-tmod_wf$theta
names(doc_scores)<-tmod_wf$docs
save(doc_scores,file=paste0("twit doc scores/",topic,"_twit_doc_scores.RData"))
save(party_text_df,file=paste0("dfs/",topic,"_twit_df.RData"))
save(twit_dfm,file=paste0("dfms/",topic,"_twit_dfm.RData"))
save(twit_fish_weights,file=paste0("weights/",topic,"_twit_fish_weights.RData"))




