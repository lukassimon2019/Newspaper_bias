setwd("C:/Users/sikul/OneDrive/PPE content/bachelor thesis/R directory 2")
topic<-"atom"
load(paste0("weights/",topic,"_twit_fish_weights.RData"))
load(paste0("weights/",topic,"_plen_wordscores.RData"))

#words not only rownames also own column
twit_fish_weights_df<-cbind(twit_fish_weights,words=rownames(twit_fish_weights))
plen_wordscores_df_pre<-data.frame(plen_wordscores)
plen_wordscores_df<-cbind(plen_wordscores_df_pre,words=rownames(plen_wordscores_df_pre))

#mean(twit_fish_weights_df$twit_fish_weights)
#mean(plen_wordscores_df$plen_wordscores)
#sd(twit_fish_weights_df$twit_fish_weights)
#sd(plen_wordscores_df$plen_wordscores)

# Identify common words
common_words <- intersect(plen_wordscores_df$words, twit_fish_weights_df$words)

# Filter the data frames to keep only rows with common words
plen_weights_filtered <- plen_wordscores_df[plen_wordscores_df$words %in% common_words, ]
twit_weights.1_filtered <- twit_fish_weights_df[twit_fish_weights_df$words %in% common_words, ]

# Merge the filtered data frames by common words
result_df <- merge(twit_weights.1_filtered, plen_weights_filtered, by = "words")

filtered_df_pos <- result_df[result_df$twit_fish_weights > 0.5 & result_df$plen_wordscores > 0.5, ]
filtered_df_neg<-result_df[result_df$twit_fish_weights < -0.5 & result_df$plen_wordscores < -0.5 , ]
filtered_df<-rbind(filtered_df_pos,filtered_df_neg)
length(rownames(filtered_df))

filtered_weights_df<-filtered_df[,-3]
save(filtered_weights_df,file=paste0("weights/",topic,"_filtered_weights.RData"))

