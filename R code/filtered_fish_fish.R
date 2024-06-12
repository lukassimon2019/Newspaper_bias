setwd("C:/Users/sikul/OneDrive/PPE content/bachelor thesis/R directory 2")
topic<-"cann"
load(paste0("weights/",topic,"_twit_fish_weights.RData"))
load(paste0("weights/",topic,"_plen_fish_weights.RData"))
#mean(twit_fish_weights$twit_fish_weights)
#mean(plen_fish_weights_df$plen_fish_weights)
#sd(twit_fish_weights$twit_fish_weights)
#sd(plen_fish_weights_df$plen_fish_weights)

#words not only rownames also own column
twit_fish_weights.1<-cbind(twit_fish_weights,words=rownames(twit_fish_weights))
plen_fish_weights_df<-cbind(plen_fish_weights_df,words=rownames(plen_fish_weights_df))

# Identify common words
common_words <- intersect(plen_fish_weights_df$words, twit_fish_weights.1$words)

# Filter the data frames to keep only rows with common words
plen_weights_filtered <- plen_fish_weights_df[plen_fish_weights_df$words %in% common_words, ]
twit_weights.1_filtered <- twit_fish_weights.1[twit_fish_weights.1$words %in% common_words, ]

# Merge the filtered data frames by common words
result_df <- merge(twit_weights.1_filtered, plen_weights_filtered, by = "words")


filtered_df_pos <- result_df[result_df$twit_fish_weights > 0.5 & result_df$plen_fish_weights > 0.5, ]
filtered_df_neg<-result_df[result_df$twit_fish_weights < -0.5 & result_df$plen_fish_weights < -0.5 , ]
filtered_df<-rbind(filtered_df_pos,filtered_df_neg)
#length(rownames(filtered_df))

#mean_weights <- (filtered_df$twit_fish_weights + filtered_df$plen_fish_weights) / 2
#weighted_df <- data.frame(words = filtered_df$words, weights = mean_weights)
filtered_df<-data.frame(words = filtered_df$words, weights = filtered_df$twit_fish_weights)

length(filtered_df$words)
save(filtered_df,file=paste0("weights/",topic,"_fish_fish_filtered_df.RData"))
