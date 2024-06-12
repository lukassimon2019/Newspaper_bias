setwd("C:/Users/sikul/OneDrive/PPE content/bachelor thesis/R directory 2")
library(quanteda)
require(quanteda.textstats)
library(readtext)
require(quanteda.textplots)
require(quanteda.textmodels)
library(dplyr)
require(ggplot2)
topic="selbst"

load(paste0("dfs/",topic,"_news_df.RData"))
source("prepro_func.R")
news_dfm<-prepro_func(df)

load(paste0("weights/",topic,"_fish_fish_filtered_df.RData")) #filtered_df
load(paste0("dfms/",topic,"_twit_dfm.RData")) #twit_dfm
subset_twit_dfm <- twit_dfm[, colnames(twit_dfm) %in% filtered_df$words]
news_dfm_subset<-news_dfm[, colnames(news_dfm) %in% filtered_df$words]

#score vectors. just formality for wordscores to use all words within the subset_party_dfm 
party_scores <- rep(NA, (length(subset_twit_dfm$docs)+length(rownames(news_dfm))))
party_scores[1] <- -1 #AfD
party_scores[2] <- -1 #AfD
party_scores[3] <- -1 #AfD
party_scores[4] <- +1 #AfD
party_scores[5] <- +1 #Greens
party_scores[6] <- -1 #AfD
names(party_scores)<-c(rownames(subset_twit_dfm),(rownames(news_dfm)))
party_scores

compl_dfm<-rbind(subset_twit_dfm,news_dfm_subset)
wordscore_obj<-textmodel_wordscores(compl_dfm,party_scores)
wordscore_obj$wordscores

wordscores_names <- names(wordscore_obj$wordscores)
match_indices <- match(wordscores_names, filtered_df$words)
ordered_weights <- filtered_df$weights[match_indices]
named_vector <- setNames(ordered_weights, wordscores_names)
wordscore_obj$wordscores<- named_vector
est<-predict(wordscore_obj)

#rescale to -1 to +1
rescale <- function(x) {
  # Find the minimum and maximum of the input
  a <- min(x)
  b <- max(x)
  # Define the new minimum and maximum
  c <- -1
  d <- 1
  # Apply the transformation formula
  y <- c + ((x - a) * (d - c)) / (b - a)
  return(y)
}

#feature plot
feature_plot<-textplot_scale1d(wordscore_obj,margin="features", alpha=0.9)

mod_feat_plot <- feature_plot +   theme(
  axis.text = element_text(size = 11),
  axis.title = element_text(size = 11),
  plot.title = element_text(face = "bold", size = 12)
)  +
  labs(title = "Feature plot: Selbstbestimmungsgesetz")
mod_feat_plot


ggsave(
  filename = paste0("pics/", topic, "/", topic, "_feature_plot.png"),
  plot = mod_feat_plot,
  dpi = 300,  # Adjust DPI if needed
  width = 2400 / 300,  # Width in inches (1670 pixels / 300 DPI)
  height = 1500 / 300  # Height in inches (1060 pixels / 300 DPI)
)



scaled_est <- rescale(est)

plot<-textplot_scale1d(scaled_est,groups=c(rep("parties",times=6),rep("news",times=5)))
modified_plot <- plot +   geom_point(size = 2.5) +   theme(
  axis.text = element_text(size = 11),
  axis.title = element_text(size = 11),
  plot.title = element_text(face = "bold", size = 12)
)  +
  labs(title = "Newspaper estimation: Cannabis")

modified_plot

ggsave(
  filename = paste0("pics/", topic, "/", topic, "_news_wordfish_plot.png"),
  plot = modified_plot,
  dpi = 300,  # Adjust DPI if needed
  width = 1670 / 300,  # Width in inches (1670 pixels / 300 DPI)
  height = 1060 / 300  # Height in inches (1060 pixels / 300 DPI)
)




load("position_estimates.RData")
row<-6
#compute static party position compared to mean
est_df[row,1]<-scaled_est[1]-mean(scaled_est[1:6])
est_df[row,2]<-scaled_est[2]-mean(scaled_est[1:6])
est_df[row,3]<-scaled_est[3]-mean(scaled_est[1:6])
est_df[row,4]<-scaled_est[4]-mean(scaled_est[1:6])
est_df[row,5]<-scaled_est[5]-mean(scaled_est[1:6])
est_df[row,6]<-scaled_est[6]-mean(scaled_est[1:6])

est_df[row,7]<-scaled_est[7]-mean(scaled_est[7:11])
est_df[row,8]<-scaled_est[8]-mean(scaled_est[7:11])
est_df[row,9]<-scaled_est[9]-mean(scaled_est[7:11])
est_df[row,10]<-scaled_est[10]-mean(scaled_est[7:11])
est_df[row,11]<-scaled_est[11]-mean(scaled_est[7:11])

rownames(est_df)[row]<-topic

#save(est_df,file="position_estimates.RData")






weights<-ordered_weights
paper<-"faz"
tstat_key<-textstat_keyness(news_dfm_subset,paper)
tstat_key<-tstat_key[match(wordscores_names,tstat_key$feature),]
stat_scored<-cbind(tstat_key[,c(1,2)],weights)
#View(stat_scored)

# Sort and select top 10 features by chi2 value
top_features <- stat_scored[order(-stat_scored$chi2), ][1:5, ]
# Create a custom color scale
scale_color <- scale_fill_gradient2(
  low = "red", mid = "white", high = "green", 
  midpoint = 0, 
  space = "Lab", 
  guide = "colourbar"
)
# Create the bar chart
modified_plot <- ggplot(top_features, aes(x = reorder(feature, chi2), y = chi2, fill = weights)) +
  geom_bar(stat = "identity") +
  scale_color +  # Ensure you specify what scale_color does, it appears incomplete
  labs(x = "Feature", y = "Chi2 Value", title = "Cannabis-FAZ keyness") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 11), # Increase size and boldness of axis text
    axis.title = element_text(size = 11),  # Increase size and boldness of axis titles
    plot.title = element_text(face = "bold", size = 12) 
  ) +
  coord_flip()

modified_plot

ggsave(
  filename = paste0("pics/", topic, "/", topic,"_keyness_",paper,".png"),
  plot = modified_plot,
  dpi = 300,  # Adjust DPI if needed
  width = 1670 / 300,  # Width in inches (1670 pixels / 300 DPI)
  height = 750 / 300  # Height in inches (1060 pixels / 300 DPI)
)









