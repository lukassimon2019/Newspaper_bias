setwd("C:/Users/sikul/OneDrive/PPE content/bachelor thesis/R directory 2")
library(quanteda)
require(quanteda.textstats)
library(readtext)
require(quanteda.textplots)
require(quanteda.textmodels)
library(dplyr)
require(ggplot2)
require(ca)
topic<-"wahl"
load("dfs/wahl_17.03.2023_df.RData") #import party_df
df1<-party_df
load("dfs/wahl_27.01.2023_df.RData") #import party_df
df2<-party_df

comb_df <- data.frame(docs=rownames(df2),text=paste0(df1$text,df2$text))
#re-order rows
#comb_df<-comb_df[c(3,2,6,4,5,1),]


corp_raw<-corpus(comb_df, text_field="text")
docnames(corp_raw)<-comb_df$docs
tok_raw<-tokens(corp_raw)

source("prepro_func_plen_twit.R")
plen_dfm<-prepro_func(comb_df)
load(paste0("twit doc scores/",topic,"_twit_doc_scores.RData"))
#reorder docscores according to plen_dfm docs order
doc_scores1<-doc_scores[c(match(plen_dfm$docs,names(doc_scores)))]

#run wordscores
wordscore_obj<-textmodel_wordscores(plen_dfm,doc_scores1)
est<-predict(wordscore_obj)
plot<-textplot_scale1d(est)
modified_plot <- plot +   geom_point(size = 2.5) +   theme(
  axis.text = element_text(size = 11), 
  axis.title = element_text(size = 11),
  plot.title = element_text(face = "bold", size = 12)
)  +
  labs(title = "Wordscores plenary: Wahlrechtsreform")

modified_plot

ggsave(
  filename = paste0("pics/", topic, "/", topic, "_wordscores_plen_plot.png"),
  plot = modified_plot,
  dpi = 300,  # Adjust DPI if needed
  width = 1670 / 300,  # Width in inches (1670 pixels / 300 DPI)
  height = 1060 / 300  # Height in inches (1060 pixels / 300 DPI)
)

plen_wordscores<-wordscore_obj$wordscores
save(plen_wordscores,file=paste0("weights/",topic,"_plen_wordscores.RData"))
