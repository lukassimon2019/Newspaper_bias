setwd("C:/Users/sikul/OneDrive/PPE content/bachelor thesis/R directory 2")
library(quanteda)
require(quanteda.textstats)
library(readtext)
require(quanteda.textplots)
require(quanteda.textmodels)
library(dplyr)
require(ggplot2)
require(ca)
topic<-"selbst"
load("dfs/selbst_12.04.2024_df.RData") #import party_df
df1<-party_df
load("dfs/selbst_15.11.2023_df.RData") #import party_df
df2<-party_df
#df2$text[5]
comb_df <- data.frame(docs=rownames(df2),text=paste0(df1$text,df2$text))
#re-order rows
#comb_df<-comb_df[c(3,2,6,4,5,1),]

#tstat_key<-textstat_keyness(plen_dfm,"FDP")
#textplot_keyness(tstat_key)
#View(kwic(tok_raw,pattern="halt*"))

corp_raw<-corpus(comb_df, text_field="text")
docnames(corp_raw)<-comb_df$docs
tok_raw<-tokens(corp_raw)
sum(ntoken(tok_raw))
source("prepro_func_plen_twit.R")
plen_dfm<-prepro_func(comb_df)

#wordfish
tmod_wf <- textmodel_wordfish(plen_dfm, dir = c(2,3))
summary(tmod_wf)
plot<-textplot_scale1d(tmod_wf)
modified_plot <- plot +   geom_point(size = 2.5) +   theme(
  axis.text = element_text(size = 11), 
  axis.title = element_text(size = 11),
  plot.title = element_text(face = "bold", size = 12)
  )  +
  labs(title = "Wordfish plenary: Selbstbestimmungsgesetz")
modified_plot

ggsave(
  filename = paste0("pics/", topic, "/", topic, "_wordfish_plen_plot.png"),
  plot = modified_plot,
  dpi = 300,  # Adjust DPI if needed
  width = 1670 / 300,  # Width in inches (1670 pixels / 300 DPI)
  height = 1060 / 300  # Height in inches (1060 pixels / 300 DPI)
)

#word weights
plen_fish_weights<-tmod_wf$beta
names(plen_fish_weights)<-tmod_wf$features
plen_fish_weights_df<-data.frame(plen_fish_weights)
#View(plen_fish_weights_df)
save(plen_dfm,file=paste0("dfms/",topic,"_plen_dfm.RData"))
save(plen_fish_weights_df,file=paste0("weights/",topic,"_plen_fish_weights.RData"))
