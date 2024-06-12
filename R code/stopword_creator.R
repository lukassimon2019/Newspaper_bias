#setwd("C:/Users/sikul/OneDrive/PPE content/bachelor thesis/R directory")
#library(xml2)
#library(XML)
#library(stringr)
#library(dplyr)
#library(quanteda)
#transcr_cann <- read_xml("13.10_cannabis.xml")
#transcr_16.04 <- read_xml("16.04_bsp_xml.xml")
#transcr_21.03 <- read_xml("21.03_bsp_xml.xml")
#transcr_11.04 <- read_xml("11.04_bsp_xml.xml")
#transcr_24.04 <- read_xml("24.04_bsp_xml.xml")
#
#
## Create a new root node to hold the combined content
#combined_root <- xml_new_root("combined")
#
## Function to add children of an XML document to the combined root
#add_children_to_combined <- function(combined_root, doc) {
#  for (child in xml_children(doc)) {
#    xml_add_child(combined_root, child)
#  }
#}
#
## Add the contents of each XML file to the combined root
#add_children_to_combined(combined_root, transcr_cann)
#add_children_to_combined(combined_root, transcr_16.04)
#add_children_to_combined(combined_root, transcr_21.03)
#add_children_to_combined(combined_root, transcr_11.04)
#add_children_to_combined(combined_root, transcr_24.04)
#
## Write the combined XML content to a new file
#write_xml(combined_root, "combined.xml")
#
#all_p_elements <- xml_find_all(combined_root, "//p")
#
## Get the text content of each <p> element
#all_p_text <- xml_text(all_p_elements)
#
#df<-data.frame(text=paste(all_p_text,collapse=" "))
#
#
#corp<-corpus(df)
#tok<-tokens(corp)
##tok_party_raw<-tokens_tolower(tokens(corp, remove_punct = TRUE,  remove_symbols = TRUE,remove_numbers = TRUE))
#
#dfm<-dfm(tok, remove_punct = TRUE,  remove_symbols = TRUE,remove_numbers = TRUE)
#dfm
#save(dfm,file="stopword_dfm.RData")

load("stopword_dfm.RData")

text_vector <- readLines("stopwords-de.txt")
dfm_selected2 <-dfm_select(dfm, pattern = c(text_vector,stop_plen), selection = "remove",padding=FALSE)
dfm_selected2
#lemma_dict <- readRDS("lemma_dict.rds")
#lemma_dfm<-dfm_lookup(dfm_selected2,lemma_dict,exclusive=FALSE)
#
#lemma_dfm
#
#top_features_lemma<-names(topfeatures(lemma_dfm,n = 200,decreasing = TRUE))
#top_features_lemma
top_features<-names(topfeatures(dfm_selected2,n = 300,decreasing = TRUE))
top_features
# Write the names to a text file
writeLines(top_features, "top_feature_names.txt")

stop_plen<- readLines("top_feature_names_manual.txt")
stop_plen<-c(stop_plen,text_vector)

#add politician names
register_df<-read.csv("EPINetz_TwitterPoliticians_2023.csv",sep=";")
namen_vec <- unique(c(register_df[,"vorname"],register_df[,"nachname"]))
stop_plen<-c(stop_plen,namen_vec)
#months<-c("januar","februar","märz","april","mai","juni","juli","august","september","oktober","november","dezember")
#stop_plen<-c(stop_plen,months)
save(stop_plen,file="stop_plen.RData")
writeLines(stop_plen, "stop_plen.txt")

#remove bisher, beste, ...

