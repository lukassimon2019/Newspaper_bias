setwd("C:/Users/sikul/OneDrive/PPE content/bachelor thesis/R directory 2")
library(xml2)
library(XML)
library(stringr)
library(dplyr)
top_id <- "Tagesordnungspunkt 2" #tagesordnungspunkt or zusatzpunkt
topic<-"atom_09.11.2022"
transcr_cann <- read_xml(paste0("xml_files/",topic,".xml"))
transcr_cann_xml <- xmlParse(transcr_cann)

rede_id_vector <- xpathSApply(transcr_cann_xml, paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede"), xmlGetAttr, "id")

combined_list <- list()  # Initialize an empty list to store combined content

for (rede_id in rede_id_vector) {
  to20_tot <- xpathApply(transcr_cann_xml, paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede[@id='", rede_id, "']/p[position() >= 1 and position() <= 80]"), function(node) xmlValue(node))
  
  to20_J1_length <- length(xpathSApply(transcr_cann_xml, paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede[@id='", rede_id, "']/p[@klasse='J_1'][1]/preceding-sibling::p"), function(nodes) length(nodes)))
  
  to20_J2_length <- length(xpathSApply(transcr_cann_xml, paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede[@id='", rede_id, "']/p[@klasse='J_1'][2]/preceding-sibling::p"), function(nodes) length(nodes)))
  
  if (to20_J2_length != 0) {
    to20_J1.2_cont <- to20_tot[(1 + to20_J1_length):to20_J2_length]
  } else {
    to20_J1.2_cont <- to20_tot[(1 + to20_J1_length):length(to20_tot)] 
    #if length=0, then take between J1 and last class p, because 
    #this is the beginning speech. problem is
    #the occasional speech without ending J1
  }
  
  to20_J3_length <- length(xpathSApply(transcr_cann_xml, paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede[@id='", rede_id, "']/p[@klasse='J_1'][3]/preceding-sibling::p"), function(nodes) length(nodes)))
  
  to20_J4_length <- length(xpathSApply(transcr_cann_xml, paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede[@id='", rede_id, "']/p[@klasse='J_1'][4]/preceding-sibling::p"), function(nodes) length(nodes)))
  
  if (to20_J4_length != 0) {
    to20_J3.4_cont <- to20_tot[(1 + to20_J3_length):to20_J4_length]
  } else {
    to20_J3.4_cont <- list()
  }
  
  to20_J5_length <- length(xpathSApply(transcr_cann_xml, paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede[@id='", rede_id, "']/p[@klasse='J_1'][5]/preceding-sibling::p"), function(nodes) length(nodes)))
  
  to20_J6_length <- length(xpathSApply(transcr_cann_xml, paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede[@id='", rede_id, "']/p[@klasse='J_1'][6]/preceding-sibling::p"), function(nodes) length(nodes)))
  
  if (to20_J6_length != 0) {
    to20_J5.6_cont <- to20_tot[(1 + to20_J5_length):to20_J6_length]
  } else {
    to20_J5.6_cont <- list()  
  }
  
  combined_list[[rede_id]] <- unlist(c(to20_J1.2_cont, to20_J3.4_cont, to20_J5.6_cont))
  #goes through each J_1 for each rede_id
  #each content e.g. J1.2 is a list of many node-texts. these are for each rede_id concatenated into
  #one large list. E.g. the Lauterbach speech then becomes be a list containing 27 elements(character 
  #vectors). By applying unlist, each sub-list becomes a single character vector with 27 elements
}
#print(names(combined_list))
#combined_list[12]

#transform the vector element associated with each rede into one large string  
string_list <- lapply(combined_list, function(x) paste(x, collapse = " "))
# Convert the named,stringed list to a dataframe
df <- data.frame(string_list)
df$ID2012004400

#named vector with each ID associated with party, bündnis90 stored as variable as "ü"problematic
xml_13.10<-xmlParse(read_xml("xml_files/cann_13.10.xml"))
green_name_var <- xpathSApply(xml_13.10,"//tagesordnungspunkt[@top-id='Tagesordnungspunkt 5']/rede[@id='ID2013014600']/descendant::fraktion[1]", function(node) xmlValue(node))
minister_parties <- c("11003797" = "SPD","11003142"="SPD","11005074"=green_name_var,"999990120"="SPD","11004127"=green_name_var,"11002720"=green_name_var)

#seq along is needed so that i can be a number, and not a rede_id, so that i can then be used to 
#replace rede_vector elements and not add new elements
redner_id_vector<-vector("character",length(rede_id_vector))
redner_party_vector<-vector("character",length(rede_id_vector))

for (i in seq_along(rede_id_vector)) { 
  redner_id <- xpathSApply(transcr_cann_xml,  paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede[@id='", rede_id_vector[i], "']/descendant::redner[1]"), xmlGetAttr, "id")
  redner_id_vector[i] <- redner_id
  
  redner_party <- xpathSApply(transcr_cann_xml,paste0("//tagesordnungspunkt[@top-id='", top_id, "']/rede[@id='", rede_id_vector[i], "']/descendant::fraktion[1]"), function(node) xmlValue(node))
  if (length(redner_party) == 0) {  redner_party <- minister_parties[redner_id] }
  redner_party_vector[i] <- redner_party
}
redner_party_vector
one_row_party <- t(data.frame(redner_party_vector,stringsAsFactors = FALSE))
one_row_redner <- t(data.frame(redner_id_vector))

colnames(one_row_party)<-colnames(df)
colnames(one_row_redner)<-colnames(df)

df<-rbind(df,one_row_redner,one_row_party)

# Replace 'BÜNDNIS 90/DIE GRÜNEN' with 'Greens' across all columns
#there is some weird formatting going on, thus it works best to read directly from XML
xml_13.10<-xmlParse(read_xml("xml_files/cann_13.10.xml"))
green_name_var <- xpathSApply(xml_13.10,"//tagesordnungspunkt[@top-id='Tagesordnungspunkt 5']/rede[@id='ID2013014600']/descendant::fraktion[1]", function(node) xmlValue(node))
df <- df %>%
  mutate(across(everything(), ~str_replace_all(., green_name_var, "Greens")))
#party names all uppercase
df[3,]<-toupper(df[3,])

# Initialize the vectors for each party
party_vectors <- list(
  SPD = c(),
  `CDU/CSU` = c(),
  AFD = c(),
  FDP = c(),
  `GREENS` = c(),
  `DIE LINKE` = c()
)

# Loop through each column
for (col in colnames(df)) {
  # Retrieve the party name from the third row of the current column
  party_name <- df[3, col]
  # Check if the party name is one of the keys in the list
  if (party_name %in% names(party_vectors)) {
    # Add the first row of the same column to the appropriate vector
    party_vectors[[party_name]] <- paste(party_vectors[[party_name]], df[1, col])
  }
}
names(party_vectors)
#party_vectors[6]<-"NA"
party_df<-as.data.frame(t(data.frame(party_vectors)))
colnames(party_df)[1]<-"text"
party_df["DIE.LINKE",]
save(party_df, file =paste0("dfs/",topic,"_df.RData"))

