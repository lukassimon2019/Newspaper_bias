load("position_estimates.RData")

#average position relative to mean: party and news
for (i in 1:11) {
  est_df[8,i] <- mean(est_df[1:6, i])
}
rownames(est_df)[8]<-"rel_mean"

#average position parties
for (i in 1:11) {
  est_df[8,i] <- mean(est_df[1:6, i])
}
rownames(est_df)[8]<-"rel_mean"


#standard deviation topic news
for (i in 1:7) {
  est_df[i,12]<-(sum(est_df[i,7:11]^2)/5)^0.5 
}
colnames(est_df)[12]<-"sd_news"



#span only 3 major newspapers
for (i in 1:7) {
  est_df[i,13]<- sum(-min(est_df[i,7:9])+max(est_df[i,7:9]))
}
colnames(est_df)[13]<-"span_major"
#span two exrteme newspapers
for (i in 1:7) {
  est_df[i,14]<- sum(-min(est_df[i,10:11])+max(est_df[i,10:11]))
}
colnames(est_df)[14]<-"span_extreme"

est_df[1:7,15]<-est_df[1:7,13]/est_df[1:7,14]

#correlation: each topic sd to each other?

parties = est_df[1:6, 1:6]  # Rows for first 6 columns
topics = est_df[1:6, 7:11]  # Rows for last 5 columns

# Calculate the correlation matrix
corr_table = round(cor(parties, topics), 2)

write.table(corr_table, file="corr_table.csv",sep=";")
corr_table

#sd only major
for (i in 1:7) {
  est_df[i,16]<-(sum((est_df[i,7:9]-mean(as.numeric(est_df[i,7:9])))^2)/3)^0.5 
}


#sd of parties, which move lot, which little (not working yet)
for (i in 1:11) {
  est_df[9,i]<-(sum((est_df[1:7,i]-est_df[8,i])^2)/7)^0.5 
}
rownames(est_df)[9]<-"sd_parties"


#compute mean for topics
for (i in 1:7) {
  est_df[i,17]<-(1-max(est_df[i,1:6]))
}
#new df with absolute values
df_absolut<-est_df[1:7,1:6]+est_df[1:7,17]
#mean for each party
for (i in 1:6) {
  df_absolut[8,i]<-mean(df_absolut[1:7,i])
}


