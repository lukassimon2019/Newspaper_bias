# Load the dataset
df <- read.csv("EPINetz_TwitterPoliticians_2023.csv", sep = ";")
topic<-"wahl"

# Filter for these parties, prevent running queries with minor parties 
party_names <- c("SPD", "GREENS", "CDU", "CSU", "AFD", "DIE.LINKE", "FDP")
handles <- list()  # Initialize an empty list

# Loop through each party and store their Twitter handles
for (party_name in party_names) {
  party_handle <- df[df$party == party_name, "twitter_handle"]
  handles[[party_name]] <- party_handle  # Properly assign to list
}

# Filter and retain only unique non-empty strings in each sublist
handles <- lapply(handles, function(x) {
  unique(x[x != ""])
})
names(handles) <- party_names

# Merge CSU and CDU
handles[["CDU"]] <- c(handles[["CDU"]], handles[["CSU"]])
handles <- handles[-which(names(handles) == "CSU")]

# Function to split Twitter handles into chunks and generate query strings
generate_query_texts <- function(handles) {
  # Initialize an empty list to store the query texts for each party
  query_texts <- list()
  
  # Loop through each party's handles
  for (party in names(handles)) {
    handles_party <- handles[[party]]
    n <- length(handles_party)
    party_queries <- list()  # Temp list to hold queries for the current party
    
    if (n > 0) {
      # Create chunks of 18 handles and generate the query for each chunk
      for (i in seq(1, n, by = 18)) {
        # Extract up to 18 handles
        current_handles <- handles_party[i:min(i + 17, n)]
        # Format them into a query string
        from_part <- paste("from:", current_handles, sep = "", collapse = " OR ")
        query_text <- sprintf("(wahlrecht OR wahlrechtsreform OR erststimme OR zweitstimme OR überhangmandat) (%s) since:2023-01-01 until:2023-05-31", from_part)
        party_queries <- c(party_queries, query_text)
      }
    }
    
    # Store the queries in the main list under the corresponding party name
    query_texts[[party]] <- party_queries
  }
  
  return(query_texts)
}

# Assuming 'handles' is already filtered from previous steps
queries_per_party <- generate_query_texts(handles)

query_texts_flat <- unlist(queries_per_party)

file_path <- paste0("query texts/", topic, "_query_text.csv")
# Write the CSV file with BOM
con <- file(file_path, open = "wt", encoding = "UTF-8")
writeLines('\uFEFF', con)  # Write BOM
write.csv(query_texts_flat, con, row.names = FALSE, quote = TRUE)
close(con)
