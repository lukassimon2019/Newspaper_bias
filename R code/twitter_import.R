df<-read.csv("EPINetz_TwitterPoliticians_2023.csv",sep=";")

party_names <- c("SPD", "Greens", "CDU", "AfD", "DIE LINKE", "FDP")
handles <- list()  # Initialize an empty list

for (party_name in party_names) {
  party_handle <- df[df$party == party_name, "twitter_handle"]
  handles[[party_name]] <- party_handle  # Properly assign to list
}
#print each element x, but filter out the x which are empty strings
handles <- lapply(handles, function(x) x[x != ""])

# Function to split Twitter handles into chunks of 80 and generate query strings
generate_query_texts <- function(handles) {
  # Initialize an empty list to store the query texts for each party
  query_texts <- list()
  
  # Loop through each party's handles
  for (party in names(handles)) {
    handles_party <- handles[[party]]
    n <- length(handles_party)
    party_queries <- list()  # Temp list to hold queries for the current party
    
    # Create chunks of 80 handles and generate the query for each chunk
    for (i in seq(1, n, by = 18)) {
      # Extract up to 80 handles
      current_handles <- handles_party[i:min(i+17, n)]
      # Format them into a query string
      from_part <- paste("from:", current_handles,sep="", collapse = " OR ")
      query_text <- sprintf("(legalisierung OR bubatz OR marihuana OR cannabis OR canabis OR legalisieren) (%s) until:2024-03-20 since:2024-01-20", from_part)
      party_queries <- c(party_queries, query_text)
    }
    
    # Store the queries in the main list under the corresponding party name
    query_texts[[party]] <- party_queries
  }
  
  return(query_texts)
}

# Assuming 'handles' is already filtered from previous steps
queries_per_party <- generate_query_texts(handles)
print(queries_per_party)



