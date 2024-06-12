# Load necessary libraries
library(quanteda)
library(readr)
library(stringr)

prepro_func <- function(input_df) {
  # Paths to the stopwords and lemma dictionary files
  stopwords_path <- "stop_plen.txt"
  stopwords <- readLines(stopwords_path)
  
  remove_hashtags <- function(text) {
    # Use regular expression to remove hashtags
    str_replace_all(text, "#(\\w+)", "\\1")
  }
  input_df$text <- sapply(input_df$text, remove_hashtags)
  
  # Convert the data frame to a corpus
  corp <- corpus(input_df, text_field = "text")
  docnames(corp) <- input_df$docs
  
  # Tokenize, remove @username and other stopwords
  tok_party_raw <-tokens(corp)
  tok_party_raw <- tokens_select(tok_party_raw, pattern = "@*", selection = "remove", padding = FALSE)
  tok_party <- tokens_tolower(tokens(tok_party_raw, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = FALSE, split_hyphens = FALSE, remove_url=TRUE))

  #stemming
  tok_stem<-tokens_wordstem(tok_party, language = "german")
  
  # Remove stopwords again (in case lemmatization reintroduced any)
tok_stem <- tokens_select(tok_stem, pattern = stopwords, selection = "remove", padding = FALSE)
  
  # Create a document-feature matrix
  output_dfm <- dfm(tok_stem)
  
  # Trim the dfm
#  output_dfm <- dfm_trim(output_dfm, min_docfreq = 2, docfreq_type = "count", min_termfreq = 2, termfreq_type = "count")
  
  return(output_dfm)
}
