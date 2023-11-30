#' Keywords in Brief Titles Word Cloud Plot
#'
#' @description
#' Generates a word cloud plot based on the brief titles of the filtered
#' clinical trials, reflecting the frequency of words
#' after text pre-processing and filtering.
#' @author Houmin Xing
#' @param studies the 'studies' data frame containing information about all clinical trials
#' @param sponsor_type the specified sponsor type for studies filtering
#' @param status_type the specified status type for studies filtering
#' @param brief_title_kw Keyword(s) for filtering studies by their brief titles.
#' If this parameter is empty, only the first 1000 studies are considered.
#' @returns Displays a word cloud plot representing the frequency of words
#' in the brief titles of the filtered clinical trials.
create_word_cloud_plot = function(studies, sponsor_type, status_type, brief_title_kw){
  if (brief_title_kw == ""){
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw) |>
      head(1000)
  }
  else{
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw)
  }
  text <- d |> select(brief_title)
  # Create a corpus
  docs <- Corpus(VectorSource(text))
  # clean texts data
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Create a document-term-matrix
  dtm <- TermDocumentMatrix(docs)
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix), decreasing = TRUE)
  df <- data.frame(word = names(words), freq = words)
  # only keep the first 35 most frequent words
  df = head(df, n = 35)
  # Generate the word cloud
  set.seed(620) # for reproducibility
  wordcloud2(df)
}
