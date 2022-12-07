#' get_wordcloud
#'
#' Creates a wordcloud from a Document Term Matrix or data frame of formatted and cleaned lyrics
#'
#' @param cleanedlyrics data frame of formatted and cleaned lyrics by formatLyrics() and cleanLyrics() or a Document Term Matrix of lyrics
#' @param dtm boolean that indicates if cleanedlyrics is a data frame or Document Term Matrix
#'
#' @return plot, word cloud of lyrics
#' @export
#'
#' @examples
#' \dontrun{
#' get_wordcloud(cleaned_lyrics, dtm = F)
#' get_wordcloud(my_dtm, dtm = T)
#' }
#'
#' @export
get_wordcloud <- function(cleanedlyrics = NULL, dtm = FALSE) {
  if (dtm == TRUE) {
    wc <- wordcloud(words = cleanedlyrics$word, freq = cleanedlyrics$freq, min.freq = 1, max.words = 500,
                    random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2") )
    return(invisible(wc))
  }

  #making a document term matrix from the dataframe of cleaned lyrics
  dtm <- TermDocumentMatrix(cleanedlyrics[, 1])
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix), decreasing = TRUE)
  df <- data.frame(word = names(words), freq = words)

  #making the actual wordcloud using the wordcloud package
  wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 500,
            random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2") )
}
