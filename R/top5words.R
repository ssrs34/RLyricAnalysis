#' top5words
#'
#' Creates a bar plot of the top 5 most used words in a Document Term Matrix or data frame of formatted and cleaned lyrics
#'
#' @param cleanedlyrics data frame of formatted and cleaned lyrics by formatLyrics() and cleanLyrics() or a Document Term Matrix of lyrics
#' @param dtm boolean that indicates if cleanedlyrics is a data frame or Document Term Matrix
#'
#' @return bar plot of top five most used lyrics in data frame
#' @export
#'
#' @examples
#' \dontrun{
#' top5words(cleaned_lyrics, dtm = F)
#' top5words(my_dtm, dtm = T)
#' }
#'
#' @export
top5words <- function(cleanedlyrics, dtm = FALSE) {
  #dtm or cleaned lyrics as input?
  if (dtm == TRUE) {
    #if input is already a DTM, do this
    bp <- barplot(cleanedlyrics$freq[1:5], names.arg = cleanedlyrics$word[1:5], col = "peachpuff4")
    return(invisible(bp))
  }

  #otherwise, we need to make a dtm first
  dtm <- TermDocumentMatrix(cleanedlyrics[, 1])
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix), decreasing = TRUE)
  df <- data.frame(word = names(words), freq = words)

  barplot(df$freq[1:5], names.arg = df$word[1:5], ylim = c(0,30),col = "peachpuff4")
}
