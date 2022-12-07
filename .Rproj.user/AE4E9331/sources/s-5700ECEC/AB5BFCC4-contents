#' get_dtm
#'
#' Transforms the data frame of formatted and cleaned lyrics into a Document Term Matrix
#'
#' @param cleanedlyrics data frame of formatted and cleaned lyrics by formatLyrics() and cleanLyrics()
#'
#' @return Document Term Matrix of lyrics
#' @export
#'
#' @examples
#' \dontrun{
#' my_dtm <- get_dtm(cleaned_lyrics)
#' }
#'
#' @export
get_dtm <- function(cleanedlyrics){
  #making a document term matrix from the dataframe of cleaned lyrics
  dtm <- TermDocumentMatrix(cleanedlyrics[,1])
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix),decreasing=TRUE)
  df <- data.frame(word = names(words),freq=words)
}
