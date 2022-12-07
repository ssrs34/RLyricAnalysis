#' sentimentProportion
#'
#' Calculates the detected sentiments in the lyrics
#'
#' @param x vector, or a column of a data frame, of lyrics
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' sentimentProportion(cleaned_lyrics[,1])
#'}
#'
#' @export
sentimentProportion <- function(x){
  sentimentMatrix <- get_nrc_sentiment(x)
  proportions <- colSums(sentimentMatrix) / length(x)
  proportions

}
