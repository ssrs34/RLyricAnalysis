#' cleanLyrics
#'
#' Expands contractions and removes punctuation from the scraped and formatted lyrics
#'
#' @param lyr data frame of formatted lyrics from formatLyrics()
#' @param expContractions boolean value that indicates if the user wants to expand the contractions
#'
#' @return data frame of cleaned lyrics
#' @export
#'
#' @examples
#' \dontrun{
#' cleaned_lyrics <- cleanLyrics(formatted_lyrics)
#'}
#' @export
cleanLyrics <- function(lyr, expContractions = FALSE){
  lyr <- lyr[!(lyr$lyrics == ""), ] #get rid of empty lyric rows
  rownames(lyr) <- 1:nrow(lyr) # renumber index

  for(row in 1:length(lyr[,1])){ # converts digits to words
    lyr[row,1] <- replace_number(lyr[row,1])
  }

  if(expContractions == TRUE){
    for(row in 1:length(lyr[,1])){ # expands contractions
      lyr[row,1] <- replace_contraction(lyr[row,1])
    }
  }

  for(row in 1:length(lyr[,1])){
    lyr[row,1] <- str_replace_all(lyr[row,1], "[[:punct:]]", "")
  }

  lyr
}
