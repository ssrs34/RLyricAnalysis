#' formatLyrics
#'
#' Formats the scraped lyrics into a data frame and removes new line characters
#'
#' @param lyrics matrix/array of the scraped lyrics from getSong() or getArtist()
#'
#' @return data frame of formatted lyrics
#' @export
#'
#' @examples
#' \dontrun{
#' formatted_lyrics <- formatLyrics(billieLyrics)
#' }
#'
#' @export
formatLyrics <- function(lyrics){
  list_mat <- list()
  for(num in 1:length(lyrics[,1])){
    a <- strsplit(lyrics[num,1], split = '\n')
    a <- unlist(a)

    z <- rep(lyrics[num,2], length(a))

    f <- cbind(a,z)

    list_mat[[num]] <- f

  }
  list_mat <- do.call(rbind, list_mat)
  list_mat <- data.frame(list_mat)

  list_mat[c('artist','song title')] <- str_split_fixed(list_mat$z, ' - ', 2)
  list_mat <- list_mat[c('a', 'artist', 'song title')]

  row.names(list_mat) <- NULL

  colnames(list_mat) <- c('lyrics', 'artist', 'song title')

  list_mat[,'lyrics'] <- sub('\r', "", list_mat[,'lyrics'])
  list_mat

}
