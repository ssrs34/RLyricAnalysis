#' getArtist
#'
#' Retrieves all the songs from a specific artist
#'
#' @param artist Name of the desired artist
#'
#' @return matrix/array of scraped lyrics
#' @export
#'
#' @examples
#' billieLyrics <- getArtist("Billie Eilish")
#'
#' @export
getArtist <- function(artist){
  artist <- tolower(gsub(" ","-", gsub("[().]","", gsub("'","-",artist))))

  url <- paste('https://www.songlyrics.com/',artist,'-lyrics/', sep='')
  singer <- url %>%
    httr::GET(config = httr::config(ssl_verifypeer = TRUE)) %>%
    read_html()

  track_links <<- singer %>%
    html_nodes('table') %>% html_nodes('tr') %>% html_node('a') %>%
    html_attr('href')

  track_links <<- track_links[1:(length(track_links) - 2)]

  lyricVector <- c()
  songNameVector <- c()
  for(num in 1:length(track_links)){

    track_url <- track_links[num]
    song <- track_url %>%
      httr::GET(config = httr::config(ssl_verifypeer = TRUE)) %>%
      read_html()
    songName <- song %>% html_elements('title') %>% html_text2()
    lyrics <- song %>% html_elements('#songLyricsDiv') %>% html_text2()
    songNameVector[num] <- songName
    lyricVector[num] <- lyrics
  }
  lyrics <- cbind(lyricVector, songNameVector)
  lyrics
}
