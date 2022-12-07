#' getSong
#'
#' Retrieves a specific song from a specific artist
#'
#' @param name Name of the desired song
#' @param artist Name of the desired artist
#'
#' @return matrix/array of scraped lyrics
#' @export
#'
#' @examples
#' song <- getSong('Billie Eilish', 'Party Favor')
#'
#' @export
getSong <- function(name, artist){
  name <- tolower(gsub(" ","-", gsub("[().']","", gsub("'","-",name))))
  artist <- gsub(" ", "-", artist)
  url <- paste("https://www.songlyrics.com/",artist, "/", name, "-lyrics/", sep='')
  track_url <-url
  song <- track_url %>%
    httr::GET(config = httr::config(ssl_verifypeer = TRUE)) %>%
    read_html()
  songName <- song %>% html_elements("title") %>% html_text2()
  songlyrics <- song %>% html_elements('#songLyricsDiv') %>% html_text2()
  lyrics <- cbind(songlyrics, songName)
  lyrics
}
