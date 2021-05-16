#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export


sing_day <- function(dataset, line, phrase_col){
  ## create the first line of the song
  day <- dataset %>%
    filter(Day == line)
  day <- day[[2]]
  line1 <- str_c( "On the ", day, " of Christmas, my true love sent to me,")

  ## filter relevant phrases
  phrases <- dataset %>%
    filter(Day <= line) %>%
    arrange(desc(Day)) %>%
    pull({{phrase_col}})

  ## combine all the songs except the last one
  song <- c(line1, phrases)
  song <- paste(song,collapse="\n")
  song <- str_glue(song, "\n")[[1]]
  return(song)
}
