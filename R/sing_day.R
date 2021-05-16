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

  ## iterate through gifts to get the plural for all the days except 1
  dataset <- dataset %>%
    mutate(plural_items = case_when(
      Day == 1 ~ Gift.Item,
      Day != 1 ~ map_chr(Gift.Item, ~pluralize_gift(..1))))


  ## create a column with the string version of the day
  num_in_words <- c("and a", "Two", "Three", "Four", "Five", "Six", "Seven",
                    "Eight", "Nine", "Ten", "Eleven", "Twelve")
  dataset <- add_column(dataset, num_in_words)

  ## make the phrases
  dataset <- dataset %>%
    mutate(Full.Phrase =
             pmap_chr(dataset, ~make_phrase(..1, ..8, ..7, ..4, ..5, ..6))) %>%
    mutate(Full.Phrase = case_when(
      Day == 1 ~ paste(Full.Phrase, ".", sep = ""),
      TRUE ~ paste(Full.Phrase, ",", sep = "")
    ))

  ## filter relevant phrases
  phrases <- dataset %>%
    filter(Day <= line) %>%
    arrange(desc(Day)) %>%
    pull({{phrase_col}})

  ## combine all the songs except the last one
  song <- c(line1, phrases)
  song <- paste(song,collapse="\n")
  song <- strsplit(song, "\n")[[1]]
  return(song)
}
