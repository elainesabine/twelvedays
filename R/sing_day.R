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
  num_in_words <- c("Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve")
  num_in_words <- case_when(
    str_detect(dataset$Gift.Item[1], "^[aeiou]") ~ c("and an", num_in_words),
    TRUE ~ c("and a", num_in_words))
  dataset <- add_column(dataset, num_in_words)

  ## make the phrases
  dataset <- dataset %>%
    mutate({{phrase_col}} := pmap_chr(dataset, ~make_phrase(..1, ..8, ..7, ..4, ..5, ..6))) %>%
    mutate({{phrase_col}} := case_when(
      Day == 1 ~ paste({{phrase_col}}, ".", sep = ""),
      TRUE ~ paste({{phrase_col}}, ",", sep = "")
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
