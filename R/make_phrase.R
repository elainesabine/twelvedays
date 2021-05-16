#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export


make_phrase <- function(num, num_word, item, verb, adjective, location){
  verb <- str_replace_na(verb, "")
  adjective <- str_replace_na(adjective, "")
  location <- str_replace_na(location, "")


  ## pluralize items except day 1 item
  plural_item <- case_when(num == 1 ~ item,
                           TRUE ~ pluralize_gift(item))

  ## get the correct num_word
  num_word <- case_when(
    num == 12 ~ "Twelve",
    num == 11 ~ "Eleven",
    num == 10 ~ "Ten",
    num == 9 ~ "Nine",
    num == 8 ~ "Eight",
    num == 7 ~ "Seven",
    num == 6 ~ "Six",
    num == 5 ~ "Five",
    num == 4 ~ "Four",
    num == 3 ~ "Three",
    num == 2 ~ "Two",
    num == 1 & str_detect(item, "^[aeiou]") ~ "and an",
    TRUE ~ "and a"
  )

  phrase <- str_c(num_word, adjective, plural_item, verb, location, sep = " ")
  phrase <- str_squish(phrase)

  ## add punctuation
  phrase <- case_when(
    num == 1 ~ paste(phrase, ".", sep=""),
    TRUE ~ paste(phrase, ",", sep="")
  )
  return(phrase)
}
