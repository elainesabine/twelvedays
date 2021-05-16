#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export


pluralize_gift <- function(gift){
  gift <- case_when(
    str_detect(gift, "s$") ~ str_replace(gift, "y$", "ies"),
    str_detect(gift, "oo") ~ str_replace(gift, "oo", "ee"),
    TRUE ~ str_c(gift, "s"))
  return(gift)
}


