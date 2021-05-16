context("12 days")

test_that("pluralize gift", {

  input <- c("apple", "computer", "baby")
  expected <- c("apples", "computers", "babies")
  expect_equal( pluralize_gift(input), expected)
})

test_that("sing day", {
  xmas2 <- read.csv("https://www.dropbox.com/s/ap2hqssese1ki4j/xmas_2.csv?dl=1")
  xmas2 <- xmas2 %>%
    mutate(
      Full.Phrase = pmap_chr(xmas2, ~make_phrase(..1, ..2, ..3, ..4, ..5, ..6))
    )
  expected <- "On the first day of Christmas, my true love sent to me,\nand an email from Cal Poly."
  expect_equal(sing_day(xmas2, 1, Full.Phrase), expected)
})

test_that("make phrases", {
  xmas2 <- read.csv("https://www.dropbox.com/s/ap2hqssese1ki4j/xmas_2.csv?dl=1")
  input <- xmas2 %>%
    mutate(
      Full.Phrase = pmap_chr(xmas2, ~make_phrase(..1, ..2, ..3, ..4, ..5, ..6))
    )
  expected <- "and an email from Cal Poly."
  expect_equal(input$Full.Phrase[[1]], expected)
})
