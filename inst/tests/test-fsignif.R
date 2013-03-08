# testthat test script for fsignif.R
# 
# Author: Paul Hurley
###############################################################################
context("Significant Figure Tests")
test_that("Normal Tests",{
  expect_that(fsignif(12.345, digits=6), prints_text("12.3450"), "12.345 to 6 sig figs")
  expect_that(fsignif(12.345, digits=5), prints_text("12.345"), "12.345 to 5 sig figs")
  expect_that(fsignif(12.345, digits=4), prints_text("12.34"), "12.345 to 4 sig figs")
  expect_that(fsignif(12.345, digits=3), prints_text("12.3"), "12.345 to 3 sig figs")
  expect_that(fsignif(12.345, digits=2), prints_text("12."), "12.345 to 2 sig figs")
  expect_that(fsignif(12.345, digits=1), prints_text("10."), "12.345 to 1 sig figs")
  #expect_that(1,equals(2),"This should fail")
})
test_that("Normal Tests",{
  expect_that(fsignif(0.0012345, digits=6), prints_text("0.00123450"), "0.0012345 to 6 sig figs")
  expect_that(fsignif(0.0012345, digits=5), prints_text("0.0012345"), "0.0012345 to 5 sig figs")
  expect_that(fsignif(0.0012345, digits=4), prints_text("0.001234"), "0.0012345 to 4 sig figs")
  expect_that(fsignif(0.0012345, digits=3), prints_text("0.00123"), "0.0012345 to 3 sig figs")
  expect_that(fsignif(0.0012345, digits=2), prints_text("0.0012"), "0.0012345 to 2 sig figs")
  expect_that(fsignif(0.0012345, digits=1), prints_text("0.001"), "0.0012345 to 1 sig figs")
  #expect_that(1,equals(2),"This should fail")
})