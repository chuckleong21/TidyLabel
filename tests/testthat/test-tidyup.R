box::use(
  testthat[test_that, 
           expect_no_error, expect_type, 
           expect_equal, expect_failure, 
           expect_contains], 
  tibble[tibble], 
  readr[read_rds]
)

box::use(
  app/logic/pdf_coord[get_coordinates, get_pdf_version],
  app/logic/tidyup[tidy_page, tidyup, fuzzymatch, header_sub]
)

test_that("fuzzymatch", {
  # z0 converted into NA
  truth <- c("1010", "2010", "5010")
  test <- c("1", "10", "101", "1010", 
            "2", "20", "201", "2010", 
            "z0", "z01", 
            "5", "50", "501", "5010")
  expect_contains(fuzzymatch(test, truth), NA)
  test <- c("1", "10", "101", "1010")
  expect_equal(unname(fuzzymatch(test, truth)), rep("1010", 4))
  test <- c("2", "20", "201", "2010")
  expect_equal(unname(fuzzymatch(test, truth)), rep("2010", 4))
  test <- c("5", "50", "501", "5010")
  expect_equal(unname(fuzzymatch(test, truth)), rep("5010", 4))
  # won't recognize as 2010
  test <- "z"
  expect_failure(expect_equal(fuzzymatch(test, truth), "2010"))
  # has one-to-many results
  expect_equal(fuzzymatch(test, truth), truth)
  test <- "z0"
  expect_equal(fuzzymatch(test, truth), truth)
  expect_failure(expect_equal(fuzzymatch(test, truth), "2010"))
  test <- "z01"
  expect_equal(unname(fuzzymatch(test, truth)), "1010")
  expect_failure(expect_equal(fuzzymatch(test, truth), "2010"))
})

test_that("tidy_page & tidyup", {
  pdf <- "10228010_050824_5209318 CN134.pdf"
  ver <- read_rds("get_pdf_version_expectant.rds")$version
  expectant <- read_rds("tidy_page_expectant.rds")
  expect_s3_class(expectant, c("tbl_df", "tbl", "data.frame"))
  expect_equal(expectant, 
                   tibble(id = rep(1, 3), 
                          hs_code = rep(8467292000, 3), 
                          weight = rep(607.84, 3), 
                          tax_code = c(1010L, 2010L, 5010L), 
                          tax = c(30000.00, 4313.32, 9489.30)))
  expect_error(tidy_page(pdf, 1:3, version = ver))
  expect_equal(unique(tidyup(pdf, page = 2:5, version = ver)$tax_code), 
               c(2010L, 5010L))
})