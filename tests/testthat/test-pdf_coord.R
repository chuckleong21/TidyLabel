box::use(
  testthat[test_that, 
           expect_equal, expect_no_error],
  readr[read_rds]
)
box::use(
  app/logic/pdf_coord[get_pdf_version, get_coordinates]
)

test_that("pdf version recognition", {
  expectant <- read_rds("get_pdf_version_expectant.rds")
  expect_no_error(get_pdf_version())
  expect_type(get_pdf_version(), "character")
  expect_equal(expectant$version, "7816168667")
  expect_equal(expectant$org, "ООО \"БАДИС\"\nРОССИЯ, 199034, ГОРОД, САНКТ-ПЕТЕРБУРГ, В.О., 8-Я\nЛИНИЯ, Д. 1/20, ПОМ. 7Н\n1027808012870")
})

test_that("pdf coordinates of different pages, 8667 version", {
  expectant1 <- read_rds("get_coordinates_expectant.rds")
  expectant2 <- read_rds("get_coordinates_expectant2.rds")
  expect_type(expectant1, "list")
  # greedy search: two different kinds of coordinates for this version
  # page 1, depth 1
  expect_equal(length(expectant1), 5)
  # page 1, depth 2
  expect_equal(sapply(expectant1, length), 
               setNames(rep(2, 5), c("id", "hs_code", "weight", "tax_code", "tax")))
  # page 1, depth 3
  m <- matrix(rep(1, 10), nrow = 2, ncol = 5, dimnames = list(c(), c("id", "hs_code", "weight", "tax_code", "tax")))
  expect_equal(sapply(expectant1, function(x) sapply(x, length)), m)
  expect_equal(sapply(expectant1, function(x) sapply(x, length)) |> dim(), c(2, 5))
  # innermost depth
  mm <- matrix(rep(4, 10), nrow = 2, ncol = 5, dimnames = list(c(), c("id", "hs_code", "weight", "tax_code", "tax")))
  expect_equal(
    sapply(expectant1, function(x) sapply(x, function(y) sapply(y, function(z) sapply(z, length)))), mm
  )
  # page >= 2
  mmm <- matrix(rep(3, 10), nrow = 2, ncol = 5, dimnames = list(c(), c("id", "hs_code", "weight", "tax_code", "tax")))
  expect_equal(sapply(expectant2, function(x) sapply(x, length)), mmm)
})
