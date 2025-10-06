

test_that("kolada_base returns base URL", {
  expect_type(kolada_base(), "character")
  expect_equal(kolada_base(), "https://api.kolada.se/v3")
})

test_that("kolada_get_list returns a list for valid path", {
  result <- kolada_get_list("/kpi", query = list(limit = 1))
  expect_type(result, "list")
  expect_true("values" %in% names(result))
})

test_that("kolada_get_paginated handles pagination correctly", {
  result <- kolada_get_paginated("/kpi", query = list(limit = 1))
  expect_type(result, "list")
  expect_true(all(sapply(result, function(x) "values" %in% names(x))))
})

test_that("get_kpi returns a tibble with expected columns", {
  result <- get_kpi()
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("id", "title", "description", "municipality_type") %in% names(result)))
})

test_that("get_kpi_by_Ids returns tibble with expected columns", {
  ids <- c("N00951", "U00002")
  result <- get_kpi_by_Ids(ids)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), length(ids))
  expect_true(all(c("id", "title", "description", "municipality_type", "operating_area") %in% names(result)))
})

test_that("get_kpi_by_Ids handles invalid IDs gracefully", {
  result <- get_kpi_by_Ids("INVALID_ID")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$id, "INVALID_ID")
  expect_true(is.na(result$title))
})

test_that("get_municipality returns tibble with correct columns", {
  res <- get_municipality()
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("id", "title", "type"))
})

test_that("get_municipality_by_Ids returns tibble for given ids", {
  res <- get_municipality_by_Ids(c("0001"))
  expect_s3_class(res, "tbl_df")
  expect_equal(res$id[1], "0001")
  expect_equal(res$title[1], "Region Stockholm")
})
