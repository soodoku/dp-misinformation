root <- "../.."
source(file.path(root, "R", "sources.R"))

test_that("answer_state() maps the release's 1/0/missing coding", {
  expect_equal(as.character(answer_state(c(1, 0, NA))), c("correct", "incorrect", "dk"))
})

test_that("every item is keyed and uses columns that exist", {
  items <- readr::read_csv(file.path(root, "docs", "items.csv"), show_col_types = FALSE)
  expect_true(all(items$key %in% c(TRUE, FALSE)))
  purrr::pwalk(items, \(file, t1, t2, ...) {
    columns <- names(readr::read_csv(file.path(root, "data", "raw", file), n_max = 0, show_col_types = FALSE))
    expect_true(all(c(t1, t2) %in% columns))
  })
})
