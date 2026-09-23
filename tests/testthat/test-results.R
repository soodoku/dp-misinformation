root <- "../.."
read_output <- \(name) readr::read_csv(file.path(root, "tabs", name), show_col_types = FALSE)
expect_near <- \(actual, expected, tolerance) expect_lte(abs(actual - expected), tolerance)

test_that("item shares match the 2017 draft's Table 2, computed from the original files", {
  changes <- read_output("item_changes.csv")
  get <- \(i, column) changes[[column]][changes$item == i]
  draft <- tibble::tribble(
    ~item, ~before, ~after,
    "life", .271, .154,
    "unemployment", .326, .221,
    "nhs_spending", .165, .161,
    "private_care", .117, .083,
    "breast_screening", .713, .539
  )
  purrr::pwalk(draft, \(item, before, after) {
    expect_near(get(item, "incorrect_t1"), before, 0.002)
    expect_near(get(item, "incorrect_t2"), after, 0.002)
  })
})

test_that("Danish shares correct at recruitment match Hansen (2004), Table 6.1", {
  changes <- read_output("item_changes.csv")
  get <- \(i) changes$correct_t1[changes$item == i]
  expect_near(get("fines"), .41, 0.01)
  expect_near(get("interest_rates"), .73, 0.01)
  expect_near(get("taxation"), .64, 0.01)
  expect_near(get("coins"), .53, 0.01)
})

test_that("transitions account for every answer", {
  panel_rows <- sum(read_output("polls.csv")$answers)
  expect_equal(sum(read_output("transitions.csv")$n), panel_rows)
})
