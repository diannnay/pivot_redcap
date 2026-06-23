library(testthat)
library(PivotRedcapTable)
# tests/testthat/test_pivot_redcap_table_warning.R
test_that("pivot_redcap_table warns when a non-existent family is provided", {
  data("sample1", package = "PivotRedcapTable")

  # Include a family that does NOT exist
  families<- c("first_name","last_name", "email", "role", "nonexistent_family")

  # Expect a warning about missing family columns
  expect_warning(
    pivot_redcap_table(  sample1
                       , families
                       , constant_cols = c("record_id")),
    regexp = "Family 'nonexistent_family' has no columns"
  )
})



