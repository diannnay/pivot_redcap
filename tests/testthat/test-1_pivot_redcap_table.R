library(hw2.repo.dianarinker)
library(testthat)
library(here)

test_that("pivot_redcap_table gives data.frame", {
  result <- pivot_redcap_table(sample2
                               , families = c("learner_type","grad_year", "num", "hours", "content", "eval_type" )
                               , constant_cols = c( "record_id", "redcap_event_name") 
                                              )
  expect_s3_class(result, "data.frame")
  expect_true(all(c("record_id", "redcap_event_name") %in% names(result)))
})
