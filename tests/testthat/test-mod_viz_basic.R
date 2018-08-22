context("mod_viz_basic")
# This file is for testing the applications in the apps/ directory.

library(shinytest)

test_that("mod_viz inputs works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  expect_pass(testApp("ifn_app/", compareImages = FALSE))
})