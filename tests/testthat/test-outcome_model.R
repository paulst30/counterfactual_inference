
#test define control group
test_that("define control group", {
  test_data <- data.frame(g = c(0,0,0,3,3,3,2,2,2),
                          t = c(1,2,3,1,2,3,1,2,3))
  group <- 2 # current group for which treatment effects are calculated
  control_group <- "not_yet_treated"
  
  # Case with valid not-yet-treated obs
  expect_equal(define_control_group(test_data, "g", "t", group, control_group),
               c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  
  # Case without valid not-yet-treated obs
  group <- 3
  expect_equal(define_control_group(test_data, "g", "t", group, control_group),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  
  # Baseline case
  control_group2 <- "never_treated"
  expect_equal(define_control_group(test_data, "g", "t", group, control_group2),
               c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  
})




