
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


# test the diff_to_ref calculator
test_that("diff_to_ref", {
  test_data <- data.frame(y = c(1,2,3,8,10),
                          id = c(1,1,1,1,1),
                          t = c(1,2,3,4,5))
  rownames(test_data) <- c(3,4,5,6,7)
  pret <- 3
  
  # Standard case
  expect_equal(diff_to_ref(test_data, "t", "y", pret)$delta, 
               c(-2,-1,0,5,7))
  
  # missing one value
  test_data[2,"y"] <- NA 
  expect_equal(diff_to_ref(test_data, "t", "y", pret)$delta,
               c(-2,NA,0,5,7))
  
  # missing ref-value
  test_data[3,"y"] <- NA 
  
  expect_equal(diff_to_ref(test_data, "t", "y", pret)$delta, 
               as.numeric(c(NA,NA,NA,NA,NA)))
  
  # check row names
  expect_equal(rownames(diff_to_ref(test_data, "t", "y", pret)),
               rownames(test_data))
  
})



# test the simple difference calculator
test_that("simple_diff",{
  
  test_data <- data.frame(y = c(1,2,3,8,10),
                          id = c(1,1,1,1,1),
                          t = c(1,2,3,4,5))
  rownames(test_data) <- c(3,4,5,6,7)
  pret <- 3
  
  # standard case
  expect_equal(diff_1lag(test_data, "t", "y", pret)$diff,
               c(NA,1,1,5,2))
  
  # unsorted data
  test_data <- test_data[order(test_data[,"t"],decreasing = TRUE),]
  
  expect_equal(diff_1lag(test_data, "t", "y", pret)$diff,
               c(NA,1,1,5,2))
  
  # test rownames
  expect_setequal(rownames(diff_1lag(test_data, "t", "y", pret)),
              rownames(test_data))
  
})



























