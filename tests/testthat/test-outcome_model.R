
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
               c(2,5,1,1,NA))
  
  # test rownames
  expect_setequal(rownames(diff_1lag(test_data, "t", "y", pret)),
              rownames(test_data))
  
})



# test the difference builder function
test_that("difference_builder", {
  
  test_data <- data.frame(y = c(1,2,3,8,10,1,2,3,4,8),
                          id = c(1,1,1,1,1,2,2,2,2,2),
                          t = c(1,2,3,4,5,1,2,3,4,5))
  rownames(test_data) <- c(3,4,5,6,7,8,9,10,11,12)
  pret <- 3
  universal_base <- FALSE
  
  # standard case
  expect_equal(difference_builder(test_data, "id", "t", "y", pret, universal_base),
               c(NA, 1,1,5,7,NA,1,1,1,5))
  
  # with missings
  test_data[c(2,8), "y"] <- NA
  expect_equal(difference_builder(test_data, "id", "t", "y", pret, universal_base),
               c(NA,NA,NA,5,7,NA,1,NA,NA,NA))
  
  # unordered input data
  test_data <- data.frame(y = c(1,2,3,8,10,1,2,3,4,8),
                          id = c(1,1,1,1,1,2,2,2,2,2),
                          t = c(1,2,3,4,5,1,2,3,4,5))
  test_data <- test_data[c(3,6,8,10,1,2,4,9,5,7),]
  
  expect_equal(difference_builder(test_data, "id", "t", "y", pret, universal_base),
               c(1,NA,1,5,NA,1,5,1,7,1))

})


# test for the outcome model 
test_that("outcome_model", {
  
  test_data <- data.frame(delta = c(2,14,1,2,2,4,3,6),
                          x = c(4,36,2,4,4,8,6,12),
                          id = c(1,1,2,2,3,3,4,4),
                          treated_unit = c(1,1,0,0,0,0,0,0),
                          unit = c(1,1,2,2,3,3,4,4),
                          time = c(3,5,3,5,3,5,3,5))
  rownames(test_data) <- c(3,4,5,6,7,8,9,10)
  pret <- 3
  t <- 5
  C <- c(FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE, TRUE)
  G <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE, FALSE)                 # index for the current treated unit in the original data
  g <- 1                                      # unitname of the treated unit             
  xformula <- c("x")
  form <- as.formula(delta ~ 1 + treated_unit + x)
  
  # Standard case: without errors?
  expect_no_error(estimate_outcome_model(test_data, "time", "id", "unit", t, C, G, g, pret, xformula, form))
  
  
  outcome_list <- estimate_outcome_model(test_data, "time", "id", "unit", t, C, G, g, pret, xformula, form)
  residuals <- outcome_list[[1]]
  treatment_effects <- outcome_list[[2]]

  # Standard case: correct atts?
  expect_equal(treatment_effects$att, 10, tolerance = 0.2)
    # Standard case: correct residuals?
  expect_equal(residuals$residuals, c(0,0,0))
  
  # with missings in control y
  test_data["10", "delta"] <- NA

  outcome_list <- estimate_outcome_model(test_data, "time", "id", "unit", t, C, G, g, pret, xformula, form)
  residuals <- outcome_list[[1]]
  treatment_effects <- outcome_list[[2]]
  
  expect_equal(treatment_effects$att, 10, tolerance = 0.2)
  expect_equal(residuals$residuals, c(0,0))
  
  
  # with missings in control x
  test_data["10", "delta"] <- 6
  test_data["9", "x"] <- NA
  
  outcome_list <- estimate_outcome_model(test_data, "time", "id", "unit", t, C, G, g, pret, xformula, form)
  residuals <- outcome_list[[1]]
  treatment_effects <- outcome_list[[2]]
  
  expect_equal(treatment_effects$att, 10, tolerance = 0.2)
  expect_equal(residuals$residuals, c(0,0))

  
  # missing in treated x
  test_data["3", "x"] <- NA
  
  outcome_list <- estimate_outcome_model(test_data, "time", "id", "unit", t, C, G, g, pret, xformula, form)
  
  expect_null(outcome_list[[1]])
  expect_null(outcome_list[[2]])
  
  # missing in treated y
  test_data["3", "x"] <- 4
  test_data["4", "delta"] <- NA
  
  outcome_list <- estimate_outcome_model(test_data, "time", "id", "unit", t, C, G, g, pret, xformula, form)
  
  expect_null(outcome_list[[1]])
  expect_null(outcome_list[[2]])
  
})
























