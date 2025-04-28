
# generate general data set with no flaws
test_that("sanity check",{
  dta <- data.frame(id = rep(1:50, each=30),
                    t = rep(1:30, n=50),
                    z = abs(rep(rnorm(50, mean=0, sd=2), each=30)),
                    x = rnorm(1500, mean=10, sd=1))

  dta$z[dta$id==1] <- 1    # sd for treated units = 2
  dta$z[dta$id==2] <- 1
  dta$e <- rnorm(1500, mean=0, sd=sqrt(1+dta$z))
  dta$y <- dta$x + dta$e
  dta$group <- 0
  dta$group[dta$id==1] <- 15
  dta$group[dta$id==2] <- 16
  dta$y[dta$group <= dta$t & dta$id %in% c(1,2)] <- dta$y[dta$group <= dta$t & dta$id %in% c(1,2)] + 10
  
  results <- simple_staggered_did(yname = "y",
                                  tname = "t",
                                  gname = "group",
                                  idname = "id",
                                  xformula = "x",
                                  varformula = c("z"),
                                  data = dta)
  # does it run?
  expect_no_error(simple_staggered_did(yname = "y",
                                       tname = "t",
                                       gname = "group",
                                       idname = "id",
                                       xformula = "x",
                                       varformula = c("z"),
                                       data = dta))
  
  # are the aggregate results correct? 
  expect_equal(results$average_treatment_effect$att, 10, tolerance = 0.4)
  
  # are the group-wise results correct?
  expect_equal(results$groupwise_treatment_effects$att, c(10,10), tolerance = 0.4)
  
  # are critical values correct?
  # Var[e] = E[(1+z)^2] = 5
  expect_equal(mean(results$grouptime_treatment_effects$crit_val), 5, tolerance = 0.3)
  
  # are the pretreatment pvalues correct? 
  expect_true(results$groupwise_treatment_effects$pre_treatment_p_value[1] > 0.05)
  expect_true(results$groupwise_treatment_effects$pre_treatment_p_value[2] > 0.05)
  
  # are one-sided pretreatment pvalues correct? 
  dta$y[dta$id %in% c(1,2)] <- dta$y[dta$id %in% c(1,2)] + dta$t[dta$id %in% c(1,2)]
  
  results <- simple_staggered_did(yname = "y",
                                  tname = "t",
                                  gname = "group",
                                  idname = "id",
                                  xformula = "x",
                                  varformula = c("z"),
                                  data = dta)


  expect_true(results$groupwise_treatment_effects$pre_treatment_p_value_left[1] > 0.05)
  expect_true(results$groupwise_treatment_effects$pre_treatment_p_value_right[1] < 0.05)
  
})



# test the number of residuals 