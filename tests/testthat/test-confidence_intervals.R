
# test the group-time effects calculation
test_that("group time effects and confidence intervals", {
  test_data <- data.frame(boot_res = c(-1,-0.8,-0.6,-0.3,0,0.3,0.6,0.8,1),
                          time = c(1,1,1,1,1,1,1,1,1),
                          g = c(6,6,6,6,6,6,6,6,6),
                          id = c(1,1,1,1,1,1,1,1,1))
  
  tname <- "time"
  
  # Standard Case
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$crit_val, 1)
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$crit_val_left, -0.92)
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$crit_val_right, 0.92)
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$uniform_crit_val, 1)
  
  # Missing values in boot_res
  test_data[7, "boot_res"] <- NA
  
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$crit_val, 1)
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$crit_val_left, -0.93)
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$crit_val_right, 0.93)
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$uniform_crit_val, 1)
  
  
  # All values missing
  test_data[,"boot_res"] <- rep(NA, n=nrow(test_data))
  
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$crit_val, NA)
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$crit_val_left, NA)
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$crit_val_right, NA)
  expect_equal(cal_group_time_effects(test_data, tname)[[1]]$uniform_crit_val, NA)
  

})



# test the group-wise crit value caluculation
test_that("group_wise crit values",{
  test_data <- data.frame(norm_residuals = rep(c(-1,-0.8,-0.6,-0.3,0,0.3,0.6,0.8,1), each=5),
                          time = rep(c(1,2,3,4,5), times=9),
                          g = rep(c(6), times=45),
                          id = rep(c(1), times=45),
                          treat_var = rep(c(1),, times=45),
                          B = rep(c(1,2,3,4,5,6,7,8,9), each=5))  # 9 bootstrapped units
  
  tname <- "time"
  
  # Standard Case
  expect_equal(cal_group_crit_vals(test_data)[[1]]$crit_val, 1)
  expect_equal(cal_group_crit_vals(test_data)[[1]]$crit_val_left, -0.92)
  expect_equal(cal_group_crit_vals(test_data)[[1]]$crit_val_right, 0.92)
  expect_equal(cal_group_crit_vals(test_data)[[1]]$norm_maxima, 1)
  
  # All values missing
  test_data[,"norm_residuals"] <- rep(NA, times=nrow(test_data))
  expect_equal(cal_group_crit_vals(test_data)[[1]]$crit_val, as.numeric(NA))
  expect_equal(cal_group_crit_vals(test_data)[[1]]$crit_val_left, as.numeric(NA))
  expect_equal(cal_group_crit_vals(test_data)[[1]]$crit_val_right, as.numeric(NA))
  expect_equal(cal_group_crit_vals(test_data)[[1]]$norm_maxima, NA)
  
})
