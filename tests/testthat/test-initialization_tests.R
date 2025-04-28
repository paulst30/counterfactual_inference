
# test check_inputs
test_that("input types", {
  test_data <- data.frame(y = c(1,1,2,2),
                          t = c(1,2,3,2),
                          id = c(1,1,1,2),
                          g = c(0,0,0,2),
                          u = c(1,1,1,1))
  
  expect_error(check_inputs(test_data, 
                      yname="y", 
                      tname="t", 
                      idname="id", 
                      gname = "g", 
                      unitname= 1))
  
  expect_error(check_inputs(test_data, 
                            yname="y", 
                            tname="t", 
                            idname="id", 
                            gname = "g", 
                            unitname= test_data[,"u"]))

  expect_error(check_inputs(test_data, 
                            yname="y", 
                            tname="t", 
                            idname="id", 
                            gname = "g", 
                            unitname= "un"))
})


# test missing errors
test_that("check missing values", {
  test_data <- data.frame(y = c(1,1,2,2),
                          t = c(1,2,3,2),
                          id = c(1,1,1,2),
                          g = c(0,0,NA,1),
                          u = c(1,1,1,1))
  
  expect_error(check_missings(test_data,"g","t","id","u"))
})


# test data types
test_that("check data types", {
  test_data <- data.frame(y = c(1,1,2,2),
                          t = c(1,2,3,2),
                          id = c(1,1,1,2),
                          g = c(0,0,0,1),
                          u = c(1,1,1,1))
  test_data$g <- as.character(test_data$g)
  
  expect_error(check_data_types(test_data, c("g")))
  
})


# test expand data
test_that("expand sort data", {
  test_data <- data.frame(y = c(1,1,2,2),
                          t = c(1,2,3,2),
                          id = c(1,1,1,2),
                          g = c(0,0,0,1),
                          u = c(1,1,1,1))
  expected_length <- length(unique(test_data$id))*length(unique(test_data$t))
  exp_data <- expand_sort_data(test_data,"y","g","t","id","u")
  expect_true(nrow(exp_data)==expected_length)
})



# test check_duplicates
test_that("check_duplicates", {
  test_data <- data.frame(y = c(1,1,2,2),
                          t = c(1,2,3,2),
                          id = c(1,1,1,2),
                          g = c(0,0,0,1),
                          u = c(1,1,1,1))
  
  expect_no_error(check_duplicates(test_data, "id", "t"))
  
  test_data[2,"id"] <- 2
  expect_error(check_duplicates(test_data, "id", "t"), 
               regexp = "Variables id and t do not uniquely identify")
})


#test get_formula
test_that("get formula", {
  x <-c("x1", "x2")
  var <- c("z1" , "z2")
  
  form <- get_x_formula(x)
  var_form <- get_var_formula(var)
  
  # correct output
  expect_true(as.formula("delta ~ 1 + treated_unit + x1 + x2")==get_x_formula(x))
  expect_true(as.formula("residuals^2 ~ 1 + I(z1^-1) + I(z2^-1)")==get_var_formula(var))
  
})











































