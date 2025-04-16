
#' @title check input data
#' @keywords internal 
check_inputs <- function(data, yname, gname,tname, idname, unitname){
  
  for (var in list(yname, gname, idname, unitname)){
    if (!is.character(var)){
      stop('Variable inputs need to be strings.')
    }
    if (!(var %in% colnames(data))){
      stop('Variable inputs need to columns in the data.')
    }
  }
}

#' @title missing values
#' @keywords internal
check_missings <- function(data, gname, tname, idname, unitname){
  if (any(is.na(data[,gname]))){
    stop("Group variable cannot have missing values.")
  }
  if (any(is.na(data[,unitname]))){
    stop("Unit variable cannot have missing values.")
  }
  if (any(is.na(data[,tname]))){
    stop("Time variable cannot have missing values.")
  }
  if (any(is.na(data[,idname]))){
    stop("Unit variable cannot have missing values.")
  }
}

#' @title data types
#' @keywords internal
check_data_types <- function(data, vars){
  for (var in vars){
    if (class(data[,var])!="numeric"){
      stop(paste(var, "needs to be numeric."))
    }
  }
}


#' @title expand data
#' 
#' @keywords internal
expand_sort_data <- function(data, yname, gname, tname, idname, unitname){

  expanded_dta <- expand.grid(unit = unique(data[,idname]),period = unique(data[,tname]))
  data <- merge(data,expanded_dta,by.x=c(idname,tname),by.y=c("unit","period"),all.y=T)
  data[,gname] <- ave(data[,gname], data[,idname], FUN = function(x) max(x, na.rm=T))
  data[,unitname] <- ave(data[,unitname], data[,idname], FUN = function(x) max(x, na.rm=T))
  
  #sort data set
  data <- data[order(data[,idname], data[,tname], decreasing=FALSE),]
  return(data)
}


#' @title check duplicates
#' @keyword internal
check_duplicates <- function(data, idname, tname) {
  duplicates <- duplicated(paste0(data[,idname],"_",data[,tname]))
  if (sum(duplicates)>0) {
    stop(paste0("Variables ", idname, " and ", tname, " do not uniquely identify all observations. Consider specifying unitname (see documentation for further information)."))
  }
}

#' @title get x formula
#' @keywords internal
get_x_formula <- function(xformula) {
  
  # check if input is in the right format
  if (class(xformula)!="character"){
    stop("xformula needs to be a string vector.")
  }
  
  if (max(!is.na(xformula))) {
    form <- as.formula(paste("delta ~ 1 + treated_unit +", paste(xformula, collapse =  "+"))) 
  } else {
    form <- as.formula(paste("delta ~ 1 + treated_unit"))
  }
  return(form)
}


#' @title get var formula
#' @keywords internal
get_var_formula <- function(varformula) {
  
  # check if input is in the right format
  if (class(varformula)!="character"){
    stop("varformula needs to be a string vector.")
  }
  

  if (max(!is.na(varformula))) {
    var_form <- as.formula(paste("residuals^2 ~ 1 + ", paste(paste0("I(",varformula, "^-1)"), collapse =  "+"))) 
  } else {
    var_form <- as.formula(paste("residuals^2 ~ 1"))
  }
  return(var_form)
}


#-------------------------------------------------------------------------------
# Functions used to estimate outcome model treatment effects and track residuals
#-------------------------------------------------------------------------------

#' @title define control group and treated unit
#' @keywords internal
define_control_group <- function(data, gname, tname, group, control_group){
  
  # default: use all never-treated units as controls
  C <- (data[,gname] == 0)
  
  # include not-yet-treated units in the control group if specified
  if (control_group=="not_yet_treated") {
    control <- data[,gname] == 0                            # never-treated units again
    not_yet_treated <- !(data[,gname] %in% c(0, group)) &   # exclude units that are never treated and are treated simultaneously
                        (data[, tname] < data[,gname]) &    # include only time periods before units are treated themselves
                        (data[,gname] > group)              # include only units that are treated after the current treated unit
    C <- (control | not_yet_treated)
    
  }
  return(C)
}


#' @title Difference to reference value calculator
#' @keywords internal
diff_to_ref <- function(data, tname, yname, pret) {
  
  # last value before treatment
  reference_value <- data[data[, tname]==pret ,yname]
  
  # difference to reference value
  delta <- data.frame(delta = data[,yname]-reference_value)
  
  # ensure right indexes
  rownames(delta) <- rownames(data)
  
  return(delta)
}


#' @title simple difference calculator
#' @keywords internal
diff_1lag <- function(data, tname, yname, pret) {
  
  # check whether data is sorted
  if (!(all(data[, tname]==sort(data[,tname])))){
    data <- data[order(data[,tname]),]
  }
  
  # calculate difference
  diff <- data.frame(diff= c(NA,diff(data[,yname], lag=1)))
  
  # ensure right indexes
  rownames(diff) <- rownames(data)
  
  return(diff)  
}


#' @title difference_builder
#' @keywords internal
difference_builder <- function(data, idname, tname, yname, pret) {
  
  
  
}





