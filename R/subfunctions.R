
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
#' @keywords internal
check_duplicates <- function(data, idname, tname) {
  duplicates <- duplicated(paste0(data[,idname],"_",data[,tname]))
  if (sum(duplicates)>0) {
    stop(paste0("Variables ", idname, " and ", tname, " do not uniquely identify all observations. Consider specifying unitname (see documentation for further information)."))
  }
}

#' @title get x formula
#' @keywords internal
get_x_formula <- function(xformula, dta) {
  
   if (max(!is.na(xformula))) {
     
    # check if input is in the right format
    if (class(xformula)!="character"){
      stop("xformula needs to be a string vector.")
    }
     
    # check if inputs are in the data
     if (!all(xformula %in% names(dta))) {
       stop("Not all control variables specified in xformula are in the dataset.")
     }
    
    form <- as.formula(paste("delta ~ 1 + treated_unit +", paste(xformula, collapse =  "+"))) 
  } else {
    form <- as.formula(paste("delta ~ 1 + treated_unit"))
  }
  return(form)
}


#' @title get var formula
#' @keywords internal
get_var_formula <- function(varformula, dta) {

  if (max(!is.na(varformula))) {
    
    # check if input is in the right format
    if (class(varformula)!="character"){
      stop("varformula needs to be a string vector.")
    }
    
    # check if inputs are in the data
    if (!all(varformula %in% names(dta))) {
      stop("Not all control variables specified in varformula are in the dataset.")
    }
    
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
    index <- row.names(data)
    data <- data[order(data[,tname]),]
  }
  
  # calculate difference
  diff <- data.frame(diff= c(NA,diff(data[,yname], lag=1)))
  
  # ensure right indexes
  rownames(diff) <- rownames(data)
  
  # ensure original ordering
  if (exists("index", inherits = FALSE)){
    diff <- as.data.frame(diff[index,])
    row.names(diff) <- index
  }
  
  return(diff)  
}


#' @title difference_builder
#' @keywords internal
difference_builder <- function(data, idname, tname, yname, pret, universal_base) {
  
  # save initial index
  initial_index <- row.names(data)
  
  # calculate first differences
  diff_ref_val <- lapply(split(data, data[,idname]), FUN = diff_to_ref, tname = tname,yname = yname, pret = pret)
  new_index <- lapply(split(data, data[,idname]), FUN = row.names)

  # attach correct index
  new_index <- unlist(new_index)
  diff_ref_val <- as.data.frame(unlist(diff_ref_val))
  names(diff_ref_val) <- "diff_ref"
  row.names(diff_ref_val) <- new_index
  
  # add first differences before the treatment if universal_base is False
  if (!universal_base) {
    diff_val <- lapply(split(data, data[,idname]), FUN = diff_1lag, tname = tname,yname = yname, pret = pret)
    diff_val <- as.data.frame(unlist(diff_val))
    
    # recreate correct index
    names(diff_val) <- "diff"
    row.names(diff_val) <- new_index
    
    # merge by index
    diff_ref_val$diff_val <- diff_val[new_index,"diff"]
  }
  
  # restore old ordering
  diff_ref_val <- diff_ref_val[initial_index,]
  
  # assign final differenced variable
  diff_ref_val$delta <- ifelse(data[,tname]<=pret, diff_ref_val$diff_val, diff_ref_val$diff_ref)
  
  
  return(diff_ref_val$delta)
}


# estimate outcome model for each time period
#' @title outcome model estimator
#' @description
#' This function estimates the outcome model and the ATTs for one time period. It is supposed to 
#' in a loop over all time periods with t being the period of the current iteration. 
#' It takes the data as an input and returns a list with outcome_residuals and the treatment effects.  
#' @keywords internal
estimate_outcome_model <- function(data, tname, idname, unitname, t, C, G, g, pret, xformula, form) {

  # define list to store results
  outcome_list <- list()
  
  # set current period
  posttreatment_period <- data[,tname]==t
  
  # Put together necessary data
  y <- as.data.frame(data[posttreatment_period & (C | G), c("delta", unitname, tname, idname)])
  
  if (max(!is.na(xformula))) {
    X <- as.data.frame(data[(C | G) & data[,tname] == pret, c(unitname, "treated_unit", xformula, idname)] )
    row.names(X) <- NULL
  } else {
    X <- as.data.frame(data[(C | G) & data[,tname] == pret, c(unitname, "treated_unit", idname)])
    row.names(X) <- NULL
  }
  
  y <- y[y[,idname] %in% X[,idname],]
  
  
  X <- merge(y,X, by.x=c(idname), by.y=c(idname), all.x= TRUE)
  row.names(X) <- row.names(y)
  
  #check if there are enough observations, skip iteration if not
  if (!max(complete.cases(X[X[,"treated_unit"]==1,]))) {
    log_text <- paste("Incomplete observation for treated unit ", g," in period ", t,
                  ". Either the outcome variable is missing in period ", t," or ", pret, 
                  ", or one of the control variables is missing in ", pret)
    return(list(NULL,NULL))
  } 
  
  if (!max(complete.cases(X[X[,"treated_unit"]!=1,]))) {
    log_text <- paste("No control units for treated unit", g,"in period", t)
    return(list(NULL,NULL))
  } 
  
  
  # Estimate outcome model
  outcome_model <- lm(form, 
                      data = X)
  
  
  # save residuals and sample size of outcome model for later use
  index <- names(residuals(outcome_model))
  residuals <- data.frame(data[index,], 
                          residuals = residuals(outcome_model))
  obs <- nrow(residuals)
  outcome_list[[1]] <- residuals[residuals[,unitname]!=g,]
  
  # add log info if obs==2 -> no analytical crit value computable
  
  
  # save treatment effect and analytical standard errors
  treatment_effects <- data.frame(unit = g,
                                  time =t, 
                                  att = outcome_model$coefficients[2],
                                  analy_crit_val = sqrt(diag(vcov(outcome_model))/obs)[2]*1.96)
  names(treatment_effects) <- c(unitname, tname, "att", "analy_crit_val")
  outcome_list[[2]] <- treatment_effects
  return(outcome_list)
}

#-------------------------------------------------------------------------------
# Functions used to estimate confidence intervals
#-------------------------------------------------------------------------------



#' @title calculate group-time average effects
#' @description
#' It takes as input the residuals data for one treated unit (provided to it via
#' lappy(split())). This data includes all bootstraped residuals for every point
#' in time. Based on that, the function calculates the critical values (double-sided
#' left- and right-sided) and the uniform critical value.
#' @keywords internal
cal_group_time_effects <- function(x, tname) {                                           # If there are bootstraped residuals for a unit+time observation...
  
  if (sum(!is.na(x$boot_res))>0 & nrow(x)>0) {
    maxima <- tapply(abs(x$boot_res), x[,tname], FUN = function(v) {
      if (all(is.na(v))) NA else max(v, na.rm = TRUE)
    })                                                                       # Get maxima for each period t
    uniform_crit_val <- quantile(maxima, prob=0.95, na.rm=T)                 # calculate the 95% confidence value of maxima.
    crit_val <- tapply(abs(x$boot_res), x[,tname], FUN=function(x) quantile(x,prob=0.95, na.rm=T))
    crit_val_left <- tapply(x$boot_res, x[,tname], FUN=function(x) quantile(x,prob=0.05, na.rm=T))
    crit_val_right <- tapply(x$boot_res, x[,tname], FUN=function(x) quantile(x,prob=0.95, na.rm=T))
    
  } else if (sum(!is.na(x$boot_res))==0 & nrow(x)>0) {                       # If there is only missing data for a specific unit x time combination
    crit_val <- rep(NA, length(unique(x[,tname])))
    crit_val_left <- rep(NA, length(unique(x[,tname])))      # set the critical value to NA also
    crit_val_right <- rep(NA, length(unique(x[,tname])))
    uniform_crit_val <- rep(NA, length(unique(x[,tname])))
  } else if (nrow(x)==0) {                                                   # If there is no data at all, return an empty vector
    crit_val <- numeric()
    uniform_crit_val <- numeric()
  }
  
  id <- unique(x$id)
  t <- unique(x[,tname])
  g <- unique(x$g)
  
  return(list(data.frame(id=id, g=g, t=t, 
                         crit_val=crit_val,
                         crit_val_left=crit_val_left,
                         crit_val_right=crit_val_right,
                         uniform_crit_val=uniform_crit_val)))
  
}


#' @title calculate group-wise critical values
#' @description
#' It takes as input the residuals data for one treated unit (provided to it via
#' lappy(split())). This data includes all bootstraped residuals for every point
#' in time. Based on that, the function calculates the critical values (double-sided
#' left- and right-sided) and the uniform critical value on the group-level. To 
#' do this, it first takes the average for all bootstraped units. Then it uses 
#' the 'bstrp' averages to infer percentiles for the confidence intervals. 
#' 
#' For the uniform confidence interval, it extracts the maximum of the 'bstrap' 
#' averages. Later, these maxima (one for each treated unit/group) provide the 
#' basis for infering the 95% uniform confidence interval.
#' @keywords internal
cal_group_crit_vals <- function(x) {
  
  id <- unique(x$id)
  g <- unique(x$g)
  treat_var <- ifelse(max(!is.na(x$treat_var))>0, unique(x$treat_var[!is.na(x$treat_var)]), NA)
  boot_mean <- tapply(x$norm_residuals, x$B, FUN = mean, na.rm=T)      # mean of each bootstrap draw
  crit_val <- quantile(abs(boot_mean), prob=0.95, na.rm=T)*treat_var        # regular pointwise CI
  crit_val_left <- quantile(boot_mean, prob=0.05, na.rm=T)*treat_var        # left pointwise CI
  crit_val_right <- quantile(boot_mean, prob=0.95, na.rm=T)*treat_var        # right pointwise CI
  suppressWarnings(norm_maxima <- max(abs(boot_mean), na.rm = TRUE))                  # maximum of bootstraped means for uniform CI
  if(norm_maxima == -Inf) norm_maxima <- NA
  
  
  return(list(data.frame(norm_maxima=norm_maxima, 
                         treat_var=treat_var, 
                         id=id, 
                         g=g, 
                         crit_val=crit_val,
                         crit_val_left=crit_val_left,
                         crit_val_right=crit_val_right)))
}


# tests???
#' @title calculate group wise atts
#' @description
#' This function is applied to the dataframe for the treatment effects. 
#' It is applied to the treatment effects of all units separately (using sapply).
#' It then returns a dataframe including all the averaged treatment effects for
#' each id. 
#' @keywords internal
cal_group_atts <- function(x, tname, gname, unitname) {
  
  index <- x[,tname]>=x[,gname]
  n <- nrow(x[index & !is.na(x$att),])
  mean_att <- mean(x$att[index], na.rm=T)
  analy_crit_val<- sd(x$att[index], na.rm = T)/sqrt(n)*1.96
  id <- unique(x[,unitname])
  g <- unique(x[,gname])
  
  return(list(data.frame(id=id, g=g, att=mean_att, analy_crit_val=analy_crit_val)))
}


#' @title calculate simple average crit values
#' @description
#' This function is designed to be applied to the resized residuals of each 
#' bootstrap iteration. For each of these iterations, the average resized residual
#' is taken. Each of these means stands for a hypothetical simple average treatment
#' effect and can therefore be used to compute critical values later. 
#' @keywords internal
cal_simple_averages_residuals <- function(x) {
  mean_val <- mean(x$boot_res, na.rm=T)                                       # bootstraped ATT (already scaled)
  B <- unique(x$B)
  return(list(data.frame(mean_tt=mean_val, B=B)))
}


#' @title calculate mean pseudo treatment effects
#' @description
#' Much like the previous functions, this function computes the average of the 
#' pseudo treatment effects. It is designed to be applied to the dataframe of 
#' treatment effects for each treated unit separately (using sapply). The 
#' resulting averages can then be used to test the credibility of PTA. 
#' @keywords internal
cal_average_pseudo_atts <- function(x, unitname, gname) {
  mean_att <- mean(x$att, na.rm=T)
  id <- unique(x[,unitname])
  g <- unique(x[,gname])
  return(list(data.frame(id=id, g=g, pseudo_att=mean_att)))
}


#' @title calculate pretreatment tests
#' @description
#' This function calculates the critical values of the pseudo treatment effects
#' and derives a p-value, indicating whether the pseudo pretreatment effects are
#' different from the pseudo treatment effects of the controls. It is designed
#' to be applied to the bootstrapped residuals grouped by the treated unit they
#' were bootstrapped for. 
#' @keywords internal
cal_pretreatment_pvalues <- function(x) {
  
  id <- unique(x$id)
  g <- unique(x$g)
  pseudo_att <- unique(x$pseudo_att)
  treat_var <- ifelse(max(!is.na(x$treat_var))>0, unique(x$treat_var[!is.na(x$treat_var)]), NA)
  boot_mean <- tapply(x$norm_residuals, x$B, FUN = mean, na.rm=T)      # mean of each bootstrap draw
  p_value <- sum(abs(boot_mean*treat_var)>abs(pseudo_att),na.rm=T)/sum(!is.na(boot_mean))
  p_value_right <- sum(boot_mean*treat_var>pseudo_att,na.rm=T)/sum(!is.na(boot_mean))
  p_value_left <- sum(boot_mean*treat_var<pseudo_att,na.rm=T)/sum(!is.na(boot_mean))
  
  return(list(data.frame(id=id,
                         g=g,
                         p_value=p_value,
                         p_value_left = p_value_left,
                         p_value_right = p_value_right)))
}

