library(devtools)
library(roxygen2)
 
#' Staggered DiD with single treated units
#' 
#' Estimates unit x time treatment effects, averages them on the unit-level and 
#' over all units. Also provides confidence intervals based on bootstrapped 
#' treatment effects.
#' @param data A data frame or tibble containing all relevant variables.
#' @param yname Name of the outcome variable. Needs too be numeric.
#' @param tname Name of the time variable. Needs to be numeric.
#' @param gname Variable indicating treatment timing. Needs to be a 
#' numeric variable indicating the period of first treatment for
#'  all observations of the treated units and is zero for all untreated units.
#' @param idname Variable unique identifying each unit. 
#' @param xformula A string vector containing all the variables that 
#' should enter the outcome model as control variables.
#' @param varformula A string vector containing all variables that determine 
#' the heteroscedasticity of the outcome model errors. 
#' @param universal_base Requires a logical input. If set to TRUE, pretreatment 
#' periods are evaluated against the first treatment period. If set to FALSE, 
#' pretreatment periods are evaluated against the follow period. 
#' Defaults to FALSE. Treatment periods are always evaluated against the 
#' last pretreatment period.
#' @param control_group A string either containing "never_treated" or 
#' "not_yet_treated". In case of the former, only never-treated units 
#' are used as controls. In case of the latter, the pretreatment periods 
#' of not-yet-treated units are added to the pool of controls.
#' 
#' @returns A list containing four data frames and an lm object. 
#' The data frames contain the unit x time treatment effects, the unit-level 
#' average treatment effects, the overall average treatment effect, and the 
#' bootstrapped residuals used to calculate confidence intervals. 
#' The lm object contains the details on the variance model.
#' @example examples.R
#' @export
simple_staggered_did <- function(yname, tname, gname, idname, xformula = NA, 
                                 varformula = NA, universal_base = FALSE,control_group = "never_treated", data){
  
  #-----------------------------------------------------------------------------
  # Data Preparation #
  #-----------------------------------------------------------------------------
  
  #make sure data is a sorted data frame
  data <- as.data.frame(data)
  #sort dataset
  data <- data[order(data[,idname], data[,tname], decreasing=FALSE),]
  
  #unpack inputs
  G <- (data[,gname]!=0)
  tlist <- as.vector(unique(data[,tname]))
  glist <- as.vector(unique(data[G,gname]))
  ulist <- as.vector(unique(data[,idname]))
  
  #set formulas for outcome and variance model
  if (!is.na(xformula)) {
    form <- as.formula(paste("delta ~ 1 + ", paste(xformula, collapse =  "+"))) 
  } else {
    form <- as.formula(paste("delta ~ 1"))
  }
  
  if (!is.na(varformula)) {
    var_form <- as.formula(paste("residuals^2 ~ 1 + ", paste(varformula, collapse =  "+"))) 
  } else {
    var_form <- as.formula(paste("residuals^2 ~ 1"))
  }
  
  # reduce dataset to necessary variables 
   necessary_var <- if(!is.na(varformula)) c(idname, gname, tname, varformula) else c(idname, gname, tname)
  
   
  # Set up output objects
  output <- list()                                           # empty output objects
  
  bootstraped_residuals <- data.frame(g=numeric(),           # data frame to track bootstraped residuals
                                      id=numeric(),
                                      B=numeric(),
                                      t=numeric(),
                                      boot_res=numeric(),
                                      tt=numeric())
  
  
  treatment_effects <- data.frame(data[G,necessary_var],     # data frame to track treatment effects for treated units
                                  att = NA)
  
  #-----------------------------------------------------------------------------
  # Estimate Outcome Model, Treatment Effects, and track Residuals
  #-----------------------------------------------------------------------------
  
  #loop over groups and units within groups
  for (group in glist) {
    treatlist <- as.vector(unique(data[data[,gname]==group, idname]))
  for (g in treatlist) {
    
    # define treated unit, control unit, and treatment timing
    pret <- as.numeric(group)-1                               # last period before treatment
    G <- data[,idname] == g                                   # treated unit
    C <- data[,gname] == 0                                    # all never-treated units
    
    # include not-yet-treated units in the control group if specified
    if (control_group=="not_yet_treated") {
      control <- data[,gname] == 0                            # never-treated units again
      not_yet_treated <- !(data[,gname] %in% c(0, group)) &   # exclude units that are never treated and are treated simultaneously
                          (data[, tname] < data[,gname]) &    # include only time periods before units are treated themselves
                          (data[,gname] > group)              # include only units that are treated after the current treated unit
      C <- (control | not_yet_treated)
    }
    
    
    # estimate the difference to last pre-treatment period 
    data$delta <- NA
    for (id in ulist){
      group_indices <- data[, idname] == id 
      pretreatment_period <- data[,tname]== pret                                      # base periods for all units
      reference_value <- as.numeric(data[group_indices & pretreatment_period, yname]) # value in the base period for every unit
      if (length(reference_value)==0) {                                               # skip to the next iteration if the base period is missing
        print(paste0("Base period (",pret ,") is missing for unit ",id ))
        next
      }
      data[group_indices, "delta"] <- data[group_indices, yname] - reference_value    # calculate changes in the outcome variable (delta)
    }
    
    # change pretreatment delta to first differences if base period is not "universal"
    if (!universal_base) {
      difference <- function(x){                                                # function returns a vector of first differences where possible and NA otherwise
        first_diff <- ifelse(is.na(x[,yname]) | is.na(c(tail(x[,yname], -1), NA)) , NA ,diff(x[,yname], lead=1))
        return(list(first_diff))
      }
      first_diff <-  sapply(split(data, data[,idname]), FUN = difference)
      first_diff <- unlist(first_diff)
      
      data$first_diff <- first_diff
      
      # replace delta for pretreatment periods
      data$delta <- ifelse(data[, tname]<=pret, data$first_diff, data$delta)
    }
    
    #are there any missings in the dataset?
    necessary_var <- if(!is.na(xformula)) c(idname, gname, tname,"delta", xformula) else c(idname, gname, tname, "delta")
    n_missing <- complete.cases(data[,necessary_var])                  # indicates all observations for which a delta and all variables of the outcome model are non-missing
    
    # outcome residuals for specific group
    outcome_residuals <- data.frame(data[C & n_missing,],              # data frame that will be populated with the residuals of the outcome model
                                    residuals = NA)
    
    
    #loop over time periods to estimate a outcome model for each period
    for (t in tlist) {
      
      # set current period
      posttreatment_period <- data[,tname]==t
      
      # estimate outcome model
      #check if there is enough data to estimate the outcome model
      obs <- sum(complete.cases(data[posttreatment_period & C,]))
      
      if (obs){                                                                 # if there are no control units for the current period skip to the next iteration
        outcome_model <- lm(form, 
                          data = data,
                          subset = (posttreatment_period & C))
      } else {
        warning(paste("Not enough observations in the control group to estimate outcome model in period ", t))
        next
      }
        
      
      # save residuals and sample size of outcome model for later use
      outcome_residuals[outcome_residuals[,tname]==t, "residuals"] <- residuals(outcome_model)
      outcome_residuals$inv_obs[outcome_residuals[,tname]==t] <- 1/obs
      
      
      # save treatment effect 
      d_untreated <- mean(predict(object = outcome_model, newdata=data[posttreatment_period & G,]))
      d_treated <- mean(data[posttreatment_period & G, "delta"])
      
      treatment_effects[treatment_effects[,idname]==g & treatment_effects[,tname]==t, "att"] <- d_treated-d_untreated
      
      # save number of treated observations
      treatment_effects$inv_obs[treatment_effects[,idname]==g & treatment_effects[,tname]==t] <- 1/nrow(data[posttreatment_period & G,])
      
    }
  
    #---------------------------------------------------------------------------
    # Estimate variance and control for heteroscedasticity
    #---------------------------------------------------------------------------
    
    # use all the squared residuals from the outcome model an regress them on a constant and custom variables
      var_model <- lm(var_form,
                      data=outcome_residuals)
    
    # normalize the residuals by dividing them by their (expected) variance
      outcome_residuals$norm_residuals <-outcome_residuals$residuals / predict(object=var_model, data=outcome_residuals)

    # estimate the variance of the treated
      treatment_effects$var <- predict(object = var_model, newdata = treatment_effects)
      
    #---------------------------------------------------------------------------
    # Bootstrap normalized residuals
    #---------------------------------------------------------------------------
      
    # pull bootstraped normalized residuals for each period separately
    # This ensures that each period is represented equally in the bootstraped residuals
      for (t in tlist){
        treatment_indices <- treatment_effects[, tname] == t & treatment_effects[,idname] == g
        
        if (sum(treatment_indices)==1) {                                                               # there should be just one period (if any)
        bootstrap_residuals_gt <- sample(outcome_residuals$norm_residuals, size = 200, replace = T)    # sample 200 normalized residuals with replacement
        bootstrap_residuals_gt <- bootstrap_residuals_gt*treatment_effects[treatment_indices,"var"]    # resize the residuals by the variance of the treated unit
        
        #save the residuals for confidence interval calculation
        append <- data.frame(id = g, group = group, t=t, B=(1:200), boot_res = bootstrap_residuals_gt, tt = treatment_effects[treatment_indices, "att"])
        bootstraped_residuals <- rbind(bootstraped_residuals, append)
        
        }
      }
  }
  }
  
  #-----------------------------------------------------------------------------
  # Estimate confidence intervals
  #-----------------------------------------------------------------------------
  
  # Calculate group-time average effects----------------------------------------
  quantile_function <- function(x) {                                           # If there are bootstraped residuals for a unit+time observation...
    if (sum(!is.na(x$boot_res))>0 & nrow(x)>0) {
      res <- x$boot_res 
      crit_val <- quantile(abs(res), prob=0.95, na.rm=T)                       # calculate the 95% confidence value. 
      att<- x$tt
    } else if (sum(!is.na(x$boot_res))==0 & nrow(x)>0) {                       # If there is only missing data for a specific unit x time combination
      res <- NA                                                                # set the critical value to NA also
      crit_val <- NA
      att <- NA
    } else if (nrow(x)==0) {                                                   # If there is no data at all, return an empty vector
      res <- numeric()
      crit_val <- numeric()
      att <- numeric()
    }
    
    id <- x$id
    t <- x$t
    return(list(data.frame(id=id, t=t, att = att, crit_val=crit_val)))
  }
  bootstraped_grouptime_average <- sapply(split(bootstraped_residuals, as.formula(~id+t)), quantile_function)
  bootstraped_grouptime_average <- do.call(rbind, bootstraped_grouptime_average)
  bootstraped_grouptime_average <- unique(bootstraped_grouptime_average) 
  
  
  # Calculate group-wise average effects----------------------------------------
  
  # group_wise average treatment effects are only computed for treated observations
  treated <- bootstraped_residuals$t >= bootstraped_residuals$g                 # indicates treated periods
  
  # calculate possible atts based on bootstraped residuals (these hypothetical atts will serve as bootsstrap sample)
  bootstraped_residuals$boot_tt <- bootstraped_residuals$tt + bootstraped_residuals$boot_res
  
  # take the mean of all hypothetical atts for each unit and bootstrap iteration. 
  # This leads to a bootstrap sample (B=200) for each unit. 
  # Each of the 200 ATTs represent a possible value for the unit ATT based on 
  # bootstraped unit x time treatment effects.
  
  mean_function <- function(x) {
    mean_val <- mean(x$boot_tt, na.rm=T)                                      # bootstraped ATT 
    tt <- mean(x$tt, na.rm=T)                                                 # actual ATT
    res <- tt - mean_val                                                      # How much does the each bootstraped ATT deviate from the actual ATT
    id <- unique(x$id)
    B <- unique(x$B)
    return(list(data.frame(res=res, mean_tt=tt, id=id, B=B)))
  }
  bootstraped_groupwise_average <- sapply(split(bootstraped_residuals[treated,], as.formula(~B+id)), mean_function)
  bootstraped_groupwise_average <- do.call(rbind, bootstraped_groupwise_average)
  
  # Use the 200 bootstraped ATTs to calculate 95% confidence intervals of the unit ATTs
  quantile_function <- function(x) {
    crit_val <- quantile(abs(x$res), prob=0.95, na.rm=T)
    att<- unique(x$mean_tt)
    id <- x$id
    return(list(data.frame(id=id, att=att, crit_val=crit_val)))
  }
  bootstraped_groupwise_average <- sapply(split(bootstraped_groupwise_average, as.formula(~id)), quantile_function)
  bootstraped_groupwise_average <- do.call(rbind, bootstraped_groupwise_average)
  bootstraped_groupwise_average <- unique(bootstraped_groupwise_average) 
  
  
  # Calculate simple average effects--------------------------------------------
  
  # Take the mean of all hypothetical unit x time treatment effects for each bootstrap iteration B. 
  # Each of these means represents a hypothetical ATT. The deviance from the actual ATT is 
  # used to calculate the 95% confidence intervals.
  
  mean_function <- function(x) {
    mean_val <- mean(x$boot_tt, na.rm=T)                                       # bootstraped ATT
    tt <- mean(x$tt, na.rm=T)                                                  # actual ATT
    res <- tt - mean_val                                                       # deviance from the actual ATT
    B <- unique(x$B)
    return(list(data.frame(res=res, mean_tt=tt, B=B)))
  }
  bootstraped_simple_average <- sapply(split(bootstraped_residuals[treated,], as.formula(~B)), mean_function)
  bootstraped_simple_average <- do.call(rbind, bootstraped_simple_average)
  
  # used the 200 bootstraped ATTs to calculate 95% confidence intervals
  quantile_function <- function(x) {
    crit_val <- quantile(abs(x$res), prob=0.95, na.rm=T)
    att<- unique(x$mean_tt)
    return(data.frame(att=att, crit_val=crit_val))
  }
  bootstraped_simple_average <- quantile_function(bootstraped_simple_average)
  
  
  
  #-----------------------------------------------------------------------------
  # Output
  #-----------------------------------------------------------------------------
  
  output[["grouptime_treatment_effects"]] <- bootstraped_grouptime_average
  output[["groupwise_treatment_effects"]] <- bootstraped_groupwise_average
  output[["average_treatment_effect"]] <- bootstraped_simple_average
  output[["bootstraped_residuals"]] <- bootstraped_residuals
  output[["variance_model"]] <- var_model
  
  return(output)
}






