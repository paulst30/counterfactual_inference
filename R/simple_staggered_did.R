#' Staggered DiD with single treated units
#' 
#' @description
#' Estimates unit x time treatment effects, averages them on the unit-level and 
#' over all units. Also provides confidence intervals based on bootstrapped 
#' treatment effects.
#' 
#' @details
#' This functions implements a simple difference in difference estimator for 
#' staggered treatment adoption with few treated units and many controls. 
#' Following the the recent literature on staggered treatment adoption, this function 
#' estimates the unit x time treatment effects and aggregates them to derive the
#' average treatment effects (Callaway & Sant'Anna, 2021). For inference, this function 
#' implements the bootstrap method proposed by Alvarez & Ferman (2023), which 
#' produces confidence intervals of the correct size in situations with a 
#' fixed number of treated units and a large number of control units. 
#' 
#' @references Callaway, Brantly and Pedro H.C. Sant'Anna. 2021. "Difference-in-Differences with Multiple Time Periods." Journal of Econometrics, Vol. 225, No. 2, pp. 200-230. <https://arxiv.org/abs/1803.09015>
#' @references Alvarez, Luis and Bruno Ferman. 2023. "Extensions for Inference in Difference-in-Difference with Few Treated Clusters". <https://arxiv.org/pdf/2302.03131.pdf>
#' 
#' @param data A data frame or tibble containing all relevant variables.
#' @param yname Name of the outcome variable. The outcome variable needs to be numeric.
#' @param tname Name of the time variable. The time variable needs to be numeric.
#' @param gname Name of the variable indicating treatment timing. Needs to be a 
#' numeric variable indicating the period of first treatment for
#'  all observations of the treated units and is zero for all untreated units.
#' @param idname Name of the variable that uniquely identifies each unit. 
#' @param xformula A string vector containing all the variables that 
#' should enter the outcome model as control variables.
#' @param varformula A string vector containing all variables that determine 
#' the heteroscedasticity of the outcome model errors. 
#' @param universal_base Requires a logical input. If set to TRUE, pretreatment 
#' periods are evaluated against the first treatment period. If set to FALSE, 
#' pretreatment periods are evaluated against the following period. 
#' Default is FALSE. Treatment periods are always evaluated against the 
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
                                 varformula = NA, universal_base = FALSE, 
                                 control_group = "never_treated", data){
  
  #-----------------------------------------------------------------------------
  # Data Preparation #
  #-----------------------------------------------------------------------------
  
  #make sure data is a sorted data frame
  data <- as.data.frame(data)
  
  #expand data 
  expanded_dta <- expand.grid(unit = unique(data[,idname]),period = unique(data[,tname]))
  data <- merge(data,expanded_dta,by.x=c(idname,tname),by.y=c("unit","period"),all.y=T)
  data[,gname] <- ave(data[,gname], data[,idname], FUN = function(x) max(x, na.rm=T))
  
  #sort dataset
  data <- data[order(data[,idname], data[,tname], decreasing=FALSE),]
  
  #unpack inputs
  G <- (data[,gname]!=0)
  data$treated_unit <- as.numeric(G)
  tlist <- as.vector(unique(data[,tname]))
  glist <- as.vector(unique(data[G,gname]))
  ulist <- as.vector(unique(data[,idname]))
  
  #check if observations are uniquely identified
  duplicates <- duplicated(paste0(data[,idname],"_",data[,tname]))
  if (sum(duplicates)>0) {
    warning(paste("Variables", idname, "and", tname, "do not uniquely identify all observations."))
  }
  
  #set formulas for outcome and variance model
  if (max(!is.na(xformula))) {
    form <- as.formula(paste("delta ~ 1 + treated_unit +", paste(xformula, collapse =  "+"))) 
  } else {
    form <- as.formula(paste("delta ~ 1 + treated_unit"))
  }
  
  if (max(!is.na(varformula))) {
    var_form <- as.formula(paste("residuals^2 ~ 1 + ", paste(paste0("I(",varformula, "^-1)"), collapse =  "+"))) 
  } else {
    var_form <- as.formula(paste("residuals^2 ~ 1"))
  }
  
  # reduce dataset to necessary variables 
   necessary_var <- if(max(!is.na(varformula))) c(idname, gname, tname, varformula) else c(idname, gname, tname)
  
   
  # Set up output objects
  output <- list()                                           # empty output objects
  
  bootstraped_residuals <- data.frame(g=numeric(),           # data frame to track bootstraped residuals
                                      id=numeric(),
                                      B=numeric(),
                                      t=numeric(),
                                      boot_res=numeric(),
                                      tt=numeric())
  
  
  treatment_effects <- data.frame(data[G,necessary_var],     # data frame to track treatment effects for treated units
                                  att = NA,
                                  analy_crit_val =NA)
  
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
    necessary_var <- if(max(!is.na(xformula))) c(idname, gname, tname,"delta", xformula) else c(idname, gname, tname, "delta")
    n_missing <- complete.cases(data[,necessary_var])                  # indicates all observations for which a delta and all variables of the outcome model are non-missing
    
    # outcome residuals for specific group
    outcome_residuals <- list()
    
    
    #loop over time periods to estimate a outcome model for each period
    for (t in tlist) {
      
      # set current period
      posttreatment_period <- data[,tname]==t
      
      # Put together necessary data
      y <- as.data.frame(data[posttreatment_period & (C | G), c("delta", idname, tname)])
      row.names(y) <- NULL
      
      if (max(!is.na(xformula))) {
       X <- as.data.frame(data[(C | G) & data[,tname] == pret, c(idname, "treated_unit", xformula)] )
       row.names(X) <- NULL
      } else {
       X <- as.data.frame(data[(C | G) & data[,tname] == pret, c(idname, "treated_unit")])
       row.names(X) <- NULL
      }
     
      X <- merge(y,X, by.x=c(idname), by.y=c(idname), all.x= TRUE)
      
      #check if there are enough observations, skip iteration if not
      if (!max(complete.cases(X[X[,"treated_unit"]==1,]))) {
        warning(paste("Incomplete observation for treated unit", g,"in period", t))
        next
      } 
      
      if (!max(complete.cases(X[X[,"treated_unit"]!=1,]))) {
        warning(paste("No control units for treated unit", g,"in period", t))
        next
      } 
      
      # Estimate outcome model
        outcome_model <- lm(form, 
                          data = X)

        
      # save residuals and sample size of outcome model for later use
      index <- as.numeric(names(residuals(outcome_model)))
      residuals <- data.frame(data[posttreatment_period & (C | G),][index,], 
                               residuals = residuals(outcome_model))
      obs <- nrow(residuals)
      #outcome_residuals$inv_obs[outcome_residuals[,tname]==t] <- 1/obs
      outcome_residuals[[t]] <- residuals[residuals[,idname]!=g,]
      
      
      # save treatment effect and analytical standard errors
      treatment_effects[treatment_effects[,idname]==g & treatment_effects[,tname]==t, "att"] <- outcome_model$coefficients[2]
      treatment_effects[treatment_effects[,idname]==g & treatment_effects[,tname]==t, "analy_crit_val"] <- sqrt(diag(vcov(outcome_model))/obs)[2]*1.96
      
      # save number of treated observations
      #treatment_effects$inv_obs[treatment_effects[,idname]==g & treatment_effects[,tname]==t] <- 1/nrow(data[posttreatment_period & G,])
      
    }
    
    outcome_residuals <- do.call(rbind, outcome_residuals)

    #---------------------------------------------------------------------------
    # Estimate variance and control for heteroscedasticity
    #---------------------------------------------------------------------------
    
    #check if there are any treatment effects (and residuals) to work with
    if (length(outcome_residuals)==0) {
      warning(paste("No treatment effects calculated for unit", g))
      next
    }
    
    
    # use all the squared residuals from the outcome model an regress them on a constant and custom variables
      var_model <- lm(var_form,
                      data=outcome_residuals)
    
    # normalize the residuals by dividing them by their (expected) standard error
      outcome_residuals$norm_residuals <-outcome_residuals$residuals / sqrt(predict(object=var_model, data=outcome_residuals))

    # estimate the standard errors of the treated
      treatment_effects$var <- sqrt(predict(object = var_model, newdata = treatment_effects))

    #---------------------------------------------------------------------------
    # Bootstrap normalized residuals
    #---------------------------------------------------------------------------
      
        
      # identify unique controls that have non-missing residuals
      unique_controls <- unique(outcome_residuals[!is.na(outcome_residuals[,"norm_residuals"]), idname])
      
      # draw a sample of 200 control units to derive the bootstrapped residuals
      bootstrap_controls <- replicate(200,sample(unique_controls,size =  1 ,replace = TRUE), simplify = FALSE)
      
      # relevant treatment effects to be matched to the bootstrapped residuals
      var_treated <- treatment_effects[treatment_effects[,idname]==g, c(tname, "var", "att")]
      
      # create empty list to save results
      bootstraped_res <- list()
      
      # get the residuals of each bootstrapped control unit
     for (i in seq_along(bootstrap_controls)){
        control_id <- bootstrap_controls[[i]]
        b_res <- outcome_residuals[outcome_residuals[,idname]==control_id, c(tname, "norm_residuals")]
        b_res$g <- group
        b_res$id <- g
        b_res$B <- i
        b_res$treat_var <- var_treated$var[match(b_res[,tname], var_treated[,tname])]
        b_res$tt <- var_treated$att[match(b_res[,tname], var_treated[,tname])]
        b_res$boot_res <- b_res$norm_residuals * b_res$treat_var
        bootstraped_res[[i]] <- b_res
     }
      
      # consolidate list into one data frame with resized residuals
      bootstraped_res <- do.call(rbind, bootstraped_res)
      bootstraped_residuals <- rbind(bootstraped_residuals, bootstraped_res)
      

  }
  }
  
  #-----------------------------------------------------------------------------
  # Estimate confidence intervals
  #-----------------------------------------------------------------------------
  
  # Calculate group-time average effects----------------------------------------
  quantile_function <- function(x) {                                           # If there are bootstraped residuals for a unit+time observation...
    if (sum(!is.na(x$boot_res))>0 & nrow(x)>0) {
      maxima <- tapply(abs(x$boot_res), x[,tname], FUN=max, na.rm=T)           # Get maxima for each period t
      uniform_crit_val <- quantile(maxima, prob=0.95, na.rm=T)                 # calculate the 95% confidence value of maxima.
      crit_val <- tapply(abs(x$boot_res), x[,tname], FUN=function(x) quantile(x,prob=0.95, na.rm=T))

    } else if (sum(!is.na(x$boot_res))==0 & nrow(x)>0) {                       # If there is only missing data for a specific unit x time combination
      crit_val <- rep(NA, length(unique(x[,tname])))                           # set the critical value to NA also
      uniform_crit_val <- rep(NA, length(unique(x[,tname])))
    } else if (nrow(x)==0) {                                                   # If there is no data at all, return an empty vector
      crit_val <- numeric()
      uniform_crit_val <- numeric()
    }
    
    id <- unique(x$id)
    t <- unique(x[,tname])
    g <- unique(x$g)
    
    
    return(list(data.frame(id=id, g=g, t=t, crit_val=crit_val, uniform_crit_val=uniform_crit_val)))
    
  }
  bootstraped_grouptime_average <- sapply(split(bootstraped_residuals, as.formula(~id)), quantile_function)
  bootstraped_grouptime_average <- do.call(rbind, bootstraped_grouptime_average)
  bootstraped_grouptime_average$att <- treatment_effects$att[match(paste0(bootstraped_grouptime_average$id,bootstraped_grouptime_average$t),
                                                                   paste0(treatment_effects[,idname], treatment_effects[,tname]))]
  bootstraped_grouptime_average$analy_crit_val <- treatment_effects$analy_crit_val[match(paste0(bootstraped_grouptime_average$id,bootstraped_grouptime_average$t),
                                                                        paste0(treatment_effects[,idname], treatment_effects[,tname]))]
  
  
  # Calculate group-wise average effects----------------------------------------
  
  # group_wise average treatment effects are only computed for treated observations
  treated <- bootstraped_residuals[,tname] >= bootstraped_residuals$g                 # indicates treated periods
  

  # take the mean of all hypothetical atts for each unit and bootstrap iteration. 
  # This leads to a bootstrap sample (B=200) for each unit. 
  # Take the maxima of the 200 bootstraped means per unit.
  # The 95% quantile of all maxima represents the uniform ciritical value.
  
  quantile_function <- function(x) {
    id <- unique(x$id)
    g <- unique(x$g)
    treat_var <- ifelse(max(!is.na(x$treat_var))>0, unique(x$treat_var[!is.na(x$treat_var)]), NA)
    boot_mean <- tapply(x$norm_residuals, x$B, FUN = mean, na.rm=T)      # mean of each bootstrap draw
    crit_val <- quantile(boot_mean, prob=0.95, na.rm=T)*treat_var        # regular pointwise CI
    norm_maxima <- max(boot_mean)                                        # maximum of bootstraped means for uniform CI
    return(list(data.frame(norm_maxima=norm_maxima, treat_var=treat_var, id=id, g=g, crit_val=crit_val)))
  }
  bootstraped_groupwise_average <- sapply(split(bootstraped_residuals[treated,], as.formula(~id)), quantile_function)
  bootstraped_groupwise_average <- do.call(rbind, bootstraped_groupwise_average)
  
  # Use the maxima of bootstraped means to calculate 95% confidence intervals of the unit ATTs
  bootstraped_groupwise_average$uniform_crit_val <- quantile(abs(bootstraped_groupwise_average$norm_maxima), prob=0.95, na.rm=T)*bootstraped_groupwise_average$treat_var
  
  # calculate actual average atts
  treatment_effects$id <- treatment_effects[,idname]
  
  mean_function <- function(x) {
    index <- x[,tname]>=x[,gname]
    n <- nrow(x[index & !is.na(x$att),])
    mean_att <- mean(x$att[index], na.rm=T)
    analy_crit_val<- sd(x$att[index], na.rm = T)/sqrt(n)*1.96
    id <- unique(x[,idname])
    g <- unique(x[,gname])
    return(list(data.frame(id=id, g=g, att=mean_att, analy_crit_val=analy_crit_val)))
  }
  groupwise_atts <- sapply(split(treatment_effects, as.formula(~id)), mean_function)
  groupwise_atts <- do.call(rbind, groupwise_atts)
  bootstraped_groupwise_average$att <- groupwise_atts$att[match(bootstraped_groupwise_average$id, groupwise_atts$id)]
  bootstraped_groupwise_average$analy_crit_val <- groupwise_atts$analy_crit_val[match(bootstraped_groupwise_average$id, groupwise_atts$id)]
  
  
  # Calculate simple average effects--------------------------------------------
  
  # Take the mean of all hypothetical unit x time treatment effects for each bootstrap iteration B. 
  # Each of these means represents a hypothetical average ATT. 
  # These 200 hypothetical ATTs are used to calculate the 95% confidence interval.
  
  mean_function <- function(x) {
    mean_val <- mean(x$boot_res, na.rm=T)                                       # bootstraped ATT (already scaled)
    B <- unique(x$B)
    return(list(data.frame(mean_tt=mean_val, B=B)))
  }
  bootstraped_simple_average <- sapply(split(bootstraped_residuals[treated,], as.formula(~B)), mean_function)
  bootstraped_simple_average <- do.call(rbind, bootstraped_simple_average)

  # get the critical value 
  overall_crit_val <- quantile(abs(bootstraped_simple_average$mean_tt), prob=0.95, na.rm=T)

  # get sample size
  index <- treatment_effects[,tname]>=treatment_effects[,gname]
  n <- nrow(treatment_effects[index & !is.na(treatment_effects$att),])
  
  # calculate average treatment effect
  bootstraped_simple_average <- data.frame(att = mean(treatment_effects$att[index], na.rm=T),
                                           analy_crit_val = sd(treatment_effects$att[index], na.rm=T)/sqrt(n)*1.96,
                                           crit_val = overall_crit_val)

  
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






