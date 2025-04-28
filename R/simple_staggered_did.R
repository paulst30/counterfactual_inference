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
#' @param unitname Name of the variable that indicates the desired unit-level of the analysis, which should be the treatment level. Treatment effects are calculated at this unit x time level. Aggregated treatment effects are aggregated at this unit level.  
#' @param idname Name of the variable that identifies the lowest unit level. 
#' The combination of idname and tname must uniquely identify all observations. 
#' @param unitname Name of the variable that indicates the desired unit-level of the analysis, 
#' which should be the treatment level. Treatment effects are calculated at this unit x time level. 
#' Aggregated treatment effects are aggregated at this unit level. 
#' Unitname only needs to be specified, when the desired level of aggregation is not the idname. 
#' When not specified, unitname is assumed to be the same as idname.  
#' @param bstrap A number indicating how many control units should be drawn for the bootstrap sample.
#' When not specified, bstrap defaults to 200. 
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
#' @returns A list containing three data frames. 
#' The data frames contain the unit x time treatment effects, the unit-level 
#' average treatment effects, the overall average treatment effect. The data frame
#' with the unit-level average treatment effects also contains the p-value 
#' indicating whether the average pseudo treatment effect is significantly different
#' from zero.
#' @example examples.R
#' @export
simple_staggered_did <- function(yname, tname, gname, idname, unitname = idname,
                                 xformula = NA, varformula = NA, universal_base = FALSE, 
                                 control_group = "never_treated",
                                 bstrap = 200, data){
  
  #-----------------------------------------------------------------------------
  # Data Preparation #
  #-----------------------------------------------------------------------------
  
  #make sure data is a sorted data frame
  data <- as.data.frame(data)
  
  #check input data
  check_inputs(data, yname, gname,tname, idname, unitname)
  check_missings(data, gname, tname, idname, unitname)
  
  # check if any variables used in the variance estimation are zero or below one (which might cause NaNs)
  for (z in varformula) {
    if (any(data[,z]>-1 & data[,z]<1)) {
      warning(paste("Variable", z, "has values close zero. This might lead to NAs in confidence intervals.")) 
    }
  }
  
  #expand and sort data 
  data <- expand_sort_data(data, yname, gname, tname, idname, unitname)
  
  #unpack inputs
  G <- (data[,gname]!=0)
  data$treated_unit <- as.numeric(G)
  tlist <- as.vector(unique(data[,tname]))
  glist <- as.vector(unique(data[G,gname]))
  ulist <- as.vector(unique(data[,idname]))
  
  #check if observations are uniquely identified
  check_duplicates(data, idname, tname)

  
  #set formulas for outcome and variance model
  form <- get_x_formula(xformula)
  var_form <- get_var_formula(varformula)
  

    # reduce dataset to necessary variables 
   necessary_var <- if(max(!is.na(varformula))) c(unitname, gname, tname, varformula) else c(unitname, gname, tname)
  
   
  # Set up output objects
  output <- list()                                           # empty output objects
  
  bootstraped_residuals <- list()          # list to track bootstraped residuals
  treatment_effects <- list()              # to track treatment effects for treated units
  

  #-----------------------------------------------------------------------------
  # Estimate Outcome Model, Treatment Effects, and track Residuals
  #-----------------------------------------------------------------------------
  
  #loop over groups and units within groups
  for (group in glist) {
    treatlist <- as.vector(unique(data[data[,gname]==group, unitname]))
  for (g in treatlist) {
    
    # define treated unit, control unit, and treatment timing
    pret <- as.numeric(group)-1                               # last period before treatment
    G <- (data[,unitname] == g)                                   # treated unit
    data$treated_unit <- G
    C <- define_control_group(data, gname, tname, group, control_group)
    
    
    # estimate the difference to last pre-treatment period 
    # and if specified the first differences for pretreatment values
    data$delta <- difference_builder(data, idname, tname, yname, pret, universal_base)
    

    #are there any missings in the dataset?
    necessary_var <- if(max(!is.na(xformula))) c(unitname, gname, tname,"delta", xformula) else c(unitname, gname, tname, "delta")
    n_missing <- complete.cases(data[,necessary_var])                  # indicates all observations for which a delta and all variables of the outcome model are non-missing
    
    # outcome residuals and treatment for specific group/unit
    outcome_residuals <- list()
    treatment_effects_list <- list()
    
    
    #loop over time periods to estimate a outcome model for each period
    for (t in tlist) {
      outcome_list <- estimate_outcome_model(data, tname, idname, unitname, t, C, G, g, pret, xformula, form)
      
      if (!is.null(outcome_list[[1]])) {
        outcome_residuals[[t]] <- outcome_list[[1]]
        treatment_effects_list[[t]] <- outcome_list[[2]]
      }
      else {
        next
      }
      
    }

    outcome_residuals <- do.call(rbind, outcome_residuals)
    treatment_effects_list <- do.call(rbind, treatment_effects_list)
    
    # match treatment effects with variables required for variance model
    treatment_effects_list <- merge(treatment_effects_list, data[,c(varformula, gname, unitname, tname)], by=c(unitname, tname), all.x=TRUE)
    
    
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
                      data=outcome_residuals,
                      na.action = "na.exclude")

    # normalize the residuals by dividing them by their (expected) standard error
      suppressWarnings(outcome_residuals$norm_residuals <-outcome_residuals$residuals / sqrt(predict(object=var_model, data=outcome_residuals)))

    # estimate the standard errors of the treated
      treatment_effects_list$var <- sqrt(predict(object = var_model, newdata = treatment_effects_list))

    #---------------------------------------------------------------------------
    # Bootstrap normalized residuals
    #---------------------------------------------------------------------------

        
      # identify unique controls that have non-missing residuals
      unique_controls <- unique(outcome_residuals[!is.na(outcome_residuals[,"norm_residuals"]), unitname])
      
      # draw a sample of 200 control units to derive the bootstrapped residuals
      bootstrap_controls <- replicate(bstrap,sample(unique_controls,size =  1 ,replace = TRUE), simplify = FALSE)
      
      # relevant treatment effects to be matched to the bootstrapped residuals
      var_treated <- treatment_effects_list[treatment_effects_list[,unitname]==g, c(tname, "var", "att")]
      
      # create empty list to save results
      bootstraped_res <- list()
      
      # get the residuals of each bootstrapped control unit
     for (i in seq_along(bootstrap_controls)){
        control_id <- bootstrap_controls[[i]]
        b_res <- outcome_residuals[outcome_residuals[,unitname]==control_id, c(tname, "norm_residuals")]
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

      # save to results to lists
      treatment_effects[[g]] <- treatment_effects_list
      bootstraped_residuals[[g]] <- bootstraped_res

  }
  }

  treatment_effects <- do.call(rbind, treatment_effects)
  bootstraped_residuals <- do.call(rbind, bootstraped_residuals)

  #-----------------------------------------------------------------------------
  # Estimate confidence intervals
  #-----------------------------------------------------------------------------
 
  # Calculate group-time average effects----------------------------------------
  
  bootstraped_grouptime_average <- sapply(split(bootstraped_residuals, as.formula(~id)), cal_group_time_effects, tname=tname)
  bootstraped_grouptime_average <- do.call(rbind, bootstraped_grouptime_average)
  bootstraped_grouptime_average$att <- treatment_effects$att[match(paste0(bootstraped_grouptime_average$id,bootstraped_grouptime_average$t),
                                                                   paste0(treatment_effects[,unitname], treatment_effects[,tname]))]
  bootstraped_grouptime_average$analy_crit_val <- treatment_effects$analy_crit_val[match(paste0(bootstraped_grouptime_average$id,bootstraped_grouptime_average$t),
                                                                        paste0(treatment_effects[,unitname], treatment_effects[,tname]))]
  
  
  # Calculate group-wise average effects----------------------------------------
  
  # group_wise average treatment effects are only computed for treated observations
  treated <- bootstraped_residuals[,tname] >= bootstraped_residuals$g                 # indicates treated periods
  

  # take the mean of all hypothetical atts for each unit and bootstrap iteration. 
  # This leads to a bootstrap sample (B=200) for each unit. 
  # Take the maxima of the 200 bootstraped means per unit.
  # The 95% quantile of all maxima represents the uniform ciritical value.

  bootstraped_groupwise_average <- sapply(split(bootstraped_residuals[treated,], as.formula(~id)), cal_group_crit_vals)
  bootstraped_groupwise_average <- do.call(rbind, bootstraped_groupwise_average)
  
  # Use the maxima of bootstraped means to calculate 95% confidence intervals of the unit ATTs
  bootstraped_groupwise_average$uniform_crit_val <- quantile(abs(bootstraped_groupwise_average$norm_maxima), prob=0.95, na.rm=T)*bootstraped_groupwise_average$treat_var
  
  # calculate actual average atts
  treatment_effects$id <- treatment_effects[,unitname]

  groupwise_atts <- sapply(split(treatment_effects, as.formula(~id)), cal_group_atts, tname = tname, gname = gname, unitname = unitname)
  groupwise_atts <- do.call(rbind, groupwise_atts)
  bootstraped_groupwise_average$att <- groupwise_atts$att[match(bootstraped_groupwise_average$id, groupwise_atts$id)]
  bootstraped_groupwise_average$analy_crit_val <- groupwise_atts$analy_crit_val[match(bootstraped_groupwise_average$id, groupwise_atts$id)]
  
  
  
  
  # Calculate simple average effects--------------------------------------------
  
  # Take the mean of all hypothetical unit x time treatment effects for each bootstrap iteration B. 
  # Each of these means represents a hypothetical average ATT. 
  # These 200 hypothetical ATTs are used to calculate the 95% confidence interval.
  
  bootstraped_simple_average <- sapply(split(bootstraped_residuals[treated,], as.formula(~B)), cal_simple_averages_residuals)
  bootstraped_simple_average <- do.call(rbind, bootstraped_simple_average)

  # get the critical value 
  overall_crit_val <- quantile(abs(bootstraped_simple_average$mean_tt), prob=0.95, na.rm=T)
  crit_val_left <- quantile(bootstraped_simple_average$mean_tt, prob=0.05, na.rm=T)
  crit_val_right <- quantile(bootstraped_simple_average$mean_tt, prob=0.95, na.rm=T)

  # get sample size
  index <- treatment_effects[,tname]>=treatment_effects[,gname]
  n <- nrow(treatment_effects[index & !is.na(treatment_effects$att),])
  
  # calculate average treatment effect
  bootstraped_simple_average <- data.frame(att = mean(treatment_effects$att[index], na.rm=T),
                                           analy_crit_val = sd(treatment_effects$att[index], na.rm=T)/sqrt(n)*1.96,
                                           crit_val = overall_crit_val,
                                           crit_val_left = crit_val_left,
                                           crit_val_right = crit_val_right)

 
  # Pre-Treatment Trend test ---------------------------------------------------
 
  # calculate mean pseudo treatment effect

  pseudo_pre_treatment_atts <- sapply(split(treatment_effects[treatment_effects[, tname]<treatment_effects[,gname],], as.formula(~id)), cal_average_pseudo_atts, unitname = unitname, gname = gname)
  pseudo_pre_treatment_atts <- do.call(rbind, pseudo_pre_treatment_atts)
  
  # attach mean pseudo treatment effect to bootstraped residuals
  bootstraped_residuals$pseudo_att <- pseudo_pre_treatment_atts$pseudo_att[match(bootstraped_residuals$id,pseudo_pre_treatment_atts$id)]

  pre_treatment_p_value <- sapply(split(bootstraped_residuals[!treated,], as.formula(~id)), cal_pretreatment_pvalues)
  pre_treatment_p_value <-do.call(rbind, pre_treatment_p_value)
  
  # attach p-values to group-wise treatment effects
  bootstraped_groupwise_average$pre_treatment_p_value <- pre_treatment_p_value$p_value[match(bootstraped_groupwise_average$id, pre_treatment_p_value$id)]
  bootstraped_groupwise_average$pre_treatment_p_value_left <- pre_treatment_p_value$p_value_left[match(bootstraped_groupwise_average$id, pre_treatment_p_value$id)]
  bootstraped_groupwise_average$pre_treatment_p_value_right <- pre_treatment_p_value$p_value_right[match(bootstraped_groupwise_average$id, pre_treatment_p_value$id)]
  
  #-----------------------------------------------------------------------------
  # Output
  #-----------------------------------------------------------------------------
  
  output[["grouptime_treatment_effects"]] <- bootstraped_grouptime_average
  output[["groupwise_treatment_effects"]] <- bootstraped_groupwise_average
  output[["average_treatment_effect"]] <- bootstraped_simple_average
  #output[["bootstraped_residuals"]] <- bootstraped_residuals
  #output[["variance_model"]] <- var_model
  
  return(output)
}






