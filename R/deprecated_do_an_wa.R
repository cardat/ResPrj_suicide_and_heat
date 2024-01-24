do_an_wa <- function(
    anomaly,
    gam_disagg_wa
){
  # Specified group
  gcc <- "5GPER"
  age_group <- "0–29"
  sex <- "M"
  
  # Initialize a data frame to store the results
  results <- data.frame(
    gcc = character(0),
    Age_Group = character(0),
    Sex = character(0),
    AN_CI = character(0),
    AN_CI_y = character(0)
  )
  
  # Calling the do_bootstrap function
  result <- do_bootstrap_wa(
    anomaly = anomaly,
    gam_disagg_wa = gam_disagg_wa,
    st = gcc,
    ag = age_group,
    sx = sex
  )
  
  # Formatting the AN, CI values and P-Value
  an_ci <- sprintf("%.2f (%.2f–%.2f)", 
                   result$attributable_deaths, 
                   result$ci_lower, 
                   result$ci_upper)
  
  an_ci_y <- sprintf("%.2f (%.2f–%.2f)", 
                     result$attributable_deaths/13, 
                     result$ci_lower/13, 
                     result$ci_upper/13)
  
  # Bind the results to the results data frame
  results <- rbind(results, data.frame(
    gcc = gcc,
    Age_Group = age_group,
    Sex = sex,
    AN_CI = an_ci,
    AN_CI_y = an_ci_y,
    stringsAsFactors = FALSE
  ))
  
  return(results)
  
}