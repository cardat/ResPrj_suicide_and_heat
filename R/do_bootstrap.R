do_bootstrap <- function(
    anomaly, 
    gam_disagg, 
    st, 
    ag,
    sx
){

  # Subset data based on specified gcc and age group
  # ste <- subset(anomaly, gcc == st & age_group == ag & sex == sx)
  
  ste <- anomaly
  
  # Create interaction terms for Tmax anomaly by sex and age group
  ste <- ste[, `:=`(
    TmaxMales10_29 = ifelse(age_group == '0–29' & sex == 'M', tmax_anomaly, 0),
    TmaxMales30_54 = ifelse(age_group == '30–54' & sex == 'M', tmax_anomaly, 0),
    TmaxMales55plus = ifelse(age_group == '55+' & sex == 'M', tmax_anomaly, 0),
    TmaxFemales10_29 = ifelse(age_group == '0–29' & sex == 'F', tmax_anomaly, 0),
    TmaxFemales30_54 = ifelse(age_group == '30–54' & sex == 'F', tmax_anomaly, 0),
    TmaxFemales55plus = ifelse(age_group == '55+' & sex == 'F', tmax_anomaly, 0)
  )]
  
  # Create a counterfactual dataset with no anomalies
  ste_no_anomaly <- copy(ste)
  ste_no_anomaly[, `:=`(
    TmaxMales10_29 = 0, 
    TmaxMales30_54 = 0, 
    TmaxMales55plus = 0, 
    TmaxFemales10_29 = 0, 
    TmaxFemales30_54 = 0, 
    TmaxFemales55plus = 0,
    tmax_anomaly = 0
  )]
  
  # Predict deaths with anomaly
  ste[, predicted_deaths := predict(
    gam_disagg, newdata = ste, type = "response")]
  
  # Predict deaths without anomaly
  ste_no_anomaly[, predicted_deaths_no_anomaly := predict(
    gam_disagg, newdata = ste_no_anomaly, type = "response")]
  
  # Print to diagnose
  print(head(ste$predicted_deaths))
  print(head(ste_no_anomaly$predicted_deaths_no_anomaly))
  
  # Calculate attributable deaths
  attributable_deaths <- sum(
    ste$predicted_deaths - ste_no_anomaly$predicted_deaths_no_anomaly)
  
  # Function to calculate attributable deaths for bootstrapping
  an <- function(
    bootstrap_indices
    ){
    bootstrap_ste <- ste[bootstrap_indices, , drop = FALSE]
    bootstrap_ste_no_anomaly <- ste_no_anomaly[
      bootstrap_indices, , drop = FALSE]
    predicted_deaths <- predict(
      gam_disagg, 
      newdata = bootstrap_ste, 
      type = "response")
    predicted_deaths_no_anomaly <- predict(
      gam_disagg, 
      newdata = bootstrap_ste_no_anomaly, 
      type = "response")
    sum(predicted_deaths - predicted_deaths_no_anomaly)
  }
  
  # Number of bootstrap samples
  n_bootstrap <- 1000
  bootstrap_attributable_deaths <- numeric(n_bootstrap)
  
  # Bootstrap sampling
  for(i in seq_len(n_bootstrap)) {
    bootstrap_indices <- sample(seq_len(nrow(ste)), replace = TRUE)
    bootstrap_attributable_deaths[i] <- an(bootstrap_indices)
  }
  
  # Compute the confidence interval
  ci_lower <- quantile(bootstrap_attributable_deaths, 0.025)
  ci_upper <- quantile(bootstrap_attributable_deaths, 0.975)
  
  # Return the attributable deaths and confidence interval
  return(list(
    attributable_deaths = attributable_deaths,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}
