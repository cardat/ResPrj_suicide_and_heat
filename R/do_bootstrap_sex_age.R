do_bootstrap_sex_age <- function(
    anomaly,
    gam_sex_age
){
  
  # Predicted deaths with anomalies
  anomaly$predicted_deaths_anomaly <- predict(
    gam_sex_age, 
    newdata = anomaly, 
    type = "response")
  
  # Set temperature anomalies to zero for baseline scenario
  baseline <- anomaly
  baseline$TmaxMales10_29 <- 0
  baseline$TmaxMales30_54 <- 0
  baseline$TmaxMales55plus <- 0
  baseline$TmaxFemales10_29 <- 0
  baseline$TmaxFemales30_54 <- 0
  baseline$TmaxFemales55plus <- 0
  
  # Predicted deaths for baseline scenario
  baseline$predicted_deaths_baseline <- predict(
    gam_sex_age, 
    newdata = baseline, 
    type = "response")
  
  # Attributable deaths
  baseline$attributable_deaths <- baseline$predicted_deaths_anomaly - baseline$predicted_deaths_baseline
  
  do_an <- function(
    bootstrap_indices,
    anomaly_subset,
    baseline_subset
  ){
    bootstrap_anomaly <- anomaly_subset[bootstrap_indices, , drop = FALSE]
    bootstrap_baseline <- baseline_subset[bootstrap_indices, , drop = FALSE]
    
    predicted_deaths <- predict(gam_sex_age, newdata = bootstrap_anomaly, type = "response")
    predicted_deaths_baseline <- predict(gam_sex_age, newdata = bootstrap_baseline, type = "response")
    
    # Calculate attributable deaths for each data point
    attributable_deaths_per_point <- predicted_deaths - predicted_deaths_baseline
    
    # Sum the attributable deaths across all data points
    total_attributable_deaths <- sum(attributable_deaths_per_point, na.rm = TRUE)
    
    return(total_attributable_deaths)
  }
  
  
  # Number of bootstrap samples
  n_bootstrap <- 1000
  # Initialise placeholders for storing results
  bootstrap_attributable_deaths_M <- numeric(n_bootstrap)
  bootstrap_attributable_deaths_F <- numeric(n_bootstrap)
  
  # Perform bootstrapping separately for males and females
  for(sex in c("M", "F")) {
    for(i in seq_len(n_bootstrap)) {
      bootstrap_indices <- sample(seq_len(nrow(anomaly[anomaly$sex == sex,])), 
                                  replace = TRUE)
      if (sex == "M") {
        bootstrap_attributable_deaths_M[i] <- do_an(bootstrap_indices, 
                                                    anomaly[anomaly$sex == "M", ], 
                                                    baseline[baseline$sex == "M", ])
      } else {
        bootstrap_attributable_deaths_F[i] <- do_an(bootstrap_indices, 
                                                    anomaly[anomaly$sex == "F", ], 
                                                    baseline[baseline$sex == "F", ])
      }
    }
  }
  
  # Calculate CIs for males and females
  ci_lower_M <- quantile(bootstrap_attributable_deaths_M, 0.025)
  ci_upper_M <- quantile(bootstrap_attributable_deaths_M, 0.975)
  ci_lower_F <- quantile(bootstrap_attributable_deaths_F, 0.025)
  ci_upper_F <- quantile(bootstrap_attributable_deaths_F, 0.975)
  
  # Sum attributable deaths for males and females
  total_attributable_deaths_M <- sum(baseline$attributable_deaths[baseline$sex == "M"], na.rm = TRUE)
  total_attributable_deaths_F <- sum(baseline$attributable_deaths[baseline$sex == "F"], na.rm = TRUE)
  
  # Store results
  overall_result_M <- list(
    attributable_deaths = total_attributable_deaths_M,
    ci_lower = ci_lower_M,
    ci_upper = ci_upper_M
  )
  
  overall_result_F <- list(
    attributable_deaths = total_attributable_deaths_F,
    ci_lower = ci_lower_F,
    ci_upper = ci_upper_F
  )
  
  grouped_attributable_deaths <- aggregate(
    attributable_deaths ~ age_group + sex, data = baseline, 
    sum, 
    na.rm = TRUE)
  
  # Initialize a list to store bootstrap results for each group
  group_bootstrap_results <- list()
  
  # Apply bootstrapping for each group
  for(group in 1:nrow(grouped_attributable_deaths)) {
    # Print group processing start message
    print(paste("Processing group:", 
                grouped_attributable_deaths$age_group[group], 
                grouped_attributable_deaths$sex[group]))
    
    filtered_anomaly <- anomaly[anomaly$age_group == grouped_attributable_deaths$age_group[group] & anomaly$sex == grouped_attributable_deaths$sex[group], ]
    filtered_baseline <- baseline[baseline$age_group == grouped_attributable_deaths$age_group[group] & baseline$sex == grouped_attributable_deaths$sex[group], ]
    
    if(nrow(filtered_anomaly) > 0) {
      bootstrap_results <- numeric(n_bootstrap)
      for(i in seq_len(n_bootstrap)) {
        # Optional: Print progress every 100 iterations
        if (i %% 100 == 0) {
          print(paste("Bootstrap progress for group", grouped_attributable_deaths$age_group[group], grouped_attributable_deaths$sex[group], ":", i, "/", n_bootstrap))
        }
        
        bootstrap_indices <- sample(seq_len(nrow(filtered_anomaly)), replace = TRUE)
        bootstrap_results[i] <- do_an(bootstrap_indices, filtered_anomaly, filtered_baseline)
      }
      ci_lower_group <- quantile(bootstrap_results, 0.025)
      ci_upper_group <- quantile(bootstrap_results, 0.975)
      group_bootstrap_results[[paste(grouped_attributable_deaths$age_group[group], grouped_attributable_deaths$sex[group], sep = "_")]] <- list(
        attributable_deaths = sum(filtered_baseline$attributable_deaths, na.rm = TRUE),
        ci_lower = ci_lower_group,
        ci_upper = ci_upper_group
      )
    }
  }
  
  # Return the results
  list <- list(
    total_M = overall_result_M,
    total_F = overall_result_F,
    by_group = group_bootstrap_results)
  
  return(list)
}