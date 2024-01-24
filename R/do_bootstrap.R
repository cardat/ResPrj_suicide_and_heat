do_bootstrap <- function(anomaly, gam_sex) {
  
  tar_load(anomaly)
  tar_load(gam_sex)
  
  # Predicted deaths with anomalies
  anomaly$predicted_deaths_anomaly <- predict(
    gam_sex, 
    newdata = anomaly, 
    type = "response")
  
  # Set temperature anomalies to zero for baseline scenario
  baseline <- anomaly
  baseline$TmaxMales <- 0
  baseline$TmaxFemales <- 0
  
  # Predicted deaths for baseline scenario
  baseline$predicted_deaths_baseline <- predict(
    gam_sex, 
    newdata = baseline, 
    type = "response")
  
  # Attributable deaths
  baseline$attributable_deaths <- baseline$predicted_deaths_anomaly - baseline$predicted_deaths_baseline
  
  # Function to calculate attributable deaths for bootstrapping
  an <- function(bootstrap_indices, anomaly_subset, baseline_subset) {
    bootstrap_anomaly <- anomaly_subset[bootstrap_indices, , drop = FALSE]
    bootstrap_baseline <- baseline_subset[bootstrap_indices, , drop = FALSE]
    
    predicted_deaths <- predict(gam_sex, newdata = bootstrap_anomaly, type = "response")
    predicted_deaths_baseline <- predict(gam_sex, newdata = bootstrap_baseline, type = "response")
    
    # Calculate attributable deaths for each data point
    attributable_deaths_per_point <- predicted_deaths - predicted_deaths_baseline
    
    # Sum the attributable deaths across all data points
    total_attributable_deaths <- sum(attributable_deaths_per_point, na.rm = TRUE)
    
    return(total_attributable_deaths)
  }
  
  # Number of bootstrap samples
  n_bootstrap <- 1000  # Adjust for better accuracy
  bootstrap_attributable_deaths <- numeric(n_bootstrap)
  
  # Bootstrap sampling for overall data
  for(i in seq_len(n_bootstrap)) {
    bootstrap_indices <- sample(seq_len(nrow(anomaly)), replace = TRUE)
    bootstrap_attributable_deaths[i] <- an(bootstrap_indices, anomaly, baseline)
  }
  
  # Compute the confidence interval for overall data
  ci_lower <- quantile(bootstrap_attributable_deaths, 0.025)
  ci_upper <- quantile(bootstrap_attributable_deaths, 0.975)
  
  # Compute total attributable deaths with NA values ignored
  attributable_deaths <- sum(baseline$attributable_deaths, na.rm = TRUE)
  
  # Prepare result for overall data
  overall_result <- list(
    attributable_deaths = attributable_deaths,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  # Group and sum attributable deaths by gcc and sex
  grouped_attributable_deaths <- aggregate(attributable_deaths ~ gcc + sex, data = baseline, sum, na.rm = TRUE)
  
  # Initialize a list to store bootstrap results for each group
  group_bootstrap_results <- list()
  
  # Apply bootstrapping for each group
  for(group in 1:nrow(grouped_attributable_deaths)) {
    # Print group processing start message
    print(paste("Processing group:", grouped_attributable_deaths$gcc[group], grouped_attributable_deaths$sex[group]))
    
    filtered_anomaly <- anomaly[anomaly$gcc == grouped_attributable_deaths$gcc[group] & anomaly$sex == grouped_attributable_deaths$sex[group], ]
    filtered_baseline <- baseline[baseline$gcc == grouped_attributable_deaths$gcc[group] & baseline$sex == grouped_attributable_deaths$sex[group], ]
    
    if(nrow(filtered_anomaly) > 0) {
      bootstrap_results <- numeric(n_bootstrap)
      for(i in seq_len(n_bootstrap)) {
        # Optional: Print progress every 100 iterations
        if (i %% 100 == 0) {
          print(paste("Bootstrap progress for group", grouped_attributable_deaths$gcc[group], grouped_attributable_deaths$sex[group], ":", i, "/", n_bootstrap))
        }
        
        bootstrap_indices <- sample(seq_len(nrow(filtered_anomaly)), replace = TRUE)
        bootstrap_results[i] <- an(bootstrap_indices, filtered_anomaly, filtered_baseline)
      }
      ci_lower_group <- quantile(bootstrap_results, 0.025)
      ci_upper_group <- quantile(bootstrap_results, 0.975)
      group_bootstrap_results[[paste(grouped_attributable_deaths$gcc[group], grouped_attributable_deaths$sex[group], sep = "_")]] <- list(
        attributable_deaths = sum(filtered_baseline$attributable_deaths, na.rm = TRUE),
        ci_lower = ci_lower_group,
        ci_upper = ci_upper_group
      )
    }
  }
  
  # Return the results
  return(list(
    total = overall_result,
    by_group = group_bootstrap_results
  ))
}














# 
# do_bootstrap <- function(anomaly, gam_sex) {
#   
#   # Predicted deaths with anomalies
#   anomaly$predicted_deaths_anomaly <- predict(
#     gam_sex, 
#     newdata = anomaly, 
#     type = "response")
#   
#   # Set temperature anomalies to zero for baseline scenario
#   baseline <- anomaly
#   baseline$TmaxMales <- 0
#   baseline$TmaxFemales <- 0
#   
#   # Predicted deaths for baseline scenario
#   baseline$predicted_deaths_baseline <- predict(
#     gam_sex, 
#     newdata = baseline, 
#     type = "response")
#   
#   # Attributable deaths
#   baseline$attributable_deaths <- baseline$predicted_deaths_anomaly - baseline$predicted_deaths_baseline
#   
#   # Function to calculate attributable deaths for bootstrapping
#   an <- function(bootstrap_indices) {
#     bootstrap_anomaly <- anomaly[bootstrap_indices, , drop = FALSE]
#     bootstrap_baseline <- baseline[bootstrap_indices, , drop = FALSE]
#     
#     predicted_deaths <- predict(gam_sex, newdata = bootstrap_anomaly, type = "response")
#     predicted_deaths_baseline <- predict(gam_sex, newdata = bootstrap_baseline, type = "response")
#     
#     # Calculate attributable deaths for each data point
#     attributable_deaths_per_point <- predicted_deaths - predicted_deaths_baseline
#     
#     # Sum the attributable deaths across all data points
#     total_attributable_deaths <- sum(attributable_deaths_per_point, na.rm = TRUE)
#     
#     return(total_attributable_deaths)
#   }
#   
#   # Number of bootstrap samples
#   n_bootstrap <- 3  # Adjust for better accuracy
#   bootstrap_attributable_deaths <- numeric(n_bootstrap)
#   
#   # Bootstrap sampling for overall data
#   for(i in seq_len(n_bootstrap)) {
#     bootstrap_indices <- sample(seq_len(nrow(anomaly)), replace = TRUE)
#     bootstrap_attributable_deaths[i] <- an(bootstrap_indices)
#   }
#   
#   # Compute the confidence interval for overall data
#   ci_lower <- quantile(bootstrap_attributable_deaths, 0.025)
#   ci_upper <- quantile(bootstrap_attributable_deaths, 0.975)
#   
#   # Compute total attributable deaths with NA values ignored
#   attributable_deaths <- sum(baseline$attributable_deaths, na.rm = TRUE)
#   
#   # Prepare result for overall data
#   overall_result <- list(
#     attributable_deaths = attributable_deaths,
#     ci_lower = ci_lower,
#     ci_upper = ci_upper
#   )
#   
#   # Group and sum attributable deaths by gcc and sex
#   grouped_attributable_deaths <- aggregate(attributable_deaths ~ gcc + sex, data = baseline, sum, na.rm = TRUE)
#   # Initialize a list to store bootstrap results for each group
#   group_bootstrap_results <- list()
#   
#   # Apply bootstrapping for each group
#   for(group in 1:nrow(grouped_attributable_deaths)) {
#     filtered_anomaly <- anomaly[anomaly$gcc == grouped_attributable_deaths$gcc[group] & anomaly$sex == grouped_attributable_deaths$sex[group], ]
#     if(nrow(filtered_anomaly) > 0) {
#       bootstrap_results <- numeric(n_bootstrap)
#       for(i in seq_len(n_bootstrap)) {
#         bootstrap_indices <- sample(seq_len(nrow(filtered_anomaly)), replace = TRUE)
#         bootstrap_results[i] <- an(bootstrap_indices)
#       }
#       ci_lower_group <- quantile(bootstrap_results, 0.025)
#       ci_upper_group <- quantile(bootstrap_results, 0.975)
#       group_bootstrap_results[[paste(grouped_attributable_deaths$gcc[group], grouped_attributable_deaths$sex[group], sep = "_")]] <- list(
#         attributable_deaths = grouped_attributable_deaths$attributable_deaths[group],
#         ci_lower = ci_lower_group,
#         ci_upper = ci_upper_group
#       )
#     }
#   }
#   
#   
#   # Return the results
#   return(list(
#     total = overall_result,
#     by_group = group_bootstrap_results
#   ))
# }



# do_bootstrap <- function(
    #     anomaly, 
#     gam_sex
# ){
#   
#   # tar_load(anomaly)
#   # tar_load(gam_sex)
#   
#   # Predicted deaths with anomalies
#   anomaly$predicted_deaths_anomaly <- predict(
#     gam_sex, 
#     newdata = anomaly, 
#     type = "response")
#   
#   # Set temperature anomalies to zero for baseline scenario
#   baseline <- anomaly
#   baseline$TmaxMales <- 0
#   baseline$TmaxFemales <- 0
#   
#   # Predicted deaths for baseline scenario
#   baseline$predicted_deaths_baseline <- predict(
#     gam_sex, 
#     newdata = baseline, 
#     type = "response")
#   
#   # Attributable deaths
#   baseline$attributable_deaths <- baseline$predicted_deaths_anomaly - baseline$predicted_deaths_baseline
#   
#   # Function to calculate attributable deaths for bootstrapping
#   an <- function(
    #     bootstrap_indices
#     ){
#     bootstrap_anomaly <- anomaly[bootstrap_indices, , drop = FALSE]
#     bootstrap_baseline <- baseline[bootstrap_indices, , drop = FALSE]
#     
#     predicted_deaths <- predict(gam_sex, newdata = bootstrap_anomaly, type = "response")
#     predicted_deaths_baseline <- predict(gam_sex, newdata = bootstrap_baseline, type = "response")
#     
#     # Calculate attributable deaths for each data point
#     attributable_deaths_per_point <- predicted_deaths - predicted_deaths_baseline
#     
#     # Sum the attributable deaths across all data points
#     total_attributable_deaths <- sum(attributable_deaths_per_point, na.rm = TRUE)
#     
#     return(total_attributable_deaths)
#   }
#   
#   # Number of bootstrap samples
#   n_bootstrap <- 1000  # Increase for better accuracy
#   bootstrap_attributable_deaths <- numeric(n_bootstrap)
#   
#   # Bootstrap sampling
#   for(i in seq_len(n_bootstrap)) {
#     bootstrap_indices <- sample(seq_len(nrow(anomaly)), replace = TRUE)
#     bootstrap_attributable_deaths[i] <- an(bootstrap_indices)
#   }
#   
#   # Compute the confidence interval
#   ci_lower <- quantile(bootstrap_attributable_deaths, 0.025)
#   ci_upper <- quantile(bootstrap_attributable_deaths, 0.975)
#   
#   # Compute total attributable deaths with NA values ignored
#   attributable_deaths <- sum(baseline$attributable_deaths, na.rm = TRUE)
#   
#   # Return the attributable deaths and confidence interval
#   result <- list(
#     attributable_deaths = attributable_deaths,
#     ci_lower = ci_lower,
#     ci_upper = ci_upper
#   )
#   
#   return(result)
# }