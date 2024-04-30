do_tab_an_sex_age <- function(
    bootstrap_sex_age,
    gam_sex_age
){

  # Extracting the 'by_group' list
  by_group <- bootstrap_sex_age[['by_group']]
  
  # Create an empty data frame to store results
  results_df <- data.frame(age_group = character(), 
                           male_attributable_deaths = character(), 
                           female_attributable_deaths = character(),
                           stringsAsFactors = FALSE)
  
  # Iterate through each group and extract data
  for(group_name in names(by_group)) {
    group_data <- by_group[[group_name]]
    age_group <- gsub("_[MF]$", "", group_name)  # Extract the age group
    gender <- sub(".*_", "", group_name)        # Extract sex indicator
    
    # Round the numbers and create a string for attributable deaths with CI
    attributable_str <- paste(round(group_data$attributable_deaths), 
                              " (", round(group_data$ci_lower), " - ", round(group_data$ci_upper), ")", sep = "")
    
    # Check if this age group is already in the results data frame
    if(!age_group %in% results_df$age_group) {
      # Add new row for the age group
      results_df <- rbind(results_df, data.frame(age_group = age_group, male_attributable_deaths = NA, female_attributable_deaths = NA, stringsAsFactors = FALSE))
    }
    
    # Assign the results to the correct gender column
    if(gender == "M") {
      results_df[results_df$age_group == age_group, "male_attributable_deaths"] <- attributable_str
    } else if(gender == "F") {
      results_df[results_df$age_group == age_group, "female_attributable_deaths"] <- attributable_str
    }
  }
  
  # Extracting total attributable deaths for males and females
  total_M <- bootstrap_sex_age$total_M
  total_F <- bootstrap_sex_age$total_F
  
  # Formatting total attributable deaths strings for males and females
  total_str_M <- paste(round(total_M$attributable_deaths), 
                       "(", round(total_M$ci_lower), "-", round(total_M$ci_upper), ")", sep = "")
  total_str_F <- paste(round(total_F$attributable_deaths), 
                       "(", round(total_F$ci_lower), "-", round(total_F$ci_upper), ")", sep = "")
  
  # Assuming the results_df already has a row for "Total", we'll update it
  if("Total" %in% results_df$age_group) {
    results_df[results_df$age_group == "Total", "male_attributable_deaths"] <- total_str_M
    results_df[results_df$age_group == "Total", "female_attributable_deaths"] <- total_str_F
  } else {
    # If not, add a new row for "Total"
    results_df <- rbind(results_df, data.frame(age_group = "Total", male_attributable_deaths = total_str_M, female_attributable_deaths = total_str_F, stringsAsFactors = FALSE))
  }
  
  # Extract summary of the GAM model
  gam_summary <- summary(gam_sex_age)
  
  results_df$male_p_value <- NA
  results_df$female_p_value <- NA

  smooth_terms <- rownames(gam_summary$s.table)
  p_values <- gam_summary$s.table[, "p-value"]
  
  formatted_p_values <- ifelse(p_values < 0.05, paste0(round(p_values, 4), " *"),
                               ifelse(p_values < 0.1, paste0(round(p_values, 4), " ."),
                                      round(p_values, 4)))
  
  for(i in seq_along(smooth_terms)) {
    term <- smooth_terms[i]
    term_cleaned <- gsub("s\\(|\\)", "", term)  # Remove 's(' at the start and ')' at the end
    gender <- if(grepl("Males", term_cleaned)) {
      "male_p_value"
    } else {
      "female_p_value"
    }
    age_group_pattern <- gsub("Tmax(Males|Females)", "", term_cleaned)
    age_group <- ifelse(age_group_pattern == "10_29", "0–29",
                        ifelse(age_group_pattern == "30_54", "30–54",
                               ifelse(age_group_pattern == "55plus", "55+", age_group_pattern)))
    
    age_group_index <- which(results_df$age_group == age_group)
    
    if(length(age_group_index) > 0) {
      results_df[age_group_index, gender] <- formatted_p_values[i]
    }
  }
  
  return(results_df)
}
