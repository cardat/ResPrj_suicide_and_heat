do_tab_an <- function(
    bootstrap
    ){
  # Mapping of gcc codes to city names
  gcc_names <- setNames(c("Sydney", "Rest of NSW", "Melbourne", "Rest of VIC", "Brisbane", "Rest of QLD", 
                          "Adelaide", "Rest of SAU", "Perth", "Rest of WAU", "Hobart", "Rest of TAS", 
                          "Darwin", "Rest of NTE", "Canberra"), 
                        c("1GSYD", "1RNSW", "2GMEL", "2RVIC", "3GBRI", "3RQLD", 
                          "4GADE", "4RSAU", "5GPER", "5RWAU", "6GHOB", "6RTAS", 
                          "7GDAR", "7RNTE", "8ACTE"))
  
  # Extracting the 'by_group' list
  by_group <- bootstrap$by_group
  
  # Create an empty data frame to store results
  results_df <- data.frame(city = character(), 
                           male_attributable_deaths = character(), 
                           female_attributable_deaths = character(),
                           stringsAsFactors = FALSE)
  
  # Iterate through each group and extract data
  for(group_name in names(by_group)) {
    group_data <- by_group[[group_name]]
    city_code <- gsub("_[MF]$", "", group_name)  # Extract the city code
    gender <- sub(".*_", "", group_name)        # Extract sex indicator
    
    # Replace city code with city name
    city_name <- gcc_names[city_code]
    
    # Round the numbers and create a string for attributable deaths with CI
    attributable_str <- paste(round(group_data$attributable_deaths), 
                              " (", round(group_data$ci_lower), " - ", round(group_data$ci_upper), ")", sep = "")
    
    # Add to the results data frame
    if(!city_name %in% results_df$city) {
      # Add new row for the city
      results_df <- rbind(results_df, data.frame(city = city_name, male_attributable_deaths = NA, female_attributable_deaths = NA, stringsAsFactors = FALSE))
    }
    
    if(gender == "M") {
      results_df[results_df$city == city_name, "male_attributable_deaths"] <- attributable_str
    } else if(gender == "F") {
      results_df[results_df$city == city_name, "female_attributable_deaths"] <- attributable_str
    }
  }
  
  # Extracting total attributable deaths for males and females
  total_M <- bootstrap$total_M
  total_F <- bootstrap$total_F
  
  # Formatting total attributable deaths strings for males and females
  total_str_M <- paste(round(total_M$attributable_deaths), 
                       "(", round(total_M$ci_lower), "-", round(total_M$ci_upper), ")", sep = "")
  total_str_F <- paste(round(total_F$attributable_deaths), 
                       "(", round(total_F$ci_lower), "-", round(total_F$ci_upper), ")", sep = "")
  
  # Assuming the results_df already has a row for "Total", we'll update it
  if("Total" %in% results_df$city) {
    results_df[results_df$city == "Total", "male_attributable_deaths"] <- total_str_M
    results_df[results_df$city == "Total", "female_attributable_deaths"] <- total_str_F
  } else {
    # If not, add a new row for "Total"
    results_df <- rbind(results_df, data.frame(city = "Total", male_attributable_deaths = total_str_M, female_attributable_deaths = total_str_F, stringsAsFactors = FALSE))
  }
  
  return(results_df)
}
