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
    gender <- sub(".*_", "", group_name)        # Extract gender indicator
    
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
  
  # Add total attributable deaths for all cities, rounding the numbers
  total <- bootstrap$total
  total_str <- paste(round(total$attributable_deaths), 
                     "(", round(total$ci_lower), "-", round(total$ci_upper), ")", sep = "")
  results_df <- rbind(results_df, data.frame(city = "Total", male_attributable_deaths = NA, female_attributable_deaths = NA, stringsAsFactors = FALSE))
  results_df[results_df$city == "Total", "male_attributable_deaths"] <- total_str
  results_df[results_df$city == "Total", "female_attributable_deaths"] <- total_str
  
  return(results_df)
}
