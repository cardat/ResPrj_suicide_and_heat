do_desc <- function(
    dat_pop,
    dat_suicide
){
  options(scipen = 999)
  tar_load(dat_pop)
  tar_load(dat_suicide)

  # Convert deaths and population figures to numeric
  dat_pop[, pop := as.numeric(pop)]
  dat_pop[, pop_m := as.numeric(pop_m)]
  dat_pop[, pop_f := as.numeric(pop_f)]
  dat_suicide[, deaths := as.numeric(deaths)]
  
  # Aggregating total deaths by GCC and sex
  suicide_summary_sex <- dat_suicide[, .(total_deaths_sex = sum(deaths)), by = .(gcc, sex)]
  
  # Pivot the aggregated data to have separate columns for male and female deaths
  library(data.table)
  suicide_pivot <- dcast(suicide_summary_sex, gcc ~ sex, value.var = "total_deaths_sex")
  
  # Rename columns to male_deaths and female_deaths
  setnames(suicide_pivot, c("M", "F"), c("male_deaths", "female_deaths"))
  
  # Calculate total deaths (sum of male and female deaths)
  suicide_pivot[, total_deaths := male_deaths + female_deaths]
  
  # Calculating total population for each GCC
  pop_summary <- dat_pop[, .(total_pop = sum(pop), male_pop = sum(pop_m), female_pop = sum(pop_f)), by = gcc]
  
  # Merge the suicide data with population data
  combined_data <- merge(suicide_pivot, pop_summary, by = "gcc")
  
  # Number of years in the dataset
  years_in_data <- length(unique(dat_suicide$year))
  
  # Calculating suicide rates
  combined_data[, rate_total := (total_deaths / years_in_data) / total_pop * 100000]
  combined_data[, rate_male := (male_deaths / years_in_data) / male_pop * 100000]
  combined_data[, rate_female := (female_deaths / years_in_data) / female_pop * 100000]
  

  # Creating a mapping of GCC codes to names
  gcc_names <- setNames(c("Sydney", "Rest of NSW", "Melbourne", "Rest of VIC", "Brisbane", "Rest of QLD", 
                          "Adelaide", "Rest of SAU", "Perth", "Rest of WAU", "Hobart", "Rest of TAS", 
                          "Darwin", "Rest of NTE", "Canberra"), 
                        c("1GSYD", "1RNSW", "2GMEL", "2RVIC", "3GBRI", "3RQLD", 
                          "4GADE", "4RSAU", "5GPER", "5RWAU", "6GHOB", "6RTAS", 
                          "7GDAR", "7RNTE", "8ACTE"))
  
  # Renaming the GCCs in combined_data
  combined_data$gcc <- gcc_names[combined_data$gcc]
  
  # Rounding deaths to whole numbers
  combined_data$total_deaths <- round(combined_data$total_deaths)
  combined_data$male_deaths <- round(combined_data$male_deaths)
  combined_data$female_deaths <- round(combined_data$female_deaths)
  


  
  # Creating a summary row for Australia
  australia_summary <- combined_data[, .(
    total_deaths = sum(total_deaths, na.rm = TRUE),
    male_deaths = sum(male_deaths, na.rm = TRUE),
    female_deaths = sum(female_deaths, na.rm = TRUE),
    total_pop = sum(total_pop, na.rm = TRUE),
    male_pop = sum(male_pop, na.rm = TRUE),
    female_pop = sum(female_pop, na.rm = TRUE),
    rate_total = mean(rate_total, na.rm = TRUE),
    rate_male = mean(rate_male, na.rm = TRUE),
    rate_female = mean(rate_female, na.rm = TRUE)
  )]
  
  # Adding a gcc column for the summary
  australia_summary$gcc <- "Australia"
  
  # Appending the Australia summary to the combined_data
  combined_data <- rbind(combined_data, australia_summary)
  
  # Rounding rates to two decimal places
  combined_data$rate_total <- round(combined_data$rate_total, 2)
  combined_data$rate_male <- round(combined_data$rate_male, 2)
  combined_data$rate_female <- round(combined_data$rate_female, 2)
  
  # Formatting numbers with commas
  combined_data$female_pop <- format(combined_data$female_pop, big.mark = ",", scientific = FALSE)
  combined_data$male_pop <- format(combined_data$male_pop, big.mark = ",", scientific = FALSE)
  combined_data$total_pop <- format(combined_data$total_pop, big.mark = ",", scientific = FALSE)
  
  combined_data$female_deaths <- format(combined_data$female_deaths, big.mark = ",", scientific = FALSE)
  combined_data$male_deaths <- format(combined_data$male_deaths, big.mark = ",", scientific = FALSE)
  combined_data$total_deaths <- format(combined_data$total_deaths, big.mark = ",", scientific = FALSE)
  
  # Reordering columns in combined_data
  descriptive <- combined_data[, .(gcc, 
                                     female_pop, male_pop, total_pop, 
                                     female_deaths, male_deaths, total_deaths, 
                                     rate_female, rate_male, rate_total)]
 
 return(descriptive)
}