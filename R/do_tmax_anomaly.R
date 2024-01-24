do_tmax_anomaly <- function(
    mrg_dat_pop,
    dat_temp
){

# Merge with the original data
# anomaly <- merge(
#   mrg_dat_pop, 
#   dat_temp, 
#   by = c("gcc", "month"), 
#   all.x = TRUE)

# Calculate tmax_anomaly
anomaly <- mrg_dat_pop[, tmax_anomaly := tmax - monthly_tmax_avg]

# exclude offshore aussie
# anomaly <- anomaly[state != "Other"]

# negative anomalies or comments out for just positive
anomaly$tmax_anomaly[anomaly$tmax_anomaly < 0] <- 0

# only summer months. commented out the following line for entire year  
# anomaly <- anomaly[month %in% c(1, 2, 12)]

anomaly[, summer_tmax_anomaly := ifelse(
  month %in% c(12, 1, 2), 
  tmax_anomaly, 0)]

anomaly[, TmaxMales10_29 := ifelse(age_group == '0–29' & sex == 'M', tmax_anomaly, 0)]
anomaly[, TmaxMales30_54 := ifelse(age_group == '30–54' & sex == 'M', tmax_anomaly, 0)]
anomaly[, TmaxMales55plus := ifelse(age_group == '55+' & sex == 'M', tmax_anomaly, 0)]

anomaly[, TmaxFemales10_29 := ifelse(age_group == '0–29' & sex == 'F', tmax_anomaly, 0)]
anomaly[, TmaxFemales30_54 := ifelse(age_group == '30–54' & sex == 'F', tmax_anomaly, 0)]
anomaly[, TmaxFemales55plus := ifelse(age_group == '55+' & sex == 'F', tmax_anomaly, 0)]

anomaly[, TmaxMales := ifelse(sex == 'M', tmax_anomaly, 0)]
anomaly[, TmaxFemales := ifelse(sex == 'F', tmax_anomaly, 0)]


return(anomaly)
}