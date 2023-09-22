do_tmax_anomaly <- function(
    mrg_dat_pop,
    dat_temp
){
  
# Merge with the original data
anomaly <- merge(
  mrg_dat_pop, 
  dat_temp, 
  by = c("state", "month"), 
  all.x = TRUE)

# Calculate tmax_anomaly
anomaly[, tmax_anomaly := tmax - monthly_tmax_avg]

return(anomaly)
}