do_tmax_anomaly <- function(
    mrg_dat_pop
){
  
# Calculate monthly average tmax for each state during the reference period
monthly_avg <- mrg_dat_pop[
  year %in% 2006:2018, .(
    monthly_tmax_avg = mean(
      tmax, na.rm=TRUE)
    ), 
  by = .(state, month)]

# Merge with the original data
anomaly <- merge(
  mrg_dat_pop, 
  monthly_avg, 
  by = c("state", "month"), 
  all.x = TRUE)

# Calculate tmax_anomaly
anomaly[, tmax_anomaly := tmax - monthly_tmax_avg]

return(anomaly)
}