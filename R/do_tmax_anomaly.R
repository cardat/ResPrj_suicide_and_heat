do_tmax_anomaly <- function(
    mrg_dat_pop,
    dat_temp
){
  
dat_temp <- dat_temp[
  year %in% 1950:2018, .(
    monthly_tmax_avg = mean(
      tmax, na.rm=TRUE)
  ),
  by = .(state, month)]
  
# Merge with the original data
anomaly <- merge(
  mrg_dat_pop, 
  dat_temp, 
  by = c("state", "month"), 
  all.x = TRUE)

# Calculate tmax_anomaly
anomaly[, tmax_anomaly := tmax - monthly_tmax_avg]

# exclude offshore aussie
anomaly <- anomaly[state != "Other"]

anomaly$tmax_anomaly[anomaly$tmax_anomaly < 0] <- 0

return(anomaly)
}