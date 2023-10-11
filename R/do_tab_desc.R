do_tab_desc <- function(
    descriptive,
    anomaly
){
 
  meansByState <- descriptive[, .(
    rate = round(mean(rate, na.rm = TRUE),2),
    rate_m = round(mean(rate_m, na.rm = TRUE),2),
    rate_f = round(mean(rate_f, na.rm = TRUE),2),
    avgtmin = round(mean(avgtmin, na.rm = TRUE),2),
    avgmeantemp = round(mean(avgmeantemp, na.rm = TRUE),2),
    avgtmax = round(mean(avgtmax, na.rm = TRUE),2)
  ), by = state]
  
  anomalyByState <- anomaly[, .(
    mean_tmax_anomaly = round(mean(tmax_anomaly, na.rm = TRUE),2)
  ), by = state]
  
  # Merge 
  tab_desc <- merge(meansByState, anomalyByState, by = "state")
  
  # Organize states
  ordered_states <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
  tab_desc <- tab_desc[match(ordered_states, state)]
  
  # Rename the columns
  setnames(tab_desc, 
           old = c("state", "rate", "rate_m", "rate_f", "avgtmin", "avgmeantemp", "avgtmax", "mean_tmax_anomaly"),
           new = c("State", 
                   "Suicides per 100,000", 
                   "Male suicides per 100,000", 
                   "Female suicides per 100,000", 
                   "Average Minimum Temperature (C)", 
                   "Average Mean Temperature (C)", 
                   "Average Maximum Temperature (C)", 
                   "Average Maximum Temperature Anomaly (C)"))
  
  return(tab_desc)
}