do_tab_desc <- function(
    descriptive,
    anomaly,
    mrg_dat_pop
){

  suicides <- mrg_dat_pop[, .(
    total_sui = sum(deaths, na.rm = TRUE)
  ), by = gcc]

  suicides <- suicides[gcc != "Other"]
  
  meansBygcc <- descriptive[, .(
    rate = round(mean(rate, na.rm = TRUE),2),
    rate_m = round(mean(rate_m, na.rm = TRUE),2),
    rate_f = round(mean(rate_f, na.rm = TRUE),2),
    avgtmin = round(mean(avgtmin, na.rm = TRUE),2),
    avgmeantemp = round(mean(avgmeantemp, na.rm = TRUE),2),
    avgtmax = round(mean(avgtmax, na.rm = TRUE),2)
  ), by = gcc]
  
  anomalyBygcc <- anomaly[, .(
    mean_tmax_anomaly = round(mean(tmax_anomaly, na.rm = TRUE),2)
  ), by = gcc]
  
  # Merge 
  tab_desc <- merge(meansBygcc, anomalyBygcc, by = "gcc")
  tab_desc <- merge(tab_desc, suicides, by = "gcc")
  
  # Organize gccs
  ordered_gccs <- c("1GSYD", "1RNSW", 
                    "2GMEL", "2RVIC", 
                    "3GBRI", "3RQLD", 
                    "4GADE", "4RSAU", 
                    "5GPER", "5RWAU",
                    "6GHOB", "6RTAS",
                    "7GDAR", "7RNTE",
                    "8ACTE")
  tab_desc <- tab_desc[match(ordered_gccs, gcc)]
  
  # Rename the columns
  setnames(tab_desc, 
           old = c("gcc", "rate", "rate_m", "rate_f", "avgtmin", "avgmeantemp", "avgtmax", "mean_tmax_anomaly", "total_sui"),
           new = c("gcc", 
                   "Suicides per 100,000", 
                   "Male suicides per 100,000", 
                   "Female suicides per 100,000", 
                   "Average Minimum Temperature (C)", 
                   "Average Mean Temperature (C)", 
                   "Average Maximum Temperature (C)", 
                   "Average Maximum Temperature Anomaly (C)",
                   "Total Suicides"))
  
  return(tab_desc)
}