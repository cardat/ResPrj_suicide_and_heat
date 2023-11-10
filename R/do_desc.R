do_desc <- function(
    mrg_dat_pop
){

  mrg_dat_pop[, date := paste(year, sprintf("%02d", month), sep="-")]
  
  desc <- mrg_dat_pop[, .(
    mtl_deaths = sum(deaths),
    rate = round((sum(deaths) / mean(pop)) * 100000, 2),
    avgtmin = round(mean(tmin), 1),
    avgtmax = round(mean(tmax), 1),
    avgmeantemp = round(mean(tmean), 1),
    year = year
 ), 
 by = .(gcc, date)]
 
 # rates by sex
 rates_by_sex <- mrg_dat_pop[, .(
   total_deaths = sum(deaths),
   rate = round(ifelse(sex == "M", (sum(deaths) / mean(pop_m)) * 100000, 
                 (sum(deaths) / mean(pop_f)) * 100000),2)
 ), 
 by = .(gcc, date, sex)]
 
 # Reshape the data to have rate_m and rate_f in separate columns
 rates_reshaped <- dcast(rates_by_sex, gcc + date ~ sex, value.var = "rate")
 setnames(rates_reshaped, c("M", "F"), c("rate_m", "rate_f"))
 
 # Merge with the previous result
 descriptive <- merge(desc, rates_reshaped, by = c("gcc", "date"))
 
 descriptive[, date := as.Date(paste(date, "01", sep="-"), format="%Y-%m-%d")]
 
 descriptive <- descriptive[descriptive$gcc != "Other", ]
 
 # Filter out the date "2018-12"
 descriptive <- descriptive[descriptive$date != as.Date("2018-12-01"), ]
 
 return(descriptive)
}