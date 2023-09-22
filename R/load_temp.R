load_temp <- function(
    dat
){
dat_temp <- readRDS("C:/Users/291828H/OneDrive - Curtin/projects/DatSci_AWAP_GRIDS_1950_2019_ABS_State/data_derived/AWAP_1950-2019_ABS_2016_State_mth_avg_temperatures.rds")

setDT(dat_temp)

state_names <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "Other")

dat_temp$state <- state_names[dat_temp$state]
dat_temp[, month := as.integer(month)]
dat_temp[, year := as.integer(year)]
dat_temp[, tmean := (tmax + tmin)/2]

dat_temp <- dat_temp[
  year %in% 1950:2018, .(
    monthly_tmax_avg = mean(
      tmax, na.rm=TRUE)
  ),
  by = .(state, month)]

return(dat_temp)
}

