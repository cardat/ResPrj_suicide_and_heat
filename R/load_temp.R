load_temp <- function(
    dat
){
dat_temp <- readRDS("C:/Users/291828H/OneDrive - Curtin/projects/DatSci_AWAP_GRIDS_1950_2019_ABS_State/data_derived/AWAP_1950-2019_ABS_2016_gcc_mth_avg_temperatures.rds")

setDT(dat_temp)

gcc_names <- c("1GSYD", "1RNSW", 
               "2GMEL", "2RVIC", 
               "3GBRI", "3RQLD", 
               "4GADE", "4RSAU", 
               "5GPER", "5RWAU",
               "6GHOB", "6RTAS",
               "7GDAR", "7RNTE",
               "8ACTE")

dat_temp[, month := as.integer(month)]
dat_temp$gcc <- gcc_names[dat_temp$gcc]
dat_temp[, year := as.integer(year)]
dat_temp[, tmean := (tmax + tmin)/2]

dat_temp[year %in% 1950:2019, monthly_tmax_avg := mean(tmax, na.rm = TRUE), by = .(gcc, month)]

return(dat_temp)
}

