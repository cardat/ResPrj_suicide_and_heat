do_plot_temp <- function(
    dat_temp
){
  
# time-series yearly avg temperature plot
  
temp <- readRDS("C:/Users/291828H/OneDrive - Curtin/projects/DatSci_AWAP_GRIDS_1950_2019_ABS_State/data_derived/AWAP_1950-2019_ABS_2016_gcc_mth_avg_temperatures.rds")

setDT(temp)

gcc_names <- c("1GSYD", "1RNSW", 
               "2GMEL", "2RVIC", 
               "3GBRI", "3RQLD", 
               "4GADE", "4RSAU", 
               "5GPER", "5RWAU",
               "6GHOB", "6RTAS",
               "7GDAR", "7RNTE",
               "8ACTE")

temp$gcc <- gcc_names[temp$gcc]
temp[, month := as.integer(month)]
temp[, year := as.integer(year)]
temp[, tmean := (tmax + tmin)/2]  
yearly <- temp[, .(yr_avg = mean(tmean, na.rm = TRUE)), by = .(gcc, year)]

yearly_all_gccs <- yearly[, .(yr_avg_all = mean(yr_avg, na.rm = TRUE)), by = year]

# Fit the model
gam_model <- gam(yr_avg_all ~ s(year), data = yearly_all_gccs)

# Calculate standard errors
fitted_values <- fitted(gam_model)
se.fit <- predict(gam_model, type="response", se.fit=TRUE)$se.fit

png("manuscript/01_figures/fig_temperaturegcc.png", res = 200, width = 1800, height = 700)
# Plot the original data
plot(yearly_all_gccs$year, yearly_all_gccs$yr_avg_all, 
     type = "l",  # "l" for line plot
     xlab = "Year", 
     ylab = "Average Temperature",
     main = "",
     col = "black", 
     ylim = c(min(yearly_all_gccs$yr_avg_all, fitted(gam_model) - 2*se.fit), 
              max(yearly_all_gccs$yr_avg_all, fitted(gam_model) + 2*se.fit)))

# Add shaded confidence intervals
polygon(c(yearly_all_gccs$year, rev(yearly_all_gccs$year)), 
        c(fitted_values - 2*se.fit, rev(fitted_values + 2*se.fit)), 
        col = adjustcolor("grey", alpha.f = 0.8), border = NA)

# Add GAM smooth line
lines(yearly_all_gccs$year, fitted_values, col="black")
dev.off()
}

# only WA

# temp <- readRDS("C:/Users/291828H/OneDrive - Curtin/projects/DatSci_AWAP_GRIDS_1950_2019_ABS_State/data_derived/AWAP_1950-2019_ABS_2016_State_mth_avg_temperatures.rds")
# 
# setDT(temp)
# 
# state_names <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "Other")
# 
# temp$state <- state_names[temp$state]
# temp[, month := as.integer(month)]
# temp[, year := as.integer(year)]
# temp[, tmean := (tmax + tmin)/2]  
# yearly <- temp[, .(yr_avg = mean(tmean, na.rm = TRUE)), by = .(state, year)]
# 
# # Filter for WA only
# yearly_wa <- yearly[state == "WA"]
# 
# # Fit the model for WA
# gam_model_wa <- gam(yr_avg ~ s(year), data = yearly_wa)
# 
# # Calculate standard errors for WA
# fitted_values_wa <- fitted(gam_model_wa)
# se.fit_wa <- predict(gam_model_wa, type="response", se.fit=TRUE)$se.fit
# 
# png("manuscript/01_figures/fig_temperature_WA.png", res = 200, width = 1800, height = 700)
# # Plot the original data for WA
# plot(yearly_wa$year, yearly_wa$yr_avg, 
#      type = "l",  # "l" for line plot
#      xlab = "Year", 
#      ylab = "Average Temperature in WA",
#      main = "",
#      col = "black", 
#      ylim = c(min(yearly_wa$yr_avg, fitted_values_wa - 2*se.fit_wa), 
#               max(yearly_wa$yr_avg, fitted_values_wa + 2*se.fit_wa)))
# 
# # Add shaded confidence intervals for WA
# polygon(c(yearly_wa$year, rev(yearly_wa$year)), 
#         c(fitted_values_wa - 2*se.fit_wa, rev(fitted_values_wa + 2*se.fit_wa)), 
#         col = adjustcolor("grey", alpha.f = 0.8), border = NA)
# 
# # Add GAM smooth line for WA
# lines(yearly_wa$year, fitted_values_wa, col="black")
# dev.off()
