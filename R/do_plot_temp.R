do_plot_temp <- function(
    dat_temp
){

png('manuscript/01_figures/fig_temp.png',res=200,width = 1800, height = 700)
# time-series yearly avg temperature plot

yearly <- temp[, .(yr_avg = mean(tmean, na.rm = TRUE)), by = .(state, year)]

yearly_all_states <- yearly[, .(yr_avg_all = mean(yr_avg, na.rm = TRUE)), by = year]

# Fit the GAM model
library(mgcv)
gam_model <- gam(yr_avg_all ~ s(year), data = yearly_all_states)

# Calculate standard errors
fitted_values <- fitted(gam_model)
se.fit <- predict(gam_model, type="response", se.fit=TRUE)$se.fit

# Plot the original data
plot(yearly_all_states$year, yearly_all_states$yr_avg_all,
     type = "l",  # "l" for line plot
     xlab = "Year",
     ylab = "Average Temperature",
     main = "",
     col = "black",
     ylim = c(min(yearly_all_states$yr_avg_all, fitted(gam_model) - 2*se.fit),
              max(yearly_all_states$yr_avg_all, fitted(gam_model) + 2*se.fit)))

# Add shaded confidence intervals
polygon(c(yearly_all_states$year, rev(yearly_all_states$year)),
        c(fitted_values - 2*se.fit, rev(fitted_values + 2*se.fit)),
        col = adjustcolor("grey", alpha.f = 0.8), border = NA)

# Add GAM smooth line
lines(yearly_all_states$year, fitted_values, col="black")

dev.off()

}