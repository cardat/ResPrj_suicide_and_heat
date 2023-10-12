# tar_load(anomaly)

# Subset the anomaly data to include only specific rows
nsw_55plus_male <- anomaly[
  anomaly$age_group == '55+' & 
  anomaly$sex == 'M' & 
  anomaly$state == 'NSW', ]

# Predict the expected number of suicides
gam_model_pred <- predict(gam_disagg, 
              newdata = nsw_55plus_male, 
              type = "response", 
              se.fit = TRUE)

# Calculate the lower and upper confidence intervals
lci <- with(gam_model_pred, fit - (1.96 * se.fit))
uci <- with(gam_model_pred, fit + (1.96 * se.fit))

# Calculate the average fit value - didnt work
# avgfit <- mean(an$fit)

# Calculate attributable deaths using predictions from GAM
nsw_55plus_male$an <- 
  (deaths/pop) * (exp(gam_model_pred$fit * tmax_anomaly) - 1) * pop 

setorder(nsw_55plus_male, month)
plot(1:12, nsw_55plus_male$deaths [1:12], type = "l", ylim = c(0,7))
lines(1:12, gam_model_pred [[1]][1:12])

# (deaths/pop) * (exp(betai * tmax_anomaly) - 1) * pop

nsw_55plus_male$an <- as.numeric(nsw_55plus_male$an)
# > sum(nsw_55plus_male$an)
# [1] 14972904536


