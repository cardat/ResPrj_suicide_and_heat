#  deprecated - not using

# tar_load(anomaly)
# tar_load(gam_disagg)
# 
# 
# # Subset the anomaly data to include only specific rows
# nsw_55plus_male <- anomaly[
#   anomaly$age_group == '55+' &
#     anomaly$sex == 'M' &
#     anomaly$state == 'NSW', ]
# 
# # Example usage with your GAM model
# # Make sure to replace 'x', 'cases', and 'exposure_var' with your actual data
# attrisk_nsw <- attrdl_gam(x = nsw_55plus_male$tmax_anomaly,  # This is the actual exposure data
#                       cases = nsw_55plus_male$deaths,      # This is the actual outcome data
#                       model = gam_disagg,                  # This is the fitted model
#                       exposure_var = c("s(TmaxMales55plus).1", "s(TmaxMales55plus).2"),  # These are the names of the coefficients in the model corresponding to the exposure variable
#                       type = "an",
#                       tot = TRUE,
#                       sim = TRUE,
#                       nsim = 5000)
# 
# qld_55plus_male <- anomaly[
#   anomaly$age_group == '55+' &
#     anomaly$sex == 'M' &
#     anomaly$state == 'QLD', ]
# 
# attrisk_qld <- attrdl_gam(x = qld_55plus_male$tmax_anomaly,  # This is the actual exposure data
#                       cases = qld_55plus_male$deaths,      # This is the actual outcome data
#                       model = gam_disagg,                  # This is the fitted model
#                       exposure_var = c("s(TmaxMales55plus).1", "s(TmaxMales55plus).2"),  # These are the names of the coefficients in the model corresponding to the exposure variable
#                       type = "an",
#                       tot = TRUE,
#                       sim = TRUE,
#                       nsim = 5000)
# 
# nt_30_54_female <- anomaly[
#   anomaly$age_group == '30–54' &
#     anomaly$sex == 'F' &
#     anomaly$state == 'NT', ]
# 
# attrisk_nt <- attrdl_gam(x = nt_30_54_female$tmax_anomaly,  # This is the actual exposure data
#                          cases = nt_30_54_female$deaths,      # This is the actual outcome data
#                          model = gam_disagg,                  # This is the fitted model
#                          exposure_var = c("s(TmaxFemales30_54).1", "s(TmaxFemales30_54).2"),  # These are the names of the coefficients in the model corresponding to the exposure variable
#                          type = "an",
#                          tot = TRUE,
#                          sim = TRUE,
#                          nsim = 5000)
# 
# 
# attrisk_nsw <- attrdl_gam(x = nsw_55plus_male$tmax_anomaly,  # This is the actual exposure data
#                       cases = nsw_55plus_male$deaths,      # This is the actual outcome data
#                       model = gam_disagg,                  # This is the fitted model
#                       exposure_var = c("s(TmaxMales55plus).1", "s(TmaxMales55plus).2"),  # These are the names of the coefficients in the model corresponding to the exposure variable
#                       type = "an",
#                       tot = TRUE,
#                       sim = TRUE,
#                       nsim = 5000)
# 
# # Calculate the average fit value - didnt work
# # avgfit <- mean(an$fit)
# 
# # # Calculate attributable deaths using predictions from GAM
# # nsw_55plus_male$an <-
# #   (deaths/pop) * (exp(gam_model_pred$fit * tmax_anomaly) - 1) * pop
# #
# # setorder(nsw_55plus_male, month)
# # plot(1:12, nsw_55plus_male$deaths [1:12], type = "l", ylim = c(0,7))
# # lines(1:12, gam_model_pred [[1]][1:12])
# #
# # # (deaths/pop) * (exp(betai * tmax_anomaly) - 1) * pop
# #
# # nsw_55plus_male$an <- as.numeric(nsw_55plus_male$an)
# # # > sum(nsw_55plus_male$an)
# # # [1] 14972904536
# 
