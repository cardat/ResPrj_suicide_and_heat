# #tar_load(anomaly)
# # Subset the data
# anomaly <- anomaly[, `:=`(
#   TmaxMales10_29 = ifelse(age_group == '10–29' & sex == 'M', tmax_anomaly, 0),
#   TmaxMales30_54 = ifelse(age_group == '30–54' & sex == 'M', tmax_anomaly, 0),
#   TmaxMales55plus = ifelse(age_group == '55+' & sex == 'M', tmax_anomaly, 0),
#   TmaxFemales10_29 = ifelse(age_group == '10–29' & sex == 'F', tmax_anomaly, 0),
#   TmaxFemales30_54 = ifelse(age_group == '30–54' & sex == 'F', tmax_anomaly, 0),
#   TmaxFemales55plus = ifelse(age_group == '55+' & sex == 'F', tmax_anomaly, 0)
# )]
# nsw <- subset(anomaly, state == 'NSW' & sex == 'M' & age_group == '55+')
# qld <- subset(anomaly, state == 'QLD' & sex == 'M' & age_group == '55+')
# vic <- subset(anomaly, state == 'VIC' & sex == 'F' & age_group == '55+')
# 
# # Fit the GAM model
# gam_nsw <- gam(
#   deaths ~
#     s(TmaxMales55plus, k=3) +  # Only include relevant smoothed term
#     ns(year,3) +
#     s(month, k=3, fx=T, bs = 'cc') +
#     offset(log(pop)),
#   data = nsw,
#   family = poisson
# )
# 
# gam_qld <- gam(
#   deaths ~
#     s(TmaxMales55plus, k=3) +  # Only include relevant smoothed term
#     ns(year,3) +
#     s(month, k=3, fx=T, bs = 'cc') +
#     offset(log(pop)),
#   data = qld,
#   family = poisson
# )
# 
# gam_vic <- gam(
#   deaths ~
#     s(TmaxFemales55plus, k=3) +  # Only include relevant smoothed term
#     ns(year,3) +
#     s(month, k=3, fx=T, bs = 'cc') +
#     offset(log(pop)),
#   data = vic,
#   family = poisson
# )
# 
# snsw <- summary(gam_nsw)
# sqld <- summary(gam_qld)
# svic <- summary(gam_vic)
# 
# # Extract the p-value associated with the smoothed term for TmaxMales55plus
# p_nsw55m <- round(snsw$s.table["s(TmaxMales55plus)", "p-value"],4)
# p_qld55m <- round(sqld$s.table["s(TmaxMales55plus)", "p-value"],4)
# p_vic55f <- round(svic$s.table["s(TmaxFemales55plus)", "p-value"],4)
# 
# # Print the p-value
# print(p_nsw55m)
# print(p_qld55m)
# print(p_vic55f)
# 
# g <- summary(gam_disagg)
