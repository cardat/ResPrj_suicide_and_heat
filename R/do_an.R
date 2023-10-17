tar_load(anomaly)
tar_load(gam_disagg)

# Example usage of the do_bootstrap function
result <- do_bootstrap(
  anomaly = anomaly,
  gam_disagg = gam_disagg,
  state = "NSW",
  age_group = "55+",
  sex = "M")
cat("Attributable Deaths:", result$attributable_deaths,
    "(95% CI", result$ci_lower, "to", result$ci_upper, ")\n")

vic_f <- do_bootstrap(
  anomaly = anomaly,
  gam_disagg = gam_disagg,
  state = "VIC",
  age_group = "55+",
  sex = "F")
cat("Attributable Deaths:", vic_f$attributable_deaths,
    "(95% CI", vic_f$ci_lower, "to", vic_f$ci_upper, ")\n")







do_an <- function(
    anomaly,
    gam_disagg
    ){
  combinations <- list(
  list(state = "NSW", age_group = "55+", sex = "M"),
  list(state = "QLD", age_group = "55+", sex = "M"),
  list(state = "VIC", age_group = "55+", sex = "F")
  )

  results <- lapply(
    combinations,
    function(
    combo
    ){
    state <- combo$state
    age_group <- combo$age_group
    sex <- combo$sex

    result <- do_bootstrap(
      anomaly = anomaly,
      gam_disagg = gam_disagg,
      state = state,
      age_group = age_group,
      sex = sex
    )
    return(list(
      state = state,
      age_group = age_group,
      sex = sex,
      attributable_deaths = result$attributable_deaths,
      ci_lower = result$ci_lower,
      ci_upper = result$ci_upper
    ))
    })

  # Convert the results list to a data frame for easier viewing and manipulation
  an <- do.call(rbind, lapply(results, as.data.frame))

  # return(an)
}
















# # deprecated as we are not using GLM
#
# do_an <- function(
#     tmaxglm
# ){
#
#
# # Extract coefficients
# glmest<-summary(tmaxglm)$coefficients
#
# # Retrieve beta for 'TmaxMales55plus'
# betai <- glmest[which(row.names(glmest)=='TmaxMales55plus'),1]
# # Retrieve the standard error of the estimated coefficient for 'TmaxMales55plus'
# sei <- glmest[which(row.names(glmest)=='TmaxMales55plus'),2]
#
# # an only for  TmaxMales55plus NSW
# attributable <- subset(anomaly,
#                        state == "NSW" &
#                        sex =="M" & age_group == '55+')
#
# # an only for  TmaxMales55plus QLD
# attributable <- subset(anomaly,
#                        state == "QLD" &
#                          sex =="M" & age_group == '55+')
#
# ### NT ####
# # Retrieve beta for 'TmaxFemales30_54'
# betai <- glmest[which(row.names(glmest)=='TmaxFemales30_54'),1]
# # Retrieve the standard error of the estimated coefficient for 'TmaxFemales30_54'
# sei <- glmest[which(row.names(glmest)=='TmaxFemales30_54'),2]
# # an only for  TmaxFemales30_54 NSW
# attributable <- subset(anomaly,
#                        state == "NT" &
#                          sex =="F" & age_group == '55+')
#
# # table(attributable$state)
#
# attributable$deathsAttributable <-
#   (attributable$deaths/attributable$pop) * (exp(betai * attributable$tmax_anomaly) - 1) * attributable$pop
#
# #LCI
# attributable$deathsAttributableLower <-
#   (attributable$deaths/attributable$pop) * (exp((betai - sei * 1.96) *  attributable$tmax_anomaly) - 1) * attributable$pop
# #UCI
# attributable$deathsAttributableUpper <-
#   (attributable$deaths/attributable$pop) * (exp((betai + sei * 1.96) * attributable$tmax_anomaly) - 1) * attributable$pop
#
# summaryAttributable <- attributable[, .(
#   deathsAttributable = sum(deathsAttributable),
#   deaths = sum(deaths),
#   pop = sum(pop),
#   tmax_anomaly = round(mean(tmax_anomaly))
# ), by = year
# ][order(year)]
#
# estOut <- attributable[, .(
#   deaths = sum(deaths),
#   deathsAttributable = sum(deathsAttributable),
#   deathsAttributableLower = sum(deathsAttributableLower),
#   deathsAttributableUpper = sum(deathsAttributableUpper)
# )]
#
# estOut$deathsAttributable
# # [1] 25.90695 - not setting negative tmax_anomaly to zero
# # [1] 38.09349 - setting negative tmax_anomaly to zero
# round(estOut$deathsAttributable / 13,2)
# # [1] 1.992843
# estOut$deathsAttributableLower
# # [1] -0.6176333
# estOut$deathsAttributableUpper
# # [1] 4.757215
#
# # The predicted number of male suicides aged 55+ per annum associated with temperature anomaly over our study period was 1.99 (95%CI -0.62 to 4.76)
#
# }