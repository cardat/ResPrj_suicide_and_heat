do_an <- function(
    anomaly,
    gam_disagg
){
  
  # Predefined combinations
  combinations <- list(
    list(st = "1GSYD", ag = "55+", sx = "M"),
    list(st = "3GBRI", ag = "55+", sx = "M"),
    list(st = "2GMEL", ag = "55+", sx = "F")
  )
  
  # Initialize a data frame to store the results
  results <- data.frame(
    gcc = character(0),
    Age_Group = character(0),
    Sex = character(0),
    AN_CI= character(0),
    AN_CI_y= character(0)
  )
  
  for(combo in combinations) {
    
    # Extracting gcc, age group, and sex from the combo list
    gcc <- combo$st
    age_group <- combo$ag
    sex <- combo$sx
    
    # Calling the do_bootstrap function
    result <- do_bootstrap(
      anomaly = anomaly,
      gam_disagg = gam_disagg,
      st = gcc,
      ag = age_group,
      sx = sex
    )
    
    # Formatting the AN, CI values and P-Value
    an_ci <- sprintf("%.2f (%.2f–%.2f)", 
                             result$attributable_deaths, 
                             result$ci_lower, 
                             result$ci_upper)
    
    an_ci_y <- sprintf("%.2f (%.2f–%.2f)", 
                     result$attributable_deaths/13, 
                     result$ci_lower/13, 
                     result$ci_upper/13)
    
    # Bind the results to the results data frame
    results <- rbind(results, data.frame(
      gcc = gcc,
      Age_Group = age_group,
      Sex = sex,
      AN_CI = an_ci,
      AN_CI_y = an_ci_y,
      stringsAsFactors = FALSE
    ))
  } 
  
  return(results)
}
# 
# # Example usage directly calling the function and storing the result
# results <- do_an(anomaly, gam_disagg)
# 
# # Printing the results
# print(results)
















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