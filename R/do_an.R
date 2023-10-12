# Extract coefficients
glmest<-summary(tmaxglm)$coefficients

# Retrieve beta for 'TmaxMales55plus'
betai <- glmest[which(row.names(glmest)=='TmaxMales55plus'),1]
# Retrieve the standard error of the estimated coefficient for 'TmaxMales55plus'
sei <- glmest[which(row.names(glmest)=='TmaxMales55plus'),2]

# an only for  TmaxMales55plus NSW
attributable <- subset(anomaly, 
                       state == "NSW" &
                       sex =="M" & age_group == '55+')

# table(attributable$state)

attributable$deathsAttributable <-
  (deaths/pop) * (exp(betai * tmax_anomaly) - 1) * pop

#LCI
attributable$deathsAttributableLower <-
  (deaths/pop) * (exp((betai - sei * 1.96) *  tmax_anomaly) - 1) * pop
#UCI
attributable$deathsAttributableUpper <-
  (deaths/pop) * (exp((betai + sei * 1.96) * tmax_anomaly) - 1) * pop

summaryAttributable <- attributable[, .(
  deathsAttributable = sum(deathsAttributable),
  deaths = sum(deaths),
  pop = sum(pop),
  tmax_anomaly = round(mean(tmax_anomaly))
), by = year
][order(year)]

estOut <- attributable[, .(
  deaths = sum(deaths),
  deathsAttributable = sum(deathsAttributable),
  deathsAttributableLower = sum(deathsAttributableLower),
  deathsAttributableUpper = sum(deathsAttributableUpper)
)]

estOut$deathsAttributable
# [1] 25.90695
round(estOut$deathsAttributable / 13,2)
# [1] 1.992843
round(estOut$deathsAttributableLower / 13,2)
# [1] -0.6176333
round(estOut$deathsAttributableUpper / 13,2)
# [1] 4.757215

# The predicted number of male suicides aged 55+ per annum associated with temperature anomaly over our study period was 1.99 (95%CI -0.62 to 4.76)