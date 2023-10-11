library(sqldf)

###################################################
### code chunk number 34: SuiDrtNSW_SupportingInfo.Rnw:2071-2330
###################################################
######################
#do,  Attributable Number of Deaths
######################



# newnode get estimate as attributable deaths
# need to calculate
# y(attributableToX) = sum((y0 x (exp(beta * X) - 1) x Pop))
# where y0 is the baseline incidence rate for the health endpoint being quantified;
# Pop is the population affected and
# beta is the effect coefficient drawn from the model.


# get a test dataset
# paste(names(data)[c(2:9,17)],sep='', collapse="','")
# data2<- data[,c('state','rural','sex','agegp','agegp2','dthyy','dthmm','deaths','pop','logDroughtCount')]
# head(data2)
# use the average rates deaths/person/month
# newnode get descriptive deaths by age/sex/month/zone groups
# calculate baseline incidence

names(anomaly)
# desc <- sqldf('
#     select state, sex, age_group,deaths as avgMonthlyDeaths, pop as avgPop,
#         deaths/pop as avgRate
#         from anomaly
#         group by state, sex, age_group
#         order by state, sex, age_group
#         ', drv = "SQLite")

desc <- anomaly[, .(avgMonthlyDeaths = mean(deaths),
                    avgPop = mean(pop),
                    avgRate = mean(deaths/pop)), 
                by = .(state, sex, age_group)
][order(state, sex, age_group)]


head(desc)
desc[1:40,]
# sqldf(
#   'select state, sum(avgMonthlyDeaths), sum(avgPop)
#    from desc
#         group by state
#         order by state
#         ', drv = "SQLite")

desc[, .(totalAvgMonthlyDeaths = sum(avgMonthlyDeaths), 
                   totalAvgPop = sum(avgPop)), 
               by = state][order(state)]


subset(desc, state == 'NSW')

with(subset(anomaly, state == 'NSW' & sex == 1), plot(age_group,deaths/pop))
with(subset(anomaly, state == 'NSW' & sex == 1 & age_group == '70plus'),
     plot(as.Date(paste(dthyy, dthmm, 1, sep='-')), deaths, type = 'l', col = 'grey')
)
abline(2.3392070,0)
# ok merge with the test dataset
data2 <- merge(data2, desc, by =  c('state', 'sex', 'age_group'))
subset(desc, state == 'Central West')
head(data2)

# now use the coefficient in
# y(attributable) = baselineIncidence x (exp(beta * X) - 1) x Pop
# recall I used
glmest<-summary(interactionDrtAgeSexRuralModel3)$coefficients
betai <- glmest[which(row.names(glmest)=='DrtMales30_49rural'),1]
sei <- glmest[which(row.names(glmest)=='DrtMales30_49rural'),2]
# estimate only for  DrtMales30_49rural
attributable <- subset(data2, rural == 1 & sex ==1 & age_group == '30_49')
table(attributable$state)
attach(attributable)

attributable$deathsAttributable <-
  (avgMonthlyDeaths/avgPop) * (exp(betai * logDroughtCount) - 1) * pop
# SE
#LCI
attributable$deathsAttributableLower <-
  (avgMonthlyDeaths/avgPop) * (exp((betai - sei * 1.96) *  logDroughtCount) - 1) * pop
#UCI
attributable$deathsAttributableUpper <-
  (avgMonthlyDeaths/avgPop) * (exp((betai + sei * 1.96) * logDroughtCount) - 1) * pop

detach(attributable)
head(attributable)


# now summarise by year
summaryAttributable <- sqldf(
  'select dthyy, sum(deathsAttributable) as deathsAttributable,
     sum(deaths) as deaths,
        sum(pop) as pop,
        round(avg(logDroughtCount),0) as logDroughtCount
        from attributable
        group by dthyy
        order by dthyy
        ', drv = "SQLite")
summaryAttributable
# plot the estimated deaths
with(summaryAttributable,
     plot(dthyy, deathsAttributable/deaths, type = 'l')
)
par(new=T)
with(summaryAttributable,
     plot(dthyy, logDroughtCount, type = 'l',col = 'blue')
)
par(new=T)
with(summaryAttributable,
     plot(dthyy, deaths, type = 'b',col = 'darkblue', pch=16)
)
# calcualte estimate

estOut <- sqldf(
  'select sum(deaths) as deaths,
    sum(deathsAttributable) as deathsAttributable,
        sum(deathsAttributableLower) as deathsAttributableLower,
        sum(deathsAttributableUpper) as deathsAttributableUpper
        from attributable
        ', drv = "SQLite")

# The predicted number of rural male suicides aged 30-49 per annum associated with droughts over our study period was 4.01 (95%CI 2.14 to 6.05)
estOut$deathsAttributable
# 152.3477
length(2007:1970)
estOut$deathsAttributable / 38
# [1] 4.009151
estOut$deathsAttributableLower / 38
# [1] 2.136019
estOut$deathsAttributableUpper / 38
# [1] 6.046266
