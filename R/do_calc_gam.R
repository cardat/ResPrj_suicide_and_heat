do_calc_gam <- function(
    anomaly
){
png('model.png',res=200,width = 1000, height = 1000)
tmaxanomModel <- gam(
  deaths ~ s(tmax_anomaly) +
    age_group * sex * ns(year,3) +
    state +
    s(month, k=4, fx=T, bs = 'cc') +
    offset(log(pop)),
  data=anomaly,
  family=poisson)

# tmaxanomedf <- summary(tmaxanomModel)$edf
# # [1] 1.016887
# summary(tmaxanomModel)

# Plot
par(mar = c(4, 4, 1, 1), # c(bottom, left, top, right)
    mgp = c(2.5, 1, 0), # c(axis_title, axis_labels, axis_line)
    las = 1,
    cex.axis = 0.8,
    cex.lab = 1)
plot(tmaxanomModel, select = 2,
     shade = TRUE, shade.col = "grey", seWithMean = TRUE,
     ylab = 'log Relative Risk',xlab='Month')
abline(h=0, lty=2, col="black")
abline(v=9, lty=2, col="black") # September
abline(v=11, lty=2, col="black") # November
# Add "Spring" text
y_position <- -0.1 # Adjust this value based on where you'd like the text to appear vertically
text(x = 10, y = y_position, labels = "Spring", col = "black")
dev.off()

# Log transform tmax_anomaly
anomaly$logTmaxAnomaly = log1p(anomaly$tmax_anomaly)

# > summary(anomaly$logTmaxAnomaly)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -6.673  -0.291   0.265   0.072   0.704   1.694
# NA's
#    3204

# > sum(anomaly$tmax_anomaly < -1)
# [1] 3204

# interaction vars for age_group, sex, and logTmaxAnomaly
anomaly$TmaxMales10_29 <- ifelse(anomaly$age_group == '10-29' & anomaly$sex == 'M', anomaly$logTmaxAnomaly, 0)
anomaly$TmaxMales30_54 <- ifelse(anomaly$age_group == '30-54' & anomaly$sex == 'M', anomaly$logTmaxAnomaly, 0)
anomaly$TmaxMales55plus <- ifelse(anomaly$age_group == '55+' & anomaly$sex == 'M', anomaly$logTmaxAnomaly, 0)

anomaly$TmaxFemales10_29 <- ifelse(anomaly$age_group == '10-29' & anomaly$sex == 'F', anomaly$logTmaxAnomaly, 0)
anomaly$TmaxFemales30_54 <- ifelse(anomaly$age_group == '30-54' & anomaly$sex == 'F', anomaly$logTmaxAnomaly, 0)
anomaly$TmaxFemales55plus <- ifelse(anomaly$age_group == '55+' & anomaly$sex == 'F', anomaly$logTmaxAnomaly, 0)

#disaggregated :

tmaxanomDisaggregatedModel <- gam(
  deaths ~ s(month, k=4, fx=T, bs = 'cc') +
    s(TmaxMales10_29) +
    s(TmaxMales30_54) +
    s(TmaxMales55plus) +
    s(TmaxFemales10_29) +
    s(TmaxFemales30_54) +
    s(TmaxFemales55plus) +
    logTmaxAnomaly +
    age_group +
    state +
    sex +
    age_group * sex * ns(year,3) +
    offset(log(pop)),
  data=anomaly,
  family=poisson
)

summary(tmaxanomDisaggregatedModel)

png('model.png',res=200,width = 1000, height = 1600)
par(mfcol=c(3,2),mar=c(4,5,2,1), cex = .5)

# For Tmax with male age groups
plot(tmaxanomDisaggregatedModel, select=2, se=T, shade=TRUE, shade.col='grey', ylab = 'log Relative Risk', xlab = 'logTmaxAnomaly')
abline(0,0)
title('TmaxMales10_29')

plot(tmaxanomDisaggregatedModel, select=3, se=T, shade=TRUE, shade.col='grey', ylab = 'log Relative Risk', xlab = 'logTmaxAnomaly')
abline(0,0)
title('TmaxMales30_54')

plot(tmaxanomDisaggregatedModel, select=4, se=T, shade=TRUE, shade.col='grey', ylab = 'log Relative Risk', xlab = 'logTmaxAnomaly')
abline(0,0)
title('TmaxMales55plus')

# For Tmax with female age groups
plot(tmaxanomDisaggregatedModel, select=5, se=T, shade=TRUE, shade.col='grey', ylab = 'log Relative Risk', xlab = 'logTmaxAnomaly')
abline(0,0)
title('TmaxFemales10_29')

plot(tmaxanomDisaggregatedModel, select=6, se=T, shade=TRUE, shade.col='grey', ylab = 'log Relative Risk', xlab = 'logTmaxAnomaly')
abline(0,0)
title('TmaxFemales30_54')

plot(tmaxanomDisaggregatedModel, select=7, se=T, shade=TRUE, shade.col='grey', ylab = 'log Relative Risk', xlab = 'logTmaxAnomaly')
abline(0,0)
title('TmaxFemales55plus')

dev.off()





}
