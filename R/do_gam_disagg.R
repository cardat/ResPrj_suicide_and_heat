do_gam_disagg <- function(
    anomaly
){
# interaction vars for age_group, sex
anomaly$TmaxMales10_29 <- ifelse(
  anomaly$age_group == '10-29' & anomaly$sex == 'M', anomaly$tmax_anomaly, 0)
anomaly$TmaxMales30_54 <- ifelse(
  anomaly$age_group == '30-54' & anomaly$sex == 'M', anomaly$tmax_anomaly, 0)
anomaly$TmaxMales55plus <- ifelse(
  anomaly$age_group == '55+' & anomaly$sex == 'M', anomaly$tmax_anomaly, 0)

anomaly$TmaxFemales10_29 <- ifelse(
  anomaly$age_group == '10-29' & anomaly$sex == 'F', anomaly$tmax_anomaly, 0)
anomaly$TmaxFemales30_54 <- ifelse(
  anomaly$age_group == '30-54' & anomaly$sex == 'F', anomaly$tmax_anomaly, 0)
anomaly$TmaxFemales55plus <- ifelse(
  anomaly$age_group == '55+' & anomaly$sex == 'F', anomaly$tmax_anomaly, 0)

gam_disagg <- gam(
  deaths ~ 
    s(TmaxMales10_29, k=3) + 
    s(TmaxMales30_54, k=3) + 
    s(TmaxMales55plus, k=3) +
    s(TmaxFemales10_29, k=3) + 
    s(TmaxFemales30_54, k=3) + 
    s(TmaxFemales55plus, k=3) +
    age_group * sex * ns(year,3) +
    state +
    s(month, k=4, fx=T, bs = 'cc') +
    offset(log(pop)),
  data = anomaly,
  family = poisson
)


png("manuscript/01_figures/fig_all_states_3knots.png", res=200, width=1000, height=1600)

par(mfcol=c(3,2), mar=c(4,5,2,1), cex=0.5)

# For TmaxM ages
plot(gam_disagg, select=2, se=T, shade=TRUE, shade.col='grey', ylab='log Relative Risk', xlab='TmaxAnomaly')
abline(0,0)
title('TmaxMales10_29')

plot(gam_disagg, select=3, se=T, shade=TRUE, shade.col='grey', ylab='log Relative Risk', xlab='TmaxAnomaly')
abline(0,0)
title('TmaxMales30_54')

plot(gam_disagg, select=4, se=T, shade=TRUE, shade.col='grey', ylab='log Relative Risk', xlab='TmaxAnomaly')
abline(0,0)
title('TmaxMales55plus')

# For Tmax f and ages
plot(gam_disagg, select=5, se=T, shade=TRUE, shade.col='grey', ylab='log Relative Risk', xlab='TmaxAnomaly')
abline(0,0)
title('TmaxFemales10_29')

plot(gam_disagg, select=6, se=T, shade=TRUE, shade.col='grey', ylab='log Relative Risk', xlab='TmaxAnomaly')
abline(0,0)
title('TmaxFemales30_54')

plot(gam_disagg, select=7, se=T, shade=TRUE, shade.col='grey', ylab='log Relative Risk', xlab='TmaxAnomaly')
abline(0,0)
title('TmaxFemales55plus')

dev.off()

return(gam_disagg)
}