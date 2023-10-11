do_gam_sex <- function(
    anomaly
){
  


# Interaction vars for sex only
anomaly$TmaxMales <- ifelse(anomaly$sex == 'M', anomaly$tmax_anomaly, 0)
anomaly$TmaxFemales <- ifelse(anomaly$sex == 'F', anomaly$tmax_anomaly, 0)

# Fit the model
gam_sex <- gam(
  deaths ~ 
    s(TmaxMales, k=3) + 
    s(TmaxFemales, k=3) +
    sex * ns(year,3) +
    state +
    s(month, k=3, fx=T, bs = 'cc') +
    offset(log(pop)),
  data = anomaly,
  family = poisson
)

# Plot the results
png("manuscript/01_figures/fig_sex_only.png", res=200, width=1000, height=800)

par(mfcol=c(2,1), mar=c(4,5,2,1), cex=0.5)

# For Tmax males
plot(gam_sex, select=1, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), col="red", ylab='log Relative Risk', xlab='TmaxAnomaly')
abline(h=0, col="black", lty=2)
title('Males All Ages')

# For Tmax females
plot(gam_sex, select=2, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), col="red", ylab='log Relative Risk', xlab='TmaxAnomaly')
abline(h=0, col="black", lty=2)
title('Females All Ages')

dev.off()
}
