do_gam_disagg_wa <- function(
    anomaly
){
#tar_load(anomaly)
  
  # interaction vars

anomaly[, summer_tmax_anomaly := ifelse(
  month %in% c(12, 1, 2), 
  tmax_anomaly, 0)]
 
anomaly[, TmaxMales10_29 := ifelse(
  age_group == '10–29' & 
    sex == 'M' & 
    state == "WA", 
  tmax_anomaly, 0)]
  

gam_disagg_wa <- gam(
  deaths ~
    s(TmaxMales10_29, k=3) +
    s(summer_tmax_anomaly, k=3) +
    ns(year,3) +
    s(month, k=3, fx=T, bs = 'cc') +
    offset(log(pop)),
  data = anomaly,
  family = poisson
)

png("manuscript/01_figures/fig_boys_wa_positive.png", res=200, width=1200, height=900)

# For TmaxM ages
plot(gam_disagg_wa, select=1, se=T, shade=TRUE, rug = TRUE, shade.col=rgb(1, 0, 0, 0.2), col="red",  ylab='log Relative Risk', xlab='TmaxAnomaly')
abline(h=0, col="black", lty=2)
title('Males 10–29')

dev.off()

return(gam_disagg_wa)
}