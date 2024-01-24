do_gam_disagg <- function(
    anomaly
){
#tar_load(anomaly)
  
gam_disagg <- gam(
  deaths ~
    s(TmaxMales, k=3) +
    s(TmaxMales10_29, k=3) +
    s(TmaxMales30_54, k=3) +
    s(TmaxMales55plus, k=3) +
    s(TmaxFemales, k=3) +
    s(TmaxFemales10_29, k=3) +
    s(TmaxFemales30_54, k=3) +
    s(TmaxFemales55plus, k=3) +
    age_group * sex * ns(year,3) +
    s(month, k=3, fx=T, bs = 'cc') +
    offset(log(pop)),
  data = anomaly,
  family = poisson
)
return(gam_disagg)
}
