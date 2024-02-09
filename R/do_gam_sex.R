do_gam_sex <- function(
    anomaly
){

# Fit the model
gam_sex <- gam(
  deaths ~ 
    s(TmaxMales, k=3) + 
    s(TmaxFemales, k=3) +
    age_group * sex * ns(year,3) +
    gcc +
    s(month, k=3, fx=T, bs = 'cc') +
    offset(log(pop)),
  data = anomaly,
  family = poisson
)

return(gam_sex)

# # Plot the results
# png("figures_and_tables/fig_sex.png", res=200, width=1000, height=1000)
# 
# par(mfcol=c(2,1), mar=c(4,5,2,1), cex=0.5)
# 
# # For Tmax males
# plot(gam_sex, select=1, se=T, shade=TRUE,
#      shade.col=adjustcolor("deepskyblue3", alpha.f = 0.1), col="deepskyblue3",
#      ylab='log Relative Risk', xlab='TmaxAnomaly')
# abline(h=0, col="black", lty=2)
# title('Males All Ages')
# 
# # For Tmax females
# plot(gam_sex, select=2, se=T, shade=TRUE,
#      shade.col=adjustcolor("brown1", alpha.f = 0.2), col="brown1",
#      ylab='log Relative Risk', xlab='TmaxAnomaly')
# abline(h=0, col="black", lty=2)
# title('Females All Ages')
# dev.off()


}
