do_gam_disagg_cold_warm <- function(
    anomaly
){
  
  # Aggregate TAS and ACT into one group
  anomaly$adjusted_gcc <- ifelse(anomaly$gcc %in% c("6GHOB", "6RTAS", "8ACTE"), "gccTAS_ACT", "warmer_gccs")
  
  unique_gccs <- c("gccTAS_ACT", "warmer_gccs")
  
  gam_disagg_cold_warm <- list()  # Store the GAM models for each gcc
  
  for(gcc_iter in unique_gccs){
    
    anomaly_gcc <- anomaly[anomaly$adjusted_gcc == gcc_iter,]
    
    # interaction vars for age_group, sex
    anomaly_gcc$TmaxMales10_29 <- ifelse(
      anomaly_gcc$age_group == '0–29' & anomaly_gcc$sex == 'M', anomaly_gcc$tmax_anomaly, 0)

    anomaly_gcc$TmaxMales30_54 <- ifelse(
      anomaly_gcc$age_group == '30–54' & anomaly_gcc$sex == 'M', anomaly_gcc$tmax_anomaly, 0)

    anomaly_gcc$TmaxMales55plus <- ifelse(
      anomaly_gcc$age_group == '55+' & anomaly_gcc$sex == 'M', anomaly_gcc$tmax_anomaly, 0)

    anomaly_gcc$TmaxFemales10_29 <- ifelse(
      anomaly_gcc$age_group == '0–29' & anomaly_gcc$sex == 'F', anomaly_gcc$tmax_anomaly, 0)

    anomaly_gcc$TmaxFemales30_54 <- ifelse(
      anomaly_gcc$age_group == '30–54' & anomaly_gcc$sex == 'F', anomaly_gcc$tmax_anomaly, 0)

    anomaly_gcc$TmaxFemales55plus <- ifelse(
      anomaly_gcc$age_group == '55+' & anomaly_gcc$sex == 'F', anomaly_gcc$tmax_anomaly, 0)

    gam_disagg <- gam(
      deaths ~ 
        s(TmaxMales10_29, k=3) + 
        s(TmaxMales30_54, k=3) + 
        s(TmaxMales55plus, k=3) +
        s(TmaxFemales10_29, k=3) + 
        s(TmaxFemales30_54, k=3) + 
        s(TmaxFemales55plus, k=3) +
        age_group * sex * ns(year,3) +
        s(month, k=3, fx=T, bs = 'cc') +
        offset(log(pop)),
      data = anomaly_gcc,
      family = poisson
    )
    
    gam_disagg_cold_warm[[gcc_iter]] <- list(
      model = gam_disagg,
      data = anomaly_gcc  # Store the data subset as well for future reference
    )
    
    png(paste0("manuscript/01_figures/fig_cold_warm_", gcc_iter, ".png"), res=200, width=1000, height=1600)
    
    par(mfcol=c(3,2), mar=c(4,5,2,1), cex=0.5)
    
    # For TmaxM ages
    plot(gam_disagg, select=1, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), col="red", ylab='log Relative Risk', xlab='TmaxAnomaly')
    abline(h=0, col="black", lty=2)
    title('Males 0–29')
    
    plot(gam_disagg, select=2, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), col="red", ylab='log Relative Risk', xlab='TmaxAnomaly')
    abline(h=0, col="black", lty=2)
    title('Males 30–54')
    
    plot(gam_disagg, select=3, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), col="red", ylab='log Relative Risk', xlab='TmaxAnomaly')
    abline(h=0, col="black", lty=2)
    title('Males 55+')
    
    # For Tmax f and ages
    plot(gam_disagg, select=4, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), col="red", ylab='log Relative Risk', xlab='TmaxAnomaly')
    abline(h=0, col="black", lty=2)
    title('Females 0–29')
    
    plot(gam_disagg, select=5, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), col="red", ylab='log Relative Risk', xlab='TmaxAnomaly')
    abline(h=0, col="black", lty=2)
    title('Females 30–54')
    
    plot(gam_disagg, select=6, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), col="red", ylab='log Relative Risk', xlab='TmaxAnomaly')
    abline(h=0, col="black", lty=2)
    title('Females 55+')
    
    dev.off()
  }
  
  return(gam_disagg_cold_warm)  # Return the list of GAM models
}
