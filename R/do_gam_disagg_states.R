do_gam_disagg_states <- function(
    anomaly
){
  
  unique_states <- unique(anomaly$state)
  
  gam_disagg_states <- list()  # Store the GAM models for each state
  
  for(state_iter in unique_states){
    
    anomaly_state <- anomaly[anomaly$state == state_iter,]
    
    #interaction vars for age_group, sex
    anomaly_state$TmaxMales10_29 <- ifelse(
      anomaly_state$age_group == '10–29' & anomaly_state$sex == 'M', anomaly_state$tmax_anomaly, 0)

    anomaly_state$TmaxMales30_54 <- ifelse(
      anomaly_state$age_group == '30–54' & anomaly_state$sex == 'M', anomaly_state$tmax_anomaly, 0)

    anomaly_state$TmaxMales55plus <- ifelse(
      anomaly_state$age_group == '55+' & anomaly_state$sex == 'M', anomaly_state$tmax_anomaly, 0)

    anomaly_state$TmaxFemales10_29 <- ifelse(
      anomaly_state$age_group == '10–29' & anomaly_state$sex == 'F', anomaly_state$tmax_anomaly, 0)

    anomaly_state$TmaxFemales30_54 <- ifelse(
      anomaly_state$age_group == '30–54' & anomaly_state$sex == 'F', anomaly_state$tmax_anomaly, 0)

    anomaly_state$TmaxFemales55plus <- ifelse(
      anomaly_state$age_group == '55+' & anomaly_state$sex == 'F', anomaly_state$tmax_anomaly, 0)

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
      data = anomaly_state,
      family = poisson
    )
    
    gam_disagg_states[[state_iter]] <- gam_disagg  # Store the model
    
    png(paste0("manuscript/01_figures/fig_", state_iter, ".png"), res=200, width=1000, height=1600)
    
    par(mfrow = c(length(unique_states)+1, 1),
      mfcol=c(3,2), mar=c(2,5,1.5,1), cex=0.5) # c(bottom, left, top, right)
    
    # For TmaxM ages
    plot(gam_disagg, select=1, se=T, shade=TRUE, shade.col=rgb(0, 0, 1, 0.2), 
         col="blue", ylab='', xlab='',
         xaxs = "i", yaxs = "i", bty = "n")
    abline(h=0, col="black", lty=2)
    title('Males 10–29')
    box(bty="l")
    
    plot(gam_disagg, select=2, se=T, shade=TRUE, shade.col=rgb(0, 0, 1, 0.2), 
         col="blue", ylab='', xlab='',
         xaxs = "i", yaxs = "i", bty = "n")
    abline(h=0, col="black", lty=2)
    title('Males 30–54')
    box(bty="l")
    
    plot(gam_disagg, select=3, se=T, shade=TRUE, shade.col=rgb(0, 0, 1, 0.2), 
         col="blue", ylab='', xlab='',
         xaxs = "i", yaxs = "i", bty = "n")
    abline(h=0, col="black", lty=2)
    title('Males 55+')
    box(bty="l")
    
    
    plot(gam_disagg, select=4, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), 
         col="red", ylab='', xlab='',
         xaxs = "i", yaxs = "i", bty = "n")
    abline(h=0, col="black", lty=2)
    title('Females 10–29')
    box(bty="l")

    plot(gam_disagg, select=5, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), 
         col="red", ylab='', xlab='',
         xaxs = "i", yaxs = "i", bty = "n")
    abline(h=0, col="black", lty=2)
    title('Females 30–54')
    box(bty="l")

    plot(gam_disagg, select=6, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), 
         col="red", ylab='', xlab='',
         xaxs = "i", yaxs = "i", bty = "n")
    abline(h=0, col="black", lty=2)
    title('Females 55+')
    box(bty="l")
    
    dev.off()
  }
  
  return(gam_disagg_states)  # Return the list of GAM models
}
