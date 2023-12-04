do_gam_disagg_states <- function(
    anomaly
){
  
  unique_gccs <- unique(anomaly$gcc)
  
  gam_disagg_gccs <- list()  # Store the GAM models for each gcc
  
  for(gcc_iter in unique_gccs){
    
    anomaly_gcc <- anomaly[anomaly$gcc == gcc_iter,]
    
    #interaction vars for age_group, sex
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
    
    gam_disagg_gccs[[gcc_iter]] <- gam_disagg  # Store the model
    
    # Create a list of the titles
    titles <- c('Males 0–29', 'Males 30–54', 'Males 55+', 'Females 0–29', 'Females 30–54', 'Females 55+')
    
    # Create a list of the colors
    colors <- c("blue", "blue", "blue", "red", "red", "red")
    
    png(paste0("manuscript/01_figures/fig_", gcc_iter, ".png"), res=200, width=1600, height=1000)
    
    # Set up the graphical parameters for 2 rows and 3 columns
    par(mfrow=c(2,3),
        mgp = c(2.5, 1, 0), # c(axis_title, axis_labels, axis_line)
        mar=c(2,3.5,1,1), 
        oma=c(3,3,3,0), # bottom, left, top, right
        cex=0.5)
    
    # Plot the males (blues) in the first row
    for (i in 1:3) {
      shadeColor <- rgb(0, 0, 1, 0.2) # Blue with transparency
      
      plot(gam_disagg, select=i, se=T, shade=TRUE, shade.col=shadeColor, 
           col="blue", ylab='', xlab='',
           xaxs = "i", yaxs = "i", bty = "n")
      abline(h=0, col="black", lty=2)
      title(titles[i])
      box(bty="l")
    }
    
    # Plot the females (reds) in the second row
    for (i in 4:6) {
      shadeColor <- rgb(1, 0, 0, 0.2) # Red with transparency
      
      plot(gam_disagg, select=i, se=T, shade=TRUE, shade.col=shadeColor, 
           col="red", ylab='', xlab='',
           xaxs = "i", yaxs = "i", bty = "n")
      abline(h=0, col="black", lty=2)
      title(titles[i])
      box(bty="l")
      
    }
    mtext(gcc_iter, side=3, line=1, cex=1.5, outer=TRUE)
    mtext("Temperature Anomaly (ºC)", side = 1, line = 1, cex = 0.8, outer = TRUE)
    mtext("log Relative Risk", side=2, outer=TRUE, line=-0.5, cex=0.8)
    dev.off()
  }
  
  return(gam_disagg_gccs)  # Return the list of GAM models
}
