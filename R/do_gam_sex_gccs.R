do_gam_sex_gccs <- function(anomaly) {
  unique_gccs <- unique(anomaly$gcc)
  
  gam_gccs <- list()  # Store the GAM models for each gcc
  
  for(gcc_iter in unique_gccs){
    anomaly_gcc <- anomaly[anomaly$gcc == gcc_iter,]
    
    # Create interaction variables
    anomaly_gcc$TmaxMales <- ifelse(anomaly_gcc$sex == 'M', anomaly_gcc$tmax_anomaly, 0)
    anomaly_gcc$TmaxFemales <- ifelse(anomaly_gcc$sex == 'F', anomaly_gcc$tmax_anomaly, 0)
    
    # Build GAM model
    gam_gcc <- gam(
      deaths ~ 
        s(TmaxMales, k=3) + 
        s(TmaxFemales, k=3) +
        age_group * sex * ns(year,3) +
        s(month, k=3, fx=T, bs='cc') + 
        offset(log(pop)),
      data = anomaly_gcc,
      family = poisson
    )
    
    gam_gccs[[gcc_iter]] <- gam_gcc  # Store the model
  }
  
  sex_combinations <- list(
    "Males" = 1,
    "Females" = 2
  )
  
  gcc_names <- c("1GSYD" = "Sydney", "1RNSW" = "Rest of NSW", 
                 "2GMEL" = "Melbourne", "2RVIC" = "Rest of VIC", 
                 "3GBRI" = "Brisbane", "3RQLD" = "Rest of QLD", 
                 "4GADE" = "Adelaide", "4RSAU" = "Rest of SA", 
                 "5GPER" = "Perth", "5RWAU" = "Rest of WA", 
                 "6GHOB" = "Hobart", "6RTAS" = "Rest of TAS", 
                 "7GDAR" = "Darwin", "7RNTE" = "Rest of NT", 
                 "8ACTE" = "ACT")
  
  colors <- c("Males" = "deepskyblue3", "Females" = "brown1")
  
  # Get RGB values of the colors
  rgb_males <- col2rgb(colors["Males"]) / 255
  rgb_females <- col2rgb(colors["Females"]) / 255
  
  # Convert to RGBA with transparency
  shadeColors <- c(
    "Males" = rgb(rgb_males[1], rgb_males[2], rgb_males[3], alpha = 0.1),
    "Females" = rgb(rgb_females[1], rgb_females[2], rgb_females[3], alpha = 0.2)
  )
  
  png_filename <- "figures_and_tables/fig_sex_gccs.png"
  png(png_filename, res=200, width=1300, height=1800)
  
  par(mfrow=c(8, 4),
      mgp = c(2.5, 1, 0),
      mar=c(2,3,1,1), #b,l,t,r
      oma=c(3,3,0,0),
      cex=0.5)
  
  for (i in 1:length(gam_gccs)) {
    for (sex in c("Males", "Females")) {
      term_index <- sex_combinations[[sex]]
      plot(gam_gccs[[i]], select=term_index, se=T, shade=TRUE, shade.col=shadeColors[sex],
           col = colors[sex],
           ylab='', xlab='',
           bty = "n")
      abline(h=0, col="black", lty=2)
      title(gcc_names[unique_gccs[i]], line = -1, cex = 0.8)
      box(bty="l")
    }
  }
  
  plot.new()
  
  par(mar=c(0, 0, 0, 0)) 
  
  legend("center",
         legend=c("Males", "95% CI", "Females", "95% CI"), 
         col=c(
           "deepskyblue3",
           adjustcolor("deepskyblue3", alpha.f = 0.4),
           "brown1",
           adjustcolor("brown1", alpha.f = 0.4)
         ),
         lty=c(1, 1, 1, 1),
         lwd = c(2, 5, 2, 5),
         cex=1.5,
         box.lty=1,
         horiz=FALSE,
         xpd=TRUE,
         inset=c(0, -0.2))

  
  mtext("Temperature Anomaly (ºC)", side = 1, line = 1, cex = 0.8, outer = TRUE)
  mtext("log Relative Risk", side=2, outer=TRUE, line=-0.5, cex=0.8)
  
  dev.off()
  
  return(gam_gccs)  # Return the list of GAM models
}
