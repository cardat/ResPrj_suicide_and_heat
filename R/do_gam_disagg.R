do_gam_disagg <- function(
    anomaly
){
#tar_load(anomaly)
  
gam_disagg <- gam(
  deaths ~
    s(TmaxMales10_29, k=3) +
    s(TmaxMales30_54, k=3) +
    s(TmaxMales55plus, k=3) +
    s(TmaxFemales10_29, k=3) +
    s(TmaxFemales30_54, k=3) +
    s(TmaxFemales55plus, k=3) +
    age_group * sex * ns(year,3) +
    gcc +
    s(month, k=3, fx=T, bs = 'cc') +
    offset(log(pop)),
  data = anomaly,
  family = poisson
)

# Create a list of the titles
titles <- c('Males 0–29', 'Males 30–54', 'Males 55+', 'Females 0–29', 'Females 30–54', 'Females 55+')

# Create a list of the colors
colors <- c("blue", "blue", "blue", "red", "red", "red")

png("manuscript/01_figures/fig_all_gccs.png", res=200, width=1600, height=1000)




# Set up the graphical parameters for 2 rows and 3 columns
par(mfrow=c(2,3),
    mgp = c(2.5, 1, 0), # c(axis_title, axis_labels, axis_line)
    mar=c(2,3.5,1,1), 
    oma=c(3,3,0,0),
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

mtext("Temperature Anomaly (ºC)", side = 1, line = 1, cex = 0.8, outer = TRUE)
mtext("log Relative Risk", side=2, outer=TRUE, line=-0.5, cex=0.8)
dev.off()
return(gam_disagg)
}
