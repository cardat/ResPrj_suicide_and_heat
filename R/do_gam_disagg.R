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
    state +
    s(month, k=3, fx=T, bs = 'cc') +
    offset(log(pop)),
  data = anomaly,
  family = poisson
)

# Create a list of the titles
titles <- c('Males 10–29', 'Males 30–54', 'Males 55+', 'Females 10–29', 'Females 30–54', 'Females 55+')

# Create a list of the colors
colors <- c("blue", "blue", "blue", "red", "red", "red")

png("manuscript/01_figures/fig_all_states.png", res=200, width=1000, height=1600)

par(mfcol=c(3,2),
    mgp = c(2.5, 1, 0), # c(axis_title, axis_labels, axis_line)
    mar=c(2,3.5,1,1), 
    oma=c(3,4,0,0),
    cex=0.5) 
# c(bottom, left, top, right)
 
for (i in 1:6) {
  shadeColor <- ifelse(colors[i] == "blue", rgb(0, 0, 1, 0.2), rgb(1, 0, 0, 0.2))
  
  plot(gam_disagg, select=i, se=T, shade=TRUE, shade.col=shadeColor, 
       col=colors[i], ylab='', xlab='',
       xaxs = "i", yaxs = "i", bty = "n")
  abline(h=0, col="black", lty=2)
  title(titles[i])
  box(bty="l")
  # Add the y-axis label to the fourth plot
  
  if (i == 2) {
    mean_point <- mean(par("usr")[3:4])
    text(par("usr")[1]-1.25, mean_point, labels = "log Relative Risk", 
         xpd = TRUE, srt = 90, cex = 1.5)
  }
}

mtext("Temperature Anomaly (ºC)", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()
  
return(gam_disagg)
}
# plot(gam_disagg, select=i, se=T, shade=TRUE, shade.col=rgb(0, 0, 1, 0.2), 
#      col="blue", ylab='', xlab='',
#      xaxs = "i", yaxs = "i", bty = "n")
# abline(h=0, col="black", lty=2)
# title('Males 10–29')
# box(bty="l")
# 
# plot(gam_disagg, select=2, se=T, shade=TRUE, shade.col=rgb(0, 0, 1, 0.2), 
#      col="blue", ylab='', xlab='',
#      xaxs = "i", yaxs = "i", bty = "n")
# abline(h=0, col="black", lty=2)
# title('Males 30–54')
# box(bty="l")
# 
# plot(gam_disagg, select=3, se=T, shade=TRUE, shade.col=rgb(0, 0, 1, 0.2), 
#      col="blue", ylab='', xlab='',
#      xaxs = "i", yaxs = "i", bty = "n")
# abline(h=0, col="black", lty=2)
# title('Males 55+')
# box(bty="l")
# 
# 
# plot(gam_disagg, select=4, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), 
#      col="red", ylab='', xlab='',
#      xaxs = "i", yaxs = "i", bty = "n")
# abline(h=0, col="black", lty=2)
# title('Females 10–29')
# box(bty="l")
# 
# plot(gam_disagg, select=5, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), 
#      col="red", ylab='', xlab='',
#      xaxs = "i", yaxs = "i", bty = "n")
# abline(h=0, col="black", lty=2)
# title('Females 30–54')
# box(bty="l")
# 
# plot(gam_disagg, select=6, se=T, shade=TRUE, shade.col=rgb(1, 0, 0, 0.2), 
#      col="red", ylab='', xlab='',
#      xaxs = "i", yaxs = "i", bty = "n")
# abline(h=0, col="black", lty=2)
# title('Females 55+')
# box(bty="l")

