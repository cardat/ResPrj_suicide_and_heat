do_gam <- function(
    anomaly
){
gam <- gam(
  deaths ~ 
    s(tmax_anomaly) + # s(tmax) the s specifies a non-linear(smoothed relationship between response and predictor tmax_anomaly)
    age_group * sex * ns(year,3) + # interaction between agegrp, sex, and natural spline on year with 3 basis functions
    gcc + # ctegorical
    s(month, k=3, fx=T, bs = 'cc') + # another smooth - effect of month. 4 basis functions, TRUE degree of freedom, bs is cyclic cubic spline - useful for cyclical like m and yr.
    offset(log(pop)), # offset to model rates, no raw counts
  data=anomaly,
  family=poisson)

# tmaxanomedf <- summary(tmaxanomModel)$edf
# # [1] 1.016887
# summary(tmaxanomModel)

# Plot
png("figures_and_tables/fig_season_risk.png",res=200,width = 1000, height = 1000)
par(mar = c(4, 4, 1, 1), # c(bottom, left, top, right)
    mgp = c(2.5, 1, 0), # c(axis_title, axis_labels, axis_line)
    las = 1,
    cex.axis = 0.8,
    cex.lab = 1)

plot(gam, select = 2,
     shade = TRUE, shade.col="grey", col="black", lwd=2, seWithMean = TRUE,
     ylab = 'log Relative Risk',xlab='Month', ylim = c(-0.1, 0.1))
abline(h=0, lty=2, col="black")
abline(v=9, lty=2, col="black") # September
abline(v=11, lty=2, col="black") # November
# Place the text at a higher position with increased font size (cex)
text(x = 10, y = -0.025, labels = "Spring", col = "black")  

dev.off()

return(gam)
}
