do_gam_sex_age <- function(
    anomaly
){
  

# Update age group categories to match your dataset
age_groups <- c("0–29", "30–54", "55+")

# Colors for the plots
colors <- c("Males" = "deepskyblue3", "Females" = "brown1")

# Build the GAM model for the entire dataset
gam_model <- gam(
  deaths ~ 
    s(TmaxMales10_29, k=3) + 
    s(TmaxMales30_54, k=3) + 
    s(TmaxMales55plus, k=3) +
    s(TmaxFemales10_29, k=3) + 
    s(TmaxFemales30_54, k=3) + 
    s(TmaxFemales55plus, k=3) +
    age_group * sex * ns(year,3) +
    gcc +
    s(month, k=3, fx=T, bs='cc') + 
    offset(log(pop)),
  data = anomaly,
  family = poisson
)

# Set up the plotting area
png("figures_and_tables/fig_sex_age.png", res=200, width=1600, height=900)
par(mfrow=c(2, 3), mar=c(4, 4, 2, 1))

# Define plot titles
plot_titles <- c("Males 0–29", "Males 30–54", "Males 55+", "Females 0–29", "Females 30–54", "Females 55+")

# Loop over the smooth terms in the model and plot
for (i in 1:6) {
  # Determine the sex based on the loop index to set colors
  sex <- ifelse(i <= 3, "Males", "Females")
  
  # Plot the model's effect of temperature anomaly on deaths
  plot(gam_model, select=i, main=plot_titles[i], col=colors[sex], xlab="Temperature Anomaly (°C)", ylab="log Relative Risk", se=TRUE, shade=TRUE, shade.col=adjustcolor(colors[sex], alpha.f=0.3))
  abline(h=0, col="black", lty=2)  # Add a horizontal line at y=0
}

dev.off()

return(gam_model)
}

