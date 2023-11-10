do_plot_desc <- function(
    descriptive
){
pdf("manuscript/01_figures/fig01_descriptivegcc.pdf", width = 8.3, height = 11.7)

# Get unique gccs
gccs <- c("1GSYD", "1RNSW", 
            "2GMEL", "2RVIC", 
            "3GBRI", "3RQLD", 
            "4GADE", "4RSAU", 
            "5GPER", "5RWAU",
            "6GHOB", "6RTAS",
            "7GDAR", "7RNTE",
            "8ACTE")

# Create titles vector
titles <- gccs

# Set up plotting parameters
par(mfrow = c(length(gccs) + 1, 1),
    mar = c(0.4, 7, 0.4, 5), # c(bottom, left, top, right)
    mgp = c(2.5, 1, 0), # c(axis_title, axis_labels, axis_line)
    las = 1,
    cex.axis = 0.8,
    cex.lab = 1)

# Loop over each gcc
for (i in 1:length(gccs)) {
  st <- gccs[i]
  data_sub <- descriptive[gcc == st]

  # Plot temperatures
  plot(data_sub$date, data_sub$avgmeantemp, 
       type ="l", col="red", 
       xlim = c(min(data_sub$date), max(data_sub$date)),
       ylim = range(c(data_sub$avgtmin, data_sub$avgtmax)), 
       ylab = "", xlab = "Date", 
       xaxt = ifelse(i == length(gccs), "s", "n"),
       xaxs = "i", yaxs = "i") # This means the x-axis is drawn from the smallest to largest data.
  
  # Add shaded area between avgtmin and avgtmax
  polygon(c(data_sub$date, rev(data_sub$date)), 
          c(data_sub$avgtmin, rev(data_sub$avgtmax)), 
          col=adjustcolor("red", alpha.f = 0.2), border=NA)
  
  # Overlay rates with a secondary y-axis
  par(new=TRUE)
  plot(data_sub$date, data_sub$rate, 
       type = "l", col="black", axes=FALSE, xlab=NA, ylab=NA,
       xlim = c(min(data_sub$date), max(data_sub$date)),
       ylim = range(c(data_sub$rate, data_sub$rate_m, data_sub$rate_f)), 
       xaxt = "n",
       xaxs = "i", yaxs = "i")
  
  axis(4, at=axTicks(4), labels=sprintf("%.1f", axTicks(4)))
  
  if (i == length(gccs)) {
    axis(1, at = as.numeric(seq(from = as.Date("2006-01-01"), to = as.Date("2018-11-01"), by = "year")),
         labels = format(seq(from = as.Date("2006-01-01"), to = as.Date("2018-11-01"), by = "year"), "%Y"))
  }
  
  title_text <- titles[i]
  # title(title_text, adj = 0, line = -2.0, side = 2)
  text(x = par("usr")[1] - 0.08 * diff(par("usr")[1:2]), 
       y = mean(par("usr")[3:4]), 
       labels = title_text, 
       xpd = TRUE, 
       adj = 1,
       font = 2) # bold
  
  if (i == 4) {
    mean_point <- mean(par("usr")[3:4])
    text(par("usr")[1]-250, mean_point, labels = "Temperature (ºC)", xpd = TRUE, srt = 90, cex = 1.0)
    text(par("usr")[2]+250, mean_point, labels = "Suicide Rate", xpd = TRUE, srt = 90, cex = 1.0)
  }
  
  # Add vertical grid lines for each year
  years <- seq(from = min(data_sub$date), to = max(data_sub$date), by = "year")
  abline(v = as.numeric(years), col = "black", lty = "dotted")
  
  # Add a box around the plot
  box(col = "black", lwd = 1)
}

par(mar = c(1, 1, 1, 1))  # Set smaller margins
plot(0, 0, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n', frame.plot=FALSE)
legend("center",
       legend=c("Monthly Average Mean Temperature", "Monthly Average Mininum to Maximum Temperature", "Suicide Rate (per 100,000 people)"),
       col=c("red", adjustcolor("red", alpha.f = 0.2), "black"),
       lty= 1,
       lwd=c(1, 5, 1),
       bty="n",
       horiz = FALSE)
dev.off()
}