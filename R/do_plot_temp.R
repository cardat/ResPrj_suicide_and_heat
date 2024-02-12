do_plot_temp <- function(
    dat_temp
){
  
library(RColorBrewer)

foo <- dat_temp[year %between% c(1950, 2019)]
foo[, tmax_anomaly := tmax - monthly_tmax_avg]
foo$tmax_anomaly[foo$tmax_anomaly < 0] <- 0
mean_anomaly <- foo[, .(mean_tmax_anomaly = mean(tmax_anomaly, na.rm = TRUE)), by = .(year)]

df <- as.data.frame(mean_anomaly)

# Calculate the minimum and maximum values for mean_tmax_anomaly
min_anomaly <- min(df$mean_tmax_anomaly)
max_anomaly <- max(df$mean_tmax_anomaly)

# Generate breaks and labels for every 5 years
breaks_years <- seq(from=min(df$year), to=max(df$year), by=5)
labels_years <- as.character(breaks_years)

png_filename <- "figures_and_tables/fig_temp.png"
png(png_filename, res=200, width=1800, height=500)

ggplot_object <- ggplot(df, aes(x=factor(year), y=1, fill=mean_tmax_anomaly)) + 
  geom_tile(color="white") + 
  scale_fill_gradientn(
    # colors=rev(heat.colors(100)),
    colors=brewer.pal(9, "YlOrRd"),
    name="", 
    limits=c(min_anomaly, max_anomaly),
    breaks=c(min_anomaly, max_anomaly), 
    labels=c(paste(round(min_anomaly, 1), "°C"), paste(round(max_anomaly, 1), "°C"))) +
  labs(x="Year", y="", title="") + 
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=0.5),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size=0.5),
    plot.title = element_text(hjust=0.5),
    legend.key.height = unit(0.5, "cm") ,
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5, 
      barwidth = 1, 
      barheight = 5
      )
  ) +
  scale_x_discrete(breaks = breaks_years, labels = labels_years) +
  geom_segment(aes(x = factor(2000), xend = factor(2019), y = 1.51, yend = 1.51), color = "black", size = 1) +
  annotate("text", x = factor(2010), y = 1.61, label = "Study period", size = 3, angle = 0, hjust = 0.8, vjust = 1)

# Display the plot
print(ggplot_object)

dev.off()
}

