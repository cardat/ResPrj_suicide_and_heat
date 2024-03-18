do_plot_temp <- function(
    dat_temp
){
  # Original gcc_names mapping, do not change
  gcc_names <- c("1GSYD" = "Sydney", "1RNSW" = "Rest of NSW", 
                 "2GMEL" = "Melbourne", "2RVIC" = "Rest of VIC", 
                 "3GBRI" = "Brisbane", "3RQLD" = "Rest of QLD", 
                 "4GADE" = "Adelaide", "4RSAU" = "Rest of SA", 
                 "5GPER" = "Perth", "5RWAU" = "Rest of WA", 
                 "6GHOB" = "Hobart", "6RTAS" = "Rest of TAS", 
                 "7GDAR" = "Darwin", "7RNTE" = "Rest of NT", 
                 "8ACTE" = "ACT", "9AUST" = "Australia")  # Add Australia as the last entry
  
  # Continue with your existing data processing
  dat_temp <- dat_temp[year %between% c(1950, 2019)]
  dat_temp[, tmax_anomaly := tmax - monthly_tmax_avg]
  dat_temp[tmax_anomaly < 0, tmax_anomaly := 0]
  mean_anomaly <- dat_temp[, .(mean_tmax_anomaly = mean(tmax_anomaly, na.rm = TRUE)), by = .(gcc, year)]
  
  # Calculate the overall mean for Australia by year and append it
  australia_overall <- dat_temp[, .(mean_tmax_anomaly = mean(tmax_anomaly, na.rm = TRUE)), by = year]
  australia_overall$gcc <- "9AUST"  # Assign to the Australia category
  
  # Combine with the main data
  mean_anomaly <- rbind(mean_anomaly, australia_overall, fill = TRUE)
  
  # Replace gcc codes with names
  mean_anomaly[, gcc := factor(gcc, levels = names(gcc_names), labels = gcc_names)]
  
  # Prepare for plotting
  min_anomaly <- min(mean_anomaly$mean_tmax_anomaly)
  max_anomaly <- max(mean_anomaly$mean_tmax_anomaly)
  breaks_years <- seq(from = min(mean_anomaly$year), to = max(mean_anomaly$year), by = 10)
  labels_years <- as.character(breaks_years)
  
  # Create the plot
  png_filename <- "figures_and_tables/fig_temp_all.png"
  png(png_filename, res=200, width=1200, height=2000)  # Adjusted for 8 rows
  
  ggplot_object <- ggplot(mean_anomaly, aes(x=factor(year), y=1, fill=mean_tmax_anomaly)) + 
    geom_tile(color="white", height = 1) +
    scale_fill_gradientn(
      colors=brewer.pal(9, "YlOrRd"),
      name="", 
      limits=c(min_anomaly, max_anomaly), 
      breaks=c(min_anomaly, max_anomaly), 
      labels=c(paste(round(min_anomaly, 1), "°C"), paste(round(max_anomaly, 1), "°C"))
    ) +
    labs(x="Year", y="") +
    facet_wrap(~gcc, ncol = 2, scales = "free_x") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle=0),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      legend.key.height = unit(0.5, "cm"),
      panel.spacing = unit(0.3, "lines"),
      strip.text.x = element_text(size = 8),
      panel.background = element_blank(),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank()   
    ) +
    scale_x_discrete(breaks = breaks_years, labels = labels_years) + 
    geom_segment(aes(x = factor(2000), xend = factor(2019), y = 1.51, yend = 1.51), color = "black", size = 1) +
    annotate("text", x = factor(2010), y = 1, label = "Study period", size = 3, angle = 0, hjust = 0.55, vjust = 0)
  
  # Display and save the plot
  print(ggplot_object)
  dev.off()
}

