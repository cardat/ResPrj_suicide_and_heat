load_packages <- function(do_it = T){
  pkgs <- c("targets",
            "data.table",
            "lubridate",
            "mgcv",
            "splines",
            "ggplot2",
            "RColorBrewer")
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  ipak(pkgs)
}