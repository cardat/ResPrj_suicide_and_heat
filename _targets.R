library(targets)

lapply(list.files("R", full.names = TRUE), source)

tar_option_set(
  packages =
    c("targets",
      "data.table",
      "lubridate",
      "mgcv",
      "splines",
      "ggplot2"
    )
)

list(
  ### LOAD DATA ####
  #### dat ####
  tar_target(
    dat_suicide,
    load_dat(
      dir
    )
  )
  ,
  #### pop ####
  tar_target(
    dat_pop,
    load_pop(
      dir
    )
  )
  ,
  #### dat_temp ####
  tar_target(
    dat_temp,
    load_temp(
      dir
    )
  )
  ,
  ### MERGE ####
  #### mrg_dat_pop ####
  tar_target(
    mrg_dat_pop,
    do_mrg_dat_pop(
      dat_suicide,
      dat_pop,
      dat_temp
    )
  )
  ,
  ### DESCRIPTIVE STATS ####
  #### calc_descriptive ####
  tar_target(
    calc_descriptive,
    do_desc(
      dat_pop,
      dat_suicide
    )
  )
  ,
  #### plot_temp ####
  tar_target(
    plot_temp,
    do_plot_temp(
      dat_temp
    )
  )
  ,
  ### PREP FOR MODELING ####
  #### anomaly ####
  tar_target(
    anomaly,
    do_tmax_anomaly(
      mrg_dat_pop,
      dat_temp
    )
  )
  ,
  
  ### MODEL ####
  #### gam ####
  tar_target(
    gam,
    do_gam(
      anomaly
    )
  )
  ,
  #### gam_sex ####
  tar_target(
    gam_sex,
    do_gam_sex(
      anomaly
    )
  )
  ,
  #### gam_sex_gcc ####
  tar_target(
    gam_sex_gcc,
    do_gam_sex_gccs(
      anomaly
    )
  )
  ,
  #### gam_sex_age ####
  tar_target(
    gam_sex_age,
    do_gam_sex_age(
      anomaly
    )
  )
  ,
  #### gam_sex_age_gcc ####
  tar_target(
    gam_sex_age_gcc,
    do_gam_sex_age_gccs(
      anomaly
    )
  )
  ,
  ### ATTRIBUTABLE NUMBER ####
  #### bootstrap ####
  tar_target(
    bootstrap,
    do_bootstrap(
      anomaly,
      gam_sex
    )
  )
  ,
  #### tab_an ####
  tar_target(
    tab_an,
    do_tab_an(
      bootstrap
    )
  )
)
