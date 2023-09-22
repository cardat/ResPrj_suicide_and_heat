library(targets)

lapply(list.files("R", full.names = TRUE), source)

tar_option_set(
  packages =
    c("targets",
      "data.table",
      "lubridate",
      "mgcv",
      "splines"
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
  #### dat ####
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
      dat_pop
    )
  )
  ,
  ### DESCRIPTIVE STATS ####
  #### calc_descriptive ####
  tar_target(
    calc_descriptive,
    do_desc(
      mrg_dat_pop
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
  #### gam_disagg ####
  tar_target(
    gam_disagg,
    do_gam_disagg(
      anomaly
    )
  )
  ,
  #### gam_disagg_states ####
  tar_target(
    gam_disagg_states,
    do_gam_disagg_states(
      anomaly
    )
  )
  ,
  #### gam_disagg_cold_warm ####
  tar_target(
    gam_disagg_cold_warm,
    do_gam_disagg_cold_warm(
      anomaly
    )
  )
  ,
  ### PLOT ####
  #### plot_desc ####
  tar_target(
    plot_desc,
    do_plot_desc(
      calc_descriptive
    )
  )
)
