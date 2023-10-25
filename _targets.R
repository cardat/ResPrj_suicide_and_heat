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
  #### gam_sex ####
  tar_target(
    gam_sex,
    do_gam_sex(
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
  #### gam_disagg_wa ####
  tar_target(
    gam_disagg_wa,
    do_gam_disagg_wa(
      anomaly
    )
  )
  ,
  #### bootstrap ####
  tar_target(
    bootstrap_wa,
    do_bootstrap_wa(
      anomaly, 
      gam_disagg_wa, 
      st = "WA", 
      ag = "10–29",
      sx = "M"
    )
  )
  ,
  #### an ####
  tar_target(
    an_wa,
    do_an_wa(
      anomaly,
      gam_disagg_wa
    )
  )
  ,
  # #### bootstrap ####
  # tar_target(
  #   bootstrap,
  #   do_bootstrap(
  #     anomaly, 
  #     gam_disagg,
  #     st = "st", 
  #     ag = "ag",
  #     sx = "sx"
  #   )
  # )
  # ,
  # #### an ####
  # tar_target(
  #   an,
  #   do_an(
  #     anomaly, 
  #     gam_disagg
  #   )
  # )
  # ,
  ### PLOT ####
  #### plot_desc ####
  tar_target(
    plot_desc,
    do_plot_desc(
      calc_descriptive
    )
  )
  ,
  ### TABLES ####
  #### tab_desc ####
  tar_target(
    tab_desc,
    do_tab_desc(
      calc_descriptive,
      anomaly
    )
  )
)
