library(targets)

lapply(list.files("R", full.names = TRUE), source)

tar_option_set(
  packages =
    c("targets",
      "data.table",
      "lubridate"
    )
)

list(
  ### LOAD DATA ####
  #### dat ####
  tar_target(
    dat,
    load_dat(
      dir
    )
  )
  ,
  #### pop ####
  tar_target(
    pop,
    load_pop(
      dir
    )
  )
  ,
  ### DESCRIPTIVE STATS ####
  #### desc_heat ####
  tar_target(
    desc_heat,
    do_desc_heat(
      dat
    )
  )
  ,
  #### desc_suicide ####
  tar_target(
    desc_desc_suicide,
    do_desc_suicide(
      dat,
      pop
    )
  )
)
