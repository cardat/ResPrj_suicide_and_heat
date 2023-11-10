do_mrg_dat_pop <- function(
    dat,
    pop,
    dat_temp
){
  mrg_dat_pop <- merge(
    dat, pop,
    by = c("gcc"),
    all = TRUE
  )
  
  dat_temp <- dat_temp[year %between% c(2000, 2019)]
  
  
  mrg_dat_pop <- merge(
    mrg_dat_pop, dat_temp,
    by = c("gcc", "year", "month"),
    all = TRUE
  )
  
  # keep only summer
  # mrg_dat_pop <- mrg_dat_pop[mrg_dat_pop$month %in% c(1, 2, 12), ]
  
  return(mrg_dat_pop)
}