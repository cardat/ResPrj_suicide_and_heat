do_mrg_dat_pop <- function(
    dat,
    pop
){
  mrg_dat_pop <- merge(
    dat, pop,
    by = c("state"),
    all = TRUE
  )
  
  # keep only summer
  # mrg_dat_pop <- mrg_dat_pop[mrg_dat_pop$month %in% c(1, 2, 12), ]
  
  return(mrg_dat_pop)
}