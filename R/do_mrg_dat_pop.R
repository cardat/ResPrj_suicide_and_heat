do_mrg_dat_pop <- function(
    dat,
    pop
){
  mrg_dat_pop <- merge(
    dat, pop,
    by = c("state"),
    all = TRUE
  )
  
  return(mrg_dat_pop)
}