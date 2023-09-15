load_pop <- function(
    dir
){
  pop <- fread(file.path("C:/Users/291828h/CloudStor/Shared/Environment_General/ABS_data/ABS_GCP_2016/data_provided/STE/AUST/2016Census_G01_AUS_STE.csv"))

  # Create a lookup vector
  state_names <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "Other")
  
  # Replace the values in the STE_CODE_2016 column
  pop[, STE_CODE_2016 := state_names[STE_CODE_2016]]
  
  setnames(pop, 
           old = c("STE_CODE_2016", "Tot_P_P", "Tot_P_M", "Tot_P_F"), 
           new = c("state", "pop", "pop_m", "pop_f"))
  
  pop <- pop[, .(state, pop, pop_m, pop_f)]
  
  return(pop)
}