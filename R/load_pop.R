load_pop <- function(
    dir
){
  pop <- fread(file.path("C:/Users/291828h/CloudStor/Shared/Environment_General/ABS_data/ABS_GCP_2016/data_provided/GCCSA/AUST/2016Census_G01_AUS_GCCSA.csv"))

  
  # Create a lookup vector
  gcc_names <- c("1GSYD", "1RNSW", 
                 "2GMEL", "2RVIC", 
                 "3GBRI", "3RQLD", 
                 "4GADE", "4RSAU", 
                 "5GPER", "5RWAU",
                 "6GHOB", "6RTAS",
                 "7GDAR", "7RNTE",
                 "8ACTE")
  
  # select relevant gccsas
  pop <- pop[pop$GCCSA_CODE_2016 %in% gcc_names, ]
  
  
  setnames(pop, 
           old = c("GCCSA_CODE_2016", "Tot_P_P", "Tot_P_M", "Tot_P_F"), 
           new = c("gcc", "pop", "pop_m", "pop_f"))
  
  pop <- pop[, .(gcc, pop, pop_m, pop_f)]
  
  return(pop)
}