load_pop <- function(
    dir
){
  pop <- fread(file.path("C:/Users/291828h/CloudStor/Shared/Environment_General/ABS_data/ABS_GCP_2016/data_provided/STE/AUST/2016Census_G01_AUS_STE.csv"))

  return(pop)
}