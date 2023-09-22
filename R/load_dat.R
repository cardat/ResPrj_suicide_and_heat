load_dat <- function(
    dir
){
  dat <- fread(file.path("data_provided/linked_heat_suicide_20210407.csv"))
  
  # drops kids, zero occurances
  dat <- dat[age_group != "00–09"]
  
  # create 55+
  dat[age_group %in% c(
    "55–64", 
    "65–74", 
    "75–84", 
    "85+"), 
    age_group := "55+"]
  
  return(dat)
}