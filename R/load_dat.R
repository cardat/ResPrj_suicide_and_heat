load_dat <- function(
    dir
){
  dat <- fread(file.path("data_provided/CAUSES_GCC.csv"))
  
  dat <- dat[Cause == "Suicide"]
  
  qc <- dat[, .(tdeaths = sum(Deaths)), by = Age_group]
  
  dat[, Age_group := gsub("\x96", "–", Age_group)]

  # lowercase
  setnames(dat, old = names(dat), new = tolower(names(dat)))
  
  # create 55+
  dat[age_group %in% c(
    "55–64", 
    "65–74", 
    "75–84", 
    "85+"), 
    age_group := "55+"]

  # Remove the 'GCC' prefix from 'gcc_code' values
  dat[, gcc_code := gsub("^GCC", "", gcc_code)]
  
  # Map of month names to numbers
  month_map <- setNames(1:12, month.name)
  
  # Convert month names to numbers
  dat[, month := month_map[month]]
  
  # Convert sex values
  dat[, sex := ifelse(sex == "Males", "M", ifelse(sex == "Females", "F", sex))]
  
  # Remove rows where sex is 'Persons'
  dat <- dat[sex != "Persons"]
  
  setnames(dat, old = "gcc_code", new = "gcc")
  
  # keep only the ones we need
  dat <- dat[, .(gcc, year, month, age_group, sex, deaths)]
  
  return(dat)
}