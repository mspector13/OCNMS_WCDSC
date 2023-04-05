#Load packages _lazily_
load_pkg <- rlang::quos(tidyverse, lubridate, readxl, janitor)

invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE))



#Get ze data
pathname <- "./data/" #name the file pathway (i.e., where in the directy the files are stored)
file_names <- list.files(path = pathname, 
                         full.names = F,
                         pattern='*.xlsx') #get a list of the names of the files (only .xlsx)
#Kris's for loop that takes the names of the files stores in file_names, and pastes them iteratively over each file as it reads it in (via read_xlsx). Note: file.temp is a temporary object that "floats" for each iterative process 
for (temp_name in file_names){
  file.temp <- read_xlsx(paste0(pathname, temp_name))
  assign(gsub(".xlsx", "", temp_name, fixed = T), file.temp)
}

