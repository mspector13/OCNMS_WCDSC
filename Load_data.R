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

#view(`tbl_Structure_Forming_Invertebrates - All OCNMS`)

#unique(`tbl_Structure_Forming_Invertebrates - All OCNMS`$invertebrate_code)

SFI_codes <- `tbl_Structure_Forming_Invertebrates - All OCNMS` %>% 
  select(dive,
         invertebrate_code) %>% 
 mutate(Code = invertebrate_code)

names_invertcodes <- names_invertcodes %>% 
  mutate(Code = as.character(Code))

SFI_names = merge(SFI_codes, names_invertcodes, "Code")

SFI_data <- SFI_names %>% 
  select(dive, Scientific_name, Common_name) %>% 
  #mutate(count = 1) %>% 
  group_by(dive, Scientific_name, Common_name) %>% 
  summarize(total = n()) %>% 
  filter(!(Scientific_name =='Placeholder'))

SFI_phy_name <-  SFI_data %>% 
  mutate(phylum = case_when(
    str_detect(Scientific_name, "Porifera") ~ "Porifera",
    str_detect(Scientific_name, "Asbest") ~ "Cnidaria",
    str_detect(Scientific_name, "Calcig") ~ "Cnidaria",
    str_detect(Scientific_name, "Desmop") ~ "Cnidaria",
    str_detect(Scientific_name, "Farrea") ~ "Porifera",
    str_detect(Scientific_name, "Hexa") ~ "Cnidaria",
    str_detect(Scientific_name, "Paragorgia") ~ "Cnidaria",
    str_detect(Scientific_name, "Plexauridae") ~ "Cnidaria",
    str_detect(Scientific_name, "Plumarella") ~ "Cnidaria",
    str_detect(Scientific_name, "Poecillastra") ~ "Porifera",
    str_detect(Scientific_name, "Polymastia") ~ "Porifera",
    str_detect(Scientific_name, "Rhabdocalyptus") ~ "Porifera",
    str_detect(Scientific_name, "Swiftia") ~ "Cnidaria",
    str_detect(Scientific_name, "Halipteris") ~ "Cnidaria",
    str_detect(Scientific_name, "Lophelia") ~ "Cnidaria",
    str_detect(Scientific_name, "NA") ~ "Unknown Cnidaria",
    str_detect(Scientific_name, "Pandalus") ~ "Arthropoda",
    str_detect(Scientific_name, "Pennatulacea") ~ "Cnidaria",
    str_detect(Scientific_name, "Plexauridae") ~ "Cnidaria",
    str_detect(Scientific_name, "Primnoa") ~ "Cnidaria",
    str_detect(Scientific_name, "Stylaster") ~ "Cnidaria",
    )) 
  
#SFI_phylum = merge(SFI_name_phylum, SFI_data, "Scientific_name") THIS SHOULD HAVE WORKED WTF

### Plot some invertebrate data

#select invert codes + dive number to plot abundance data

SFI_phy_name %>% 
  ggplot(aes(fill = dive, y=total, x=phylum)) + 
  geom_bar(position="dodge", stat="identity")

