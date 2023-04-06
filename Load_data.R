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

# Let's start with structure forming inverts ------------------------------
#take original data.frame, select only what we want (dive # and invert code), and then rename "invertebrate_code" to Code, this will be useful in a moment. Also note, because each ROW is an observation, this gives us frequency and abundance data
SFI_codes <- `tbl_Structure_Forming_Invertebrates - All OCNMS` %>% 
  select(dive,
         invertebrate_code) %>% 
 mutate(Code = invertebrate_code)

#we need R to recognize "Code" as a character, NOT an integer/number
names_invertcodes <- names_invertcodes %>% 
  mutate(Code = as.character(Code))

#merge the data sets by a common variable, Code
SFI_names = merge(SFI_codes, names_invertcodes, "Code")

#Final processing set, take only the columns we want, group_by them and then get a summary of UNIQUE observations based on this grouping (i.e., how many times was a given Scientific/Common_name *thing* seen on each dive?). Finally, remove "Placeholder" observations because they don't mean anything here. 
SFI_data <- SFI_names %>% 
  select(dive, Scientific_name, Common_name) %>% 
  #mutate(count = 1) %>% 
  group_by(dive, Scientific_name, Common_name) %>% 
  summarize(total = n()) %>% 
  filter(!(Scientific_name =='Placeholder'))

#SFI_phylum = merge(SFI_name_phylum, SFI_data, "Scientific_name") THIS SHOULD HAVE WORKED WTF!?

#Because the above code *didnt* work, we have to use case_when and str_detect to add the qualifier "phylum" to our data. Otherwise, we'd end up plotting the abundance/frequency of _each_ observed invert!
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
  
# Plot invert data --------------------------------------------------------

#geom_bar() - bar charts are nice for abundance data. ggplot2 is great for making fancy figures
SFI_phy_name %>% 
  #ggplot(aes(fill = phylum, y=total, x=dive)) + 
  ggplot(aes(y=total, x=phylum)) +
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  theme_classic() +
  facet_grid(cols = vars(dive),
             scales = "free_x", space = "free") +
  theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(margin = margin(r = 10))) +
  labs(x = "", 
       y = "Total number of SFI observations",
       title = "2019 WCDSC Cruise",
       subtitle = "Observations by dive") +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) 



# Time for fish data ---------------------------------------------------------------

fish_data <- `dbo_tbl_FISH - All OCNMS` %>% 
    select(dive, fishcode, size_class)

fish_code <- dbo_names_fishcodes %>% 
  select(fishcode, Scientific_Name, Common_Name, Family, category)

fish_abun = merge(fish_data, fish_code, "fishcode") %>%
  mutate(dive = case_when(
    str_detect(dive, "1") ~ "OCNMS1",
    str_detect(dive, "2") ~ "OCNMS2",
    str_detect(dive, "3") ~ "OCNMS3",
    str_detect(dive, "4") ~ "OCNMS4",
    str_detect(dive, "5") ~ "OCNMS5")) %>% 
  mutate(across('Family', str_replace, 'na', 'Pleuronectiformes')) %>% 
  group_by(dive, Scientific_Name, Common_Name, Family) %>% 
  summarize(total = n()) %>% 
  drop_na()


# Let's plot fish data ----------------------------------------------------
fish_abun %>% 
  ggplot(aes(y=total, x=Family)) + 
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  theme_classic() +
  facet_grid(cols = vars(dive),
             scales = "free", space = "free") +
  theme(panel.spacing = unit(2, "lines")) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 0.90),
        axis.title.y = element_text(margin = margin(r = 10))) +
  labs(x = "", 
       y = "Total number of fish observations",
       title = "2019 WCDSC Cruise",
       subtitle = "Observations by dive") +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5))



