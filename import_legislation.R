library(here)
library(striprtf)
library(tidyverse)
library(stringr)

#Get the directories of all the acts
acts_list_dir <- list.files(path = here("raw_data", "download_14112018", "A"), 
                        pattern = "rtf$", 
                        recursive = TRUE)

#Extract the act titles
acts_list <- ""

for (n in 1:length(acts_list_dir)) {
  
acts_list[n] <-  str_extract(acts_list_dir[n], ".+?(?=/)") #https://stackoverflow.com/questions/40113963/how-to-extract-everything-until-first-occurrence-of-pattern

}

#Get the text of the acts
acts_content <- ""

for (n in 1:length(acts_list)) {

  acts_content[n] <- read_rtf(here("raw_data", "download_14112018", "A", acts_list_dir[n])) %>%
    paste(collapse = " ")

}

#Put it all in a data frame
acts_df <- data.frame(acts_list_dir, acts_list, acts_content)
#Save that data frame because it took ages to create
saveRDS(acts_df, paste0(here("derived_data"), "/acts_df.rds"))