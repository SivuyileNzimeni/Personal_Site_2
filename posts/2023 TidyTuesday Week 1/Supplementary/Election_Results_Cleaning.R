# Libraries ---------------------------------------------------------------
lapply(as.list(c("tidyverse","janitor",
                 "readxl","writexl",
                 "tidymodels","arrow")),
       require,character.only=TRUE)
# Import Data -------------------------------------------------------------
Election_Files <- data.frame(election_files =list.files(full.names=TRUE,
           pattern =  ".csv",
           recursive = TRUE)) %>% 
  mutate(election_results = map(election_files,read_csv,
                                locale = locale(encoding = "latin1")))


Election_Files <- Election_Files %>% 
  split(.$election_files)

Election_Files <- map(Election_Files,unnest,election_results)

Elections_2016 <- Election_Files[[1]]

Election_Files <- Election_Files[-1]

Election_Files <- do.call(rbind,Election_Files)

Elections_2021 <- Election_Files
rm(Election_Files)

Election_Files <- bind_rows(Elections_2016 %>% 
  mutate(election_year = 2016),
Elections_2021 %>% 
  mutate(election_year =2021)) %>% 
  rename(source_file = election_files) %>% 
  clean_names()

rm(list = grep("\\d{1,}",ls(),value = TRUE))

# Write_New_Dbs -----------------------------------------------------------
dir.create("Final_Dbs")

write_csv(Election_Files,
          file = paste0("./Final_Dbs/2016 - 2021_Local_Gov_Election_Results_",Sys.Date(),".csv"))

write_parquet(Election_Files,
                paste0("./Final_Dbs/2016 - 2021_Local_Gov_Election_Results_",Sys.Date(),".parquet"))
# Create_Subsets ---------------------------------------------------------
