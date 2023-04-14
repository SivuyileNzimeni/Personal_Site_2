# Libraries ---------------------------------------------------------------
lapply(c("tidyverse","tidycensus","segregation",
         "tigris","sf"),
       require,character.only = TRUE) |> 
  suppressWarnings() |> 
  suppressMessages() |> 
  invisible()

# Modify Data Acquisition -------------------------------------------------
ca_acs_data <- get_acs(geography = "tract",
                       variables = c(
                         white = "B03002_003",
                         black = "B03002_004",
                         asian = "B03002_006",
                         hispanic = "B03002_012"),
                       state = "CA",
                       geometry = TRUE,
                       progress_bar = FALSE,
                       year = 2019) |> 
  suppressMessages() |> 
  invisible() 
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  cache_table = TRUE,
  year = 2019,
  progress_bar = FALSE,
  survey = "acs1") %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME,fixed(", CAUrbanizedArea(2010)")))

ca_urban_data <- ca_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry() |> 
  suppressMessages() 