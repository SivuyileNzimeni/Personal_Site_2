# Libraries ---------------------------------------------------------------
lapply(c("tidyverse","janitor","readxl","writexl",
         "sf","spdep"),
       require,
       character.only = TRUE) |> 
  suppressWarnings() |> 
  suppressMessages()

sf_use_s2(FALSE)
# Import Data -------------------------------------------------------------
National_2019 <- read_csv(file ="./posts/2024 Election Results/Results/National_2019/Nationa_2019.csv") |>
  clean_names()

National_2024 <- read_csv(file = "./posts/2024 Election Results/Results/National_2024/National_2024.csv") |> 
  clean_names()

New_Stations <- setdiff(unique(c(National_2024$vd_number)),
        unique(c(National_2019$vd_number)))
# Station Aggregates ------------------------------------------------------
National_2019_Aggregate <- National_2019[,c('vd_number',"registered_population","total_valid_votes","spoilt_votes")] |> 
  unique() |> 
  mutate(registered_population = as.integer(registered_population)) |> 
  filter(!is.na(registered_population)) 

National_2024_Aggregate <- National_2024[,c("vd_number","registered_population","total_valid_votes","spoilt_votes")] |> 
  unique() |> 
  mutate(registered_population = as.integer(registered_population)) |> 
  filter(!is.na(registered_population))
# Import Stations Coordinates ---------------------------------------------
Voting_Stations <- read.csv(file = "./posts/2024 Election Results/Data/2024_Voting_Stations_SANEF.csv") |> 
  mutate(across(ends_with('tude'),
                .fns = \(x){
                  gsub(",",
                       replacement = ".",
                       x) |> 
                    as.double()
                })) |> 
  select(province,ward,name,votingdistrict,municipality,
         longitude,latitude) |> 
  unique()

Voting_Stations <- Voting_Stations |> 
  mutate(main_place = str_extract(municipality,
                                  "(?<=\\[).{1,}[[:punct:]]"),
         main_place = str_remove(main_place,"[[:punct:]]$"),
         municipality = str_remove(municipality,
                                   "\\[.{1,}"),
         municipal_code = str_extract(municipality,
                                      "(?!\\-)\\w{1,}"),
         municipality = str_remove(municipality,
                                   "(?!\\-)\\w{1,}") |> 
           str_remove("^\\s{1,}[[:punct:]]{1,}\\s{1,}") |> str_trim(
           )) |> 
  unique()

National_2019_Aggregate <- left_join(National_2019_Aggregate,
          Voting_Stations |> 
            rename(vd_number=votingdistrict) |> 
            select(vd_number,ward))

National_2024_Aggregate <- left_join(National_2024_Aggregate,
                                     Voting_Stations |> 
                                       rename(vd_number=votingdistrict) |> 
                                       select(vd_number,ward))

National_2019_Aggregate <- National_2019_Aggregate |> 
  group_by(ward) |> 
  summarise(registered_population = sum(registered_population),
            total_valid_votes = sum(total_valid_votes),
            total_spoilt_votes = sum(spoilt_votes),
            .groups = "drop") |> 
  mutate(turn_out_2019 = (100/registered_population)*(total_valid_votes+total_spoilt_votes))


National_2019_Aggregate <- National_2019_Aggregate[!National_2019_Aggregate$turn_out_2019>=100,]

National_2024_Aggregate <- National_2024_Aggregate |> 
  group_by(ward) |> 
  summarise(registered_population = sum(registered_population),
            total_valid_votes = sum(total_valid_votes),
            total_spoilt_votes = sum(spoilt_votes),
            .groups = "drop") |> 
  mutate(turn_out_2024 = (100/registered_population)*(total_valid_votes+total_spoilt_votes))

National_2024_Aggregate <- National_2024_Aggregate[!National_2024_Aggregate$turn_out_2024>=100,]


National_Turnout <- left_join(National_2024_Aggregate[,c("ward","turn_out_2024")],
          National_2019_Aggregate[,c("ward","turn_out_2019")])

National_Turnout <- National_Turnout |> 
  na.omit()

Ward_Turnout <- National_Turnout

Ward_Turnout <- Ward_Turnout |> 
  mutate(turnout_diff = turn_out_2024-turn_out_2019)

# Add SA Wards ------------------------------------------------------------
SA_Wards <- st_read("./posts/2023 TidyTuesday Week 1/2020_Spatial_Data/SA_Wards2020.shp") |> 
  clean_names()

SA_Wards <- left_join(SA_Wards |> 
            rename(ward = ward_id) |> 
            mutate(ward = as.integer(ward)),Ward_Turnout)

sf::write_sf(SA_Wards,
             paste0("SA_Wards_Turn_Out_Difference_1_",Sys.Date(),".GeoJSON"))
