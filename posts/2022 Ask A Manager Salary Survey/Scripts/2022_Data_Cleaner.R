start_time <- Sys.time()
# Libraries ---------------------------------------------------------------
lapply(as.list(c("tidyverse","janitor","arrow",
                 "ggthemes","tidytext","tidymodels",
                 "textrecipes","glmnet")),
       require,character.only=TRUE) |>
  suppressWarnings() |> 
  suppressMessages() |> 
  invisible()
# Import Data -------------------------------------------------------------
Salary_Survey <- read_csv(file = "./Data/Ask A Manager Salary Survey 2022_Responses.csv") |> 
  clean_names() |> 
  mutate(timestamp = mdy_hms(timestamp)) |> 
  filter(!is.na(job_title)) |> 
  mutate(additional_compensation = ifelse(is.na(additional_compensation),
                                          0,additional_compensation),
         full_compensation = salary+additional_compensation) |> 
  filter(!currency %in% c("Other","HKD")) |> 
  rownames_to_column("respondent")
# Completeness Function ---------------------------------------------------
completeness <- function(a_vec){
  incomplete <- round(100/length(a_vec)*a_vec[is.na(a_vec)] |> length(),2)
return(incomplete)}

Valid_Columns <- sapply(Salary_Survey,completeness) |> 
  data.frame() 

Valid_Columns$variable <- rownames(Valid_Columns)

names(Valid_Columns) <- c("percentage","variable")

Salary_Survey <- Salary_Survey[,names(Salary_Survey) %in% c(Valid_Columns[Valid_Columns$percentage<5,"variable"])]

rm(Valid_Columns)
# Additional Cleaning -----------------------------------------------------
Salary_Survey |>
  group_by(employers_industry) |> 
  summarise(number_of_entries =n(),
            .groups = "drop") |> 
  arrange(desc(number_of_entries))

Salary_Survey |> 
  group_by(job_title) |> 
  summarise(number_of_entries = n(),
            .groups = "drop") |> 
  arrange(desc(number_of_entries)) 

# Open Text Cleaner Function ----------------------------------------------
text_cleaner <- function(a_vec){
  tokens <- str_split(a_vec,pattern="\\s{1,}") |> 
    unlist()
  tokens <- tokens[!grepl("\\s{1,}|[[:punct:]]|\\d{1,}",tokens)] |> str_to_lower()
  tokens <- unique(tokens) 
  tokens <- tokens[!tokens %in% c(stopwords::data_stopwords_nltk[["en"]])]
  return(tokens)}
# Create New Db -----------------------------------------------------------
Salary_Df <- Salary_Survey |> 
  mutate(employers_industry = map(employers_industry,text_cleaner),
         functional_area_of_job = map(functional_area_of_job,text_cleaner),
         job_title = map(job_title,text_cleaner),
         country = map(country,text_cleaner),
         race = map(race,text_cleaner),
         city = map(city,text_cleaner)
         )
# List to Vec -------------------------------------------------------------
list_to_vec <- function(a_vec){
  new_vec <- a_vec |> 
    as.character()
new_vec <- gsub("(^c+[[:punct:]])|[[:punct:]]",
                replacement = "",
                new_vec)
  return(new_vec)}

# Per Respondent ----------------------------------------------------------------
Salary_Df <- Salary_Df |> 
  group_by(across(!where(is.list))) |> 
  summarise(across(where(is.list),
                   .fns=list_to_vec),
            .groups = "drop") 
# Convert to Dollar -------------------------------------------------------
Currencies <- data.frame(iso_code = Salary_Df$currency |> unique()) |> 
  mutate(iso_code = str_extract(iso_code,"^[[:upper:]]{3,}"))
# Scrape World Currencies -------------------------------------------------
lapply(as.list(c("rvest","xml2")),
       require,character.only = TRUE) |> 
  suppressWarnings() |> 
  suppressMessages()
# Pull Table --------------------------------------------------------------
Currency_Info <- read_html("https://en.wikipedia.org/wiki/List_of_circulating_currencies") |> 
  html_table()

Currency_Info <- Currency_Info[[2]] |> 
  clean_names()

names(Currency_Info) <- gsub("([[:punct:]]{1}\\d{1,}){1,}",
                             replacement = "",
                             names(Currency_Info))

Currency_Info <- Currency_Info |> 
  filter(iso_code %in% c(Currencies$iso_code))
# Collect Exchange Rates --------------------------------------------------
# Data Source: International Monetary Fund Exchange Rates Query. 
# Website: www.imf.org
# Data Range: 17 December 2021 - 17 December 2022

Exchange_Rates <- read_tsv(file = "./Data/Exchange_Rate_Report.tsv")
Exchange_Rates <- Exchange_Rates[,-c(1,41)]

# Pivot Long_Exchange -----------------------------------------------------
Exchange_Rates <- Exchange_Rates |> 
  pivot_longer(cols = !starts_with("Date"),
               names_to = "currency_details",
               values_to = "exchange_rate") |> 
  filter(!is.na(exchange_rate)) |> 
  mutate(exchange_rate = str_remove(exchange_rate,
                                    "\\s{1,}.{1,}") |> as.double(),
         iso_code = str_extract(currency_details,"[[:punct:]].{1,}[[:punct:]]"),
         currency_details = str_remove(currency_details,
                                       pattern= "[[:punct:]].{1,}[[:punct:]]"),
         iso_code = str_remove_all(iso_code,
                                   "[[:punct:]]"),
         iso_code = str_extract(iso_code,"[[:upper:]]{3}")) |> 
  filter(!is.na(exchange_rate)) |>
  group_by(iso_code) |> 
  summarise(exchange_rate = median(exchange_rate),
            .groups = "drop")

Currencies <- left_join(Currencies,Exchange_Rates) |> 
  rename(currency = iso_code)
# Expand Currency Information ---------------------------------------------
Salary_Df <- left_join(Salary_Df,Currencies) |> 
  mutate(full_comp_usd = full_compensation*exchange_rate) |> 
  select(-c(full_compensation,exchange_rate,currency,respondent,
            timestamp,salary,additional_compensation))

Salary_Df <- Salary_Df |> 
  na.omit() |> 
  filter(full_comp_usd > 10000)
# Preprocessing -----------------------------------------------------------
Salary_Modelling <- recipe(full_comp_usd ~ .,data = Salary_Df) |> 
  step_tokenize(employers_industry:race) |> 
  step_tfidf(employers_industry:race) |> 
  step_log(all_outcomes()) |>
  step_nzv(all_numeric_predictors()) |> 
  prep() |> 
  bake(new_data = NULL)

# Sanity Check ------------------------------------------------------------
Salary_Modelling <- unique(Salary_Modelling)
Salary_Modelling <- na.omit(Salary_Modelling)
# For Reproducibility Purposes --------------------------------------------
set.seed(2022)
# Lasso Regression: Feature Selection -------------------------------------
X <- model.matrix(full_comp_usd ~ .,data =Salary_Modelling)[,-1]
Y <- Salary_Modelling$full_comp_usd

Lasso_Model <- glmnet(x=X,y =Y,
                      family = "gaussian",
                      alpha=1)

plot(Lasso_Model)
# Variable Extractor ------------------------------------------------------
variable_extractor <- function(a_list){
  min_lambda <- a_list$lambda |> min()
  last_model <- length(a_list$lambda)
  lasso_variables <- as.matrix(coef(a_list))[,last_model] |> 
    data.frame()
  lasso_variables$variable <- rownames(lasso_variables)
  names(lasso_variables) <- c("coef","variable")
  lasso_variables <- lasso_variables[,c("variable","coef")]
  lasso_variables <- lasso_variables[!grepl(pattern = "[[:punct:]]Intercept[[:punct:]]",
                                            lasso_variables$variable),]
return(lasso_variables)}

Salary_Modelling <- cbind(Salary_Modelling[,grep("full_comp_usd",names(Salary_Modelling))],
Salary_Modelling[,names(Salary_Modelling) %in% variable_extractor(Lasso_Model)$variable])
# Split Data --------------------------------------------------------------
Training_Salary <- Salary_Modelling[sample(nrow(Salary_Modelling),
                        size = floor(nrow(Salary_Modelling)*0.80),
                        replace = F),]

Testing_Salary <- anti_join(Salary_Modelling,Training_Salary)

end_time <- Sys.time()

end_time - start_time

rm(list = grep("Testing|Training",ls(),value = TRUE,invert = T))
#===========================================================================