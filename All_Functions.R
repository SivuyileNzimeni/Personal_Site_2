# Libraries ---------------------------------------------------------------
sivu_is_a_lib <- function(){
  lapply(as.list(c("tidyverse","janitor",
                   "readxl","writexl","glue",
                   "ggthemes","arrow","pdftools",
                   "tidytext","textrecipes")),
         require,character.only=TRUE)
  theme_set(theme_clean())
}

# All Excel Sheets --------------------------------------------------------
all_excel <- function(path){
  require("readxl")
  collect_sheets <- excel_sheets(path)
  number_of_sheets <- 1:length(collect_sheets)
  per_sheet <- list()
  for(i in seq_along(number_of_sheets)){
    per_sheet[[i]] <- read_xlsx(path = path,
                                sheet = collect_sheets[i])
  }
  return(per_sheet)}
# Lasso_Variable_Extraction -----------------------------------------------
variable_extractor <- function(a_list){
  min_lambda <- data.frame(best_lambda =a_list[["lambda"]]==min(a_list[["lambda"]]))
  min_lambda <- cbind.data.frame(data.frame(index = rownames(min_lambda)),
                                 min_lambda)
  min_lambda <- min_lambda %>% 
    filter(best_lambda == TRUE)
  min_lambda <- as.double(unique(min_lambda$index))
  lasso_variables <- as.matrix(coef(a_list))|>data.frame()
  lasso_variables <- cbind.data.frame(variables = rownames(lasso_variables),
                                      lasso_variables)
  lasso_variables <- tibble(lasso_variables)
  lasso_variables <- lasso_variables[,c(1,min_lambda+1)]
  names(lasso_variables) <- c("variable","importance")
  lasso_variables <- lasso_variables %>% 
    filter(variable != "(Intercept)",
           importance != 0.00000000)
  
  return(lasso_variables)
}
# List to Character -------------------------------------------------------
list_to_character <- function(x){
  require("stringr")
  require("dplyr")
  require("tidyr")
  x <- str_replace(x,"^c[[:punct:]]{1,}","")
  x <- str_replace_all(x,"[[:punct:]]{1,}$","")
  x <- str_replace_all(x,'\\"',"")
  return(x)}
# Remove Numbering --------------------------------------------------------
remove_numbering <- function(x){
  y <- data.frame(variable_names = colnames(x))
  y <- y %>% 
    mutate(variable_names = gsub("^\\w{1}\\d{1,}[[:punct:]]|^\\w{1}\\d{1,}[[:punct:]]\\d{1,}[[:punct:]]","",
                                 variable_names))
  colnames(x) <- y$variable_names
  return(x)}

# File_Type Extractor -----------------------------------------------------
extract_file_type <- function(file_url){
  x <- GET(url=file_url)
  file_type <- x[["headers"]][["content-type"]]
  file_type <- str_replace(file_type,"application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                           ".docx")
  file_type <- str_replace(file_type,"application[[:punct:]]{1,}pdf",".pdf")
  file_type <- str_replace(file_type,"image[[:punct:]]{1,}jpeg",".jpeg")
  file_type <- str_replace(file_type,"image[[:punct:]]{1,}png",".png")
  return(file_type)
}






