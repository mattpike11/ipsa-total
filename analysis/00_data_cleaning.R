library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)
library(stringr)


read_ipsa_data <- function(yr) {
  ipsa_url <- "https://www.theipsa.org.uk/api/download?type=totalSpend&year="
  
  # back up as DLUHC desktop blocks IPSA website
  #ipsa_url <- "bkup_data/totalSpend_"
  
  file_ext <- ifelse(ipsa_url == "bkup_data/totalSpend_",".csv","")
  
  year_start_nn <- yr - 2000
  year_end_nn <- year_start_nn + 1
  
  df <- read.csv(paste0(ipsa_url, year_start_nn, '_', year_end_nn, file_ext))
  
  df$MP.name <- df[,1]
  
  df <- df %>%
    rename_with(~ str_replace_all(., c("\\.maximum" = "", "\\.available" = "", "\\.uncapped\\." = "spend")))
  
  df <- df %>%
    mutate(location = ifelse(str_detect(tolower(Reason.for.budget.set), "non-london"), "Non-London", "London"),
           year = paste0(yr, '/', year_end_nn)) %>%
    select(
      year,
      MP.name,
      Constituency,
      location,
      Office.budget,
      Office.spend,
      Remaining.office.budget,
      Staffing.budget,
      Staffing.spend,
      Remaining.staffing.budget,
      Accommodation.budget,
      Accommodation.spend,
      Remaining.accommodation.budget,
      Travel.and.subsistence.spend,
      Other.costs.spend
    )
  
  # Identify columns whose names contain "budget" or "spend" but not "reason"
  columns_to_process <- grep("budget|spend", names(df), ignore.case = TRUE, value = TRUE)
  
  # Function to remove non-numeric characters and convert to numeric
  convert_currency_to_numeric <- function(column) {
    as.numeric(gsub("[^0-9.-]", "", column))
  }
  
  # Apply the function to each identified column
  df[columns_to_process] <- lapply(df[columns_to_process], function(col) convert_currency_to_numeric(as.character(col)))
  
  return(df)
  
}

combine_ipsa_time_series <- function(year_start,year_end){
  if(year_end >= 2023 | year_start <= 2009){
    return("Error: Years must range between 2010 and 2022")
  }
  
  df <- read_ipsa_data(year_start)
  
  if(year_end - year_start == 1){
    return(df)
  }
  
  for (year in (year_start+1):year_end){
    df <- rbind(df,read_ipsa_data(year))
  }
  return(df)
}

df_master <- combine_ipsa_time_series(2013,2022)
