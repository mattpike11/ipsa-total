library(readr)

read_ipsa_data <- function(year_start){
  ipsa_url <- 'https://www.theipsa.org.uk/api/download?type=totalSpend&year='
  
  year_start_nn <- year_start - 2000
  year_end_nn <- year_start_nn + 1
  
  df <- read.csv(paste(sep = '',
                       ipsa_url,
                       year_start_nn,
                       '_',
                       year_end_nn))
  
  #df$year <- paste(sep='',year_start,'/',year_end_nn)
  
  return(df)
}

df_master <- readr::read_csv("data/totalSpend_22_23.csv")
#read_ipsa_data(2022)

# Identify columns whose names contain "budget" or "spend" but not "reason"
columns_to_process <- grep("budget|spend|uncapped", names(df_master), ignore.case = TRUE, value = TRUE)
columns_to_process <- columns_to_process[!grepl("reason", columns_to_process, ignore.case = TRUE)]

# Function to remove £ symbol and convert to numeric
convert_currency_to_numeric <- function(column) {
  as.numeric(gsub("£", "", gsub(",","",column)))
}

# Apply the function to each identified column
df_master[columns_to_process] <- lapply(df_master[columns_to_process], convert_currency_to_numeric)
