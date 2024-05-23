library(readr)

year_start <- 2010
year_end <- 2023


read_ipsa_data <- function(year_start,year_end){
  ipsa_url <- 'https://www.theipsa.org.uk/api/download?type=totalSpend&year='
  
  year_start_nn <- year_start - 2000
  year_end_nn <- year_end - 2000
  
  df <- read.csv(
    paste(sep = '',
          ipsa_url,
          year_start_nn,
          '_',
          year_end_nn
          ))

}

df <- read_csv('data/totalSpend_22_23.csv')


