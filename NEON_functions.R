library(httr)
library(jsonlite)
library(tidyverse)
library(neonUtilities)

## function for quickly getting NEON data products dp #'s
find_neon_dp<- function(search_for) {

  ## if neon data products not in environment -> get
  if (!"neon_data_products" %in% ls(.GlobalEnv)) {
    print("Creating neon_data_products table")
    call <- "http://data.neonscience.org/api/v0/products/"
    details <- GET(url = call)
    
    Sys.sleep(3)
    print(paste0("Connecting Status ",status_code(details)))
    
    if (status_code(details) != 200) {
      ## try again
      details <- GET(url = call)
      
      Sys.sleep(3)
      print(paste0("trying again, Connecting Status ",status_code(details)))
    }
    
    headers <- c(
      'Content-Type' = 'application/json'
    )
    
    res <- VERB("GET", url = call,
                #body = body,
                add_headers(headers))
    
    data<- fromJSON(rawToChar(res$content))
    
    product_code<- data$data$productCode
    product_name<- data$data$productName
    
    neon_data_products<<- cbind(product_name, product_code)
  } #else {print("neon_data_products already exists")}

  p<- grep(search_for, neon_data_products[,1], ignore.case = T)
  d_res<- neon_data_products[c(p),]
  return(d_res)
}
#find_neon_dp(search_for = "tick")

#View(neon_data_procucts)

