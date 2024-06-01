library(httr)
library(jsonlite)
library(tidyverse)
library(neonUtilities)

## function for quickly getting NEON data products dp #'s
find_neon_dp<- function(search_for) {

  ## if neon data products not in environment -> get
  if (!"neon_data_products" %in% ls(.GlobalEnv)) {
    headers <- c(
      'Content-Type' = 'application/json'
    )
    call <- "http://data.neonscience.org/api/v0/products/"
    
    cat(paste0("Trying to connect..."))
    details <- GET(url = call, add_headers(headers))
    cat("\n")

    for (i in c(10:1)) {
      cat(paste0("...",i,"... \r"))
      Sys.sleep(1)
      #cat(" ")
    }

    cat(paste0("Connecting Status ",status_code(details)))
    cat("\n")
    
    if (status_code(details) != 200) {
      ## try again
      details <- GET(url = call)
      
      for (i in c(10:1)) {
        cat(paste0("...",i,"... \r"))
        Sys.sleep(1)
        #cat(" ")
      }
      
      print(paste0("trying again, Connecting Status ",status_code(details)))
    }
    cat("\n")
    cat("Creating neon_data_products table...")
    
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
  
  cat("\n")
  Sys.sleep(1)
  ## save to global environment
  data_dpID<- as.data.frame(d_res)
  data_dpID_df<<- data_dpID
  
  return(d_res)
} ## End of function

# find_neon_dp(search_for = "plant")
# View(data_dpID_df)

### Function to get Fulcrum data with just sql query as input - from Greg Chapman
getFulcrumData <- function(sql,verbose=FALSE) {
  token <- fulcrumToken # Read-only API token for Fulcrum
  url <-  paste0("https://api.fulcrumapp.com/api/v2/query?token=", token,
                 "&format=json", "&q=", URLencode(sql), "&headers=true")
  request <- httr::GET(url, add_headers("X-ApiToken"=token, Accept="application/json"))
  content <- jsonlite::fromJSON(httr::content(request, as="text"))
  data.return <- content$rows
  # if(verbose==TRUE){print(content(request))}
  return(content) #I switched to return content rather than content$rows
}
