library(httr)
library(jsonlite)
library(tidyverse)
library(neonUtilities)

call <- "http://data.neonscience.org/api/v0/products/"

details <- GET(url = call)
status_code(details)
#text <- content(details, "parsed", encoding = "UTF-8")
#text <- content(details, "raw", encoding = "UTF-8")

headers <- c(
  'Content-Type' = 'application/json'
)

res <- VERB("GET", url = call,
            #body = body,
            add_headers(headers))

data<- fromJSON(rawToChar(res$content))
product_code<- data$data$productCode
product_name<- data$data$productName

neon_data_procucts<- cbind(product_name, product_code)
View(neon_data_procucts)

search_for<- "tick"
p<- grep(search_for, neon_data_procucts[,1], ignore.case = T)
d_res<- neon_data_procucts[c(p),]



tick_code<- "DP1.10093.001"

## SRER
tick_data<- loadByProduct(tick_code, startdate = "2016-01", site = "SRER", check.size = F)

f_data<- tick_data$tck_fielddata
View(f_data)
f_data_2<- f_data %>% 
  filter(targetTaxaPresent != "N")
View(f_data_2)

## JORN
tick_data_jorn<- loadByProduct(tick_code, startdate = "2015-01", site = "JORN", check.size = F)

f_data<- tick_data_jorn$tck_fielddata
#View(f_data)
f_data_2<- f_data %>% 
  filter(targetTaxaPresent != "N")
View(f_data_2)

tax<- tick_data_jorn$tck_taxonomyProcessed
View(tax)

dna<- "DP1.10038.001"
sampled<- "DP1.10043.001"

mos_data_srer<- loadByProduct(sampled, site = "SRER", check.size = F)

tax_moa<- mos_data_srer$mos_expertTaxonomistIDProcessed
tax_moa<- tax_moa %>% 
  filter(targetTaxaPresent != "N")
#View(tax_moa)


# ---------------------

sav<- tapply(X = tax_moa$individualCount, INDEX = tax_moa$taxonID, FUN = sum)
## OR add col
tax_moa$sumCounts <- ave(tax_moa$individualCount, tax_moa$taxonID, FUN=sum)

# ---------------------

mos_data_srer<- loadByProduct(sampled, site = "JORN", check.size = F)

tax_moa<- mos_data_srer$mos_expertTaxonomistIDProcessed
tax_moa<- tax_moa %>% 
  filter(targetTaxaPresent != "N")
View(tax_moa)

un_mos<- unique(tax_moa$taxonID)
length(un_mos)
un_mos

path<- "DP1.10041.001"
path_data_srer<- loadByProduct(path, site = "JORN", check.size = F)

a<- path_data_srer$mos_pathogenresults %>% 
  filter(testResult == "Negative")
View(a)


path_data_srer<- loadByProduct(path, site = "SRER", check.size = F)

d<- path_data_srer$mos_pathogenresults %>% 
  filter(testResult != "Negative")
View(d)

