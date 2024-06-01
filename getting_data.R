library(httr)
library(jsonlite)
library(tidyverse)
library(neonUtilities)

source("NEON_functions.R")

find_neon_dp(search_for = "plant")

cat("Select one...\n")
cat(paste0(data_dpID_df$product_name,"\n"))

product_pick<- data_dpID_df["Plant foliar traits" == data_dpID_df$product_name,]

product_pick$product_code

neon_data<- loadByProduct(product_pick$product_code,
                          site = "SRER",
                          startdate = "2020-01",
                          enddate = "2022-12",
                          nCores = 2,
                          forceParallel = T,
                          tabl = "all",
                          check.size = F)

field_data<- neon_data$cfc_fieldData %>% 
  select(!c("uid"))

lignin<- neon_data$cfc_lignin %>% 
  select(!c("domainID", "siteID", "plotID", "uid", "plotType", "collectDate",
            "namedLocation", "sampleCode", "release", "publicationDate"))
names(lignin)[names(lignin)=="dataQF"]<- "dataQF_lignin"
names(lignin)[names(lignin)=="remarks"]<- "remarks_lignin"
names(lignin)[names(lignin)=="dryMass"]<- "dryMass_lignin"



lma<- neon_data$cfc_LMA %>% 
  select(!c("domainID", "siteID", "uid", "plotType", "collectDate",
            "namedLocation", "sampleCode", "release", "publicationDate"))
names(lma)[names(lma)=="dataQF"]<- "dataQF_lma"
names(lma)[names(lma)=="remarks"]<- "remarks_lma"
names(lma)[names(lma)=="dryMass"]<- "dryMass_lma"



# View(lignin)
# View(lma)
# View(field_data)


t<- full_join(field_data, lignin, by="sampleID")

t2<- full_join(t, lma, by="sampleID")
View(t2)

write.csv(field_data, "tables/fd.csv")
write.csv(lignin, "tables/lignin.csv")
write.csv(lma, "tables/lma.csv")













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

