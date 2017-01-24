# ------------------------
# Data pull from TCdata360 API
# ------------------------
library(jsonlite)
# Query data based on ids of filtered indicators
# loop by country and indicator id. Bind it all in a data.frame
# Query country metadata:
countries <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/countries/?fields=id%2Ciso2%2Ciso3%2Cname%2Cregion%2CincomeLevel%2ClendingType%2CcapitalCity%2Cgeo",
                      flatten = TRUE)
# Query indicators:
indicators <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/indicators?fields=id%2Cname%2Cdataset%2CvalueType%2CdatasetId%2Cnotes%2Cproperties%2Crank%2Cdefinition",
                       flatten=TRUE)
# read data extracted from API. 
## ---- Run Writer_Entrepr_data.R to update data from TCdata360 API
Entrepr_data <- read.csv("/Users/asanchez3/Desktop/Work/TCMN/Entrepreneurship_data/Entrepr_data.csv",stringsAsFactors = FALSE)

# Read data description file (what goes in the PDF report)
dataDesc <- read.csv("Entrepr_DataDescription.csv", stringsAsFactors = FALSE)