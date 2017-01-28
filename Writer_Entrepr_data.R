# ------------------------
# Data pull from TCdata360 API
# ------------------------
library(jsonlite)
library(tidyverse)
# Query data based on ids of filtered indicators
# loop by country and indicator id. Bind it all in a data.frame
# Query country metadata:
countries <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/countries/?fields=id%2Ciso2%2Ciso3%2Cname%2Cregion%2CincomeLevel%2ClendingType%2CcapitalCity%2Cgeo",
                      flatten = TRUE)
# Query indicators:
indicators <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/indicators?fields=id%2Cname%2Cdataset%2CvalueType%2CdatasetId%2Cnotes%2Cproperties%2Crank%2Cdefinition",
                       flatten=TRUE)
# Read data description file (what goes in the PDF report)
dataDesc <- read.csv("Entrepr_DataDescription.csv", stringsAsFactors = FALSE)

indicatorsID <- filter(dataDesc, !is.na(tcdata360_id))$tcdata360_id

indicators_Entrep <- indicators %>%
  filter(id %in% indicatorsID) %>% 
  distinct(id,.keep_all=TRUE) %>%
  arrange(name)


Entrepr_data <- data.frame()
specialchars <- paste(c("[-]","[.]"),collapse = "|")
#for (cou in c("BRA","ARG","ECU","URY","BOL")){
for (cou in countries$id){
  for (ind in indicators_Entrep$id){
    print(paste0("Processing...",cou," ",ind))
    thisQuery <- fromJSON(paste0("http://datascope-prod.amida-demo.com/api/v1/data?countries=",cou,
                                 "&indicators=",ind),
                          flatten = TRUE)
    if (length(thisQuery$data)>0){
      thisQuery <- flatten(thisQuery$data$indicators[[1]])
      if (!is.null(thisQuery$estimated)){
        thisQuery$estimated <- NULL
        thisQuery <- as.data.frame(thisQuery)
      }  
      thisQuery <- thisQuery %>%
        mutate(iso3 = cou)
      names(thisQuery) <- gsub("values.","",names(thisQuery),fixed=TRUE)
      names(thisQuery) <- ifelse(grepl(specialchars,names(thisQuery)),substr(names(thisQuery),1,4),names(thisQuery))
      # consolidate quarterly data by the 4th quarter
      names(thisQuery) <- gsub("Q4","",names(thisQuery))
      thisQuery <- select(thisQuery, -dplyr::contains("Q"))
      
      if (nrow(Entrepr_data)==0) {
        Entrepr_data <- thisQuery
      } else {
        Entrepr_data <- bind_rows(Entrepr_data,thisQuery)
      }
    }
  }
}

# create Period variable
Entrepr_data <- gather(Entrepr_data, Period, Observation, -iso3,-id)

# -----------------------------------------------------------
write.csv(Entrepr_data,"/Users/asanchez3/Desktop/Work/TCMN/Entrepreneurship_data/Entrepr_data.csv",row.names = FALSE)
# -----------------------------------------------------------


