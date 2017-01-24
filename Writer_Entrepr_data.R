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

# Add descriptors and source fields
Entrepr_data <- merge(Entrepr_data,dataDesc, by.x = "id", by.y = "tcdata360_id")
Entrepr_data <- merge(Entrepr_data, countries[,c("iso3","iso2","name","region")],by="iso3",all.x = TRUE)
# clean up: remove duplicate columns
Entrepr_data <- Entrepr_data %>%
  select(Key = id, Country = name, Period, Observation, CountryCode = iso3, iso2,  
         IndicatorShort = varname, Source = Source_Link, Unit = Unit.of.Measure, 
         Section, Subsection, Subsection2, region)

# Missing indicators from TCdata360
load("/Users/asanchez3/Desktop/Data Analysis/Entrepreneurship-Ind/Testapp/all datasets.rda")
missInd <- select(all.datasets$WB.data, iso2 = iso2c, Period = year, Observation = one_of("SL.SRV.EMPL.ZS")) %>%
  mutate(var = "SL.SRV.EMPL.ZS") %>%
  join(dataDesc, by = "var") %>%
  join(countries[,c("iso3","iso2","name","region")], by = "iso2") %>%
  filter(!is.na(iso3)) %>%
  mutate(Period = as.character(Period)) %>%
  select(Key = tcdata360_id, Country = name, Period, Observation, CountryCode = iso3, iso2,  
         IndicatorShort = varname, Source = Source_Link, Unit = Unit.of.Measure, 
         Section, Subsection, Subsection2, region)

# Append to master data file
Entrepr_data <- bind_rows(Entrepr_data, missInd)


write.csv(Entrepr_data,"Entrepr_data.csv",row.names = FALSE)
# -----------------------------------------------------------



