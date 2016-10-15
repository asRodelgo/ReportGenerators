# load global packages ----------------------------------------------
library(plyr) # manipulate data 
library(dplyr) # manipulate data 
library(ggplot2) # charts
library(gridExtra) # ggplot charts side by side
library(data.table) # fast operations
library(tidyr) # transform data
library(xtable) # LaTeX tables
library(fmsb) # radar charts
require(treemap) # treemap charts
library(DT) # customize dataTable javascript library
library(reshape2) # manipulate data
library(devtools) # allow install packages from source
#install_github('htmlwidgets/sparkline') # install sparklines
library(sparkline) # sparklines
library(knitr) # generate LaTeX PDF report

# avoid scientific notation
options(scipen=999)
thisYear <- substr(Sys.Date(),1,4)
# PDF Offline Report generator --------------------------
# Read data
load("Entrepr_DataByCategory.rda")
#load("datasets by dimension_new.rda")
dataDesc <- read.csv("Entrepr_DataDescription.csv", stringsAsFactors = FALSE)
# country table ----------------------------
countries <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/CountryClassification.csv", stringsAsFactors = FALSE)
countries[countries$CountryCodeISO3=="NAM",]$CountryCodeISO2 <- "NA"
# list of only countries (useful for selectors and others)
countryNames <- filter(countries, !(CountryCodeISO2==""))
countryNames <- select(countryNames, CountryCodeISO3, Country)# remove CountryISO2
# transform data for easier processing
Entrepr_data <- data.frame()
for (i in 1:length(datasets_by_dimension)){
  for (j in 1:length(datasets_by_dimension[[i]])){
    thisDataFrame <- as.data.frame(sapply(datasets_by_dimension[[i]][[j]], as.character), stringsAsFactors = FALSE)
    thisGather <- gather(thisDataFrame, Key, Observation, -one_of("iso2c","country","Country", "year", "iso3c", "Year"))
    thisGather$Observation <- as.numeric(thisGather$Observation)
    thisGather <- mutate(thisGather, Category = names(datasets_by_dimension)[i], Subcategory = names(datasets_by_dimension[[i]])[j])
    if (nrow(Entrepr_data)>0) {
      Entrepr_data <- bind_rows(Entrepr_data, thisGather)
    } else {
      Entrepr_data <- thisGather
    }
  }
}
# Add descriptors and source fields
Entrepr_data <- merge(Entrepr_data, dataDesc[,c("var","varname","Source_Link","Unit.of.Measure","Section","Subsection","Subsection2")], by.x="Key", by.y = "var", all.x = TRUE)
Entrepr_data <- merge(Entrepr_data, countries[,c("CountryCodeISO3","CountryCodeISO2")],by.x="iso2c",by.y="CountryCodeISO2",all.x = TRUE)
# clean up: remove duplicate columns
Entrepr_data <- Entrepr_data %>%
  mutate(Year = ifelse(is.na(Year),year,Year),
                       Country = ifelse(is.na(Country),country,Country)) %>%
  select(Key, Category, Subcategory, Observation, CountryCode = CountryCodeISO3, iso2c, Period = Year, 
         IndicatorShort = varname, Source = Source_Link, Unit = Unit.of.Measure, Section, Subsection, Subsection2)

# might need TCMN data for some charts/tables
TCMN_data <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/TCMN_data.csv", colClasses = c(rep("character",4),rep("numeric",2),rep("character",2)))
indicators <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/IndicatorClassification.csv", stringsAsFactors = FALSE)
TCMN_sources <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/TCMN_sources.csv", stringsAsFactors = FALSE)
TCMN_indic <- read.csv("/Users/asanchez3/shinyTCMN_Original/data/TCMN_Indicators.csv", stringsAsFactors = FALSE)

# -------------------------------------------------------------
##### Auxiliary functions -------------------------------------

.getISO2 <- function(couName){
  
  countryISO2 <- tolower(as.character(filter(countries,Country==couName)$CountryCodeISO2))
}

.getRegion <- function(couName){
  
  cou <- .getCountryCode(couName)
  region <- as.character(countries[countries$CountryCodeISO3==cou,]$RegionShort) 
}

.getCountryCode <- function(couName){
  
  countryCode <- filter(countries, Country==couName)$CountryCodeISO3
  if (length(countryCode)==1){
    return(countryCode)
  } else{
    return(0)
  }
}

# country flags -----------------------------------
.outFlag <- function(couName){
  
  iso <- .getISO2(couName)  
  if (paste0(iso,".png")==".png"){
    
    tags$img(src="world.png", width="40%")  
    
  } else{
    
    tags$img(src=paste0(iso,".png"), width="40%")  
  } 
  
}

.generatePDFReports <- function(couNameList){
  
  for (c in couNameList) {
    print(paste("Report generated successfully for",c))
  }
  
}
