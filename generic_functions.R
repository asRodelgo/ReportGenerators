# load global packages ----------------------------------------------
library(plyr) # manipulate data 
library(dplyr) # manipulate data 
library(ggplot2) # charts
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
# Operations date range global values ----------
fromDate <- "2013-07-01"
toDate <- "2020-06-30"
# read data
# PDF Offline Report generator --------------------------
# Read data
TCMN_data <- read.csv("/Users/asanchez3/shinyTCMN/data/TCMN_data.csv", colClasses = c(rep("character",4),rep("numeric",2),rep("character",2)))
# country table ----------------------------
countries <- read.csv("/Users/asanchez3/shinyTCMN/data/CountryClassification.csv", stringsAsFactors = FALSE)
# list of only countries (useful for selectors and others)
countryNames <- filter(countries, !(CountryCodeISO2==""))
countryNames <- select(countryNames, CountryCodeISO3, Country)# remove CountryISO2
# indicator table ----------------------------
indicators <- read.csv("/Users/asanchez3/shinyTCMN/data/IndicatorClassification.csv", stringsAsFactors = FALSE)
# TCMN specific source ----------------------------
TCMN_sources <- read.csv("/Users/asanchez3/shinyTCMN/data/TCMN_sources.csv", stringsAsFactors = FALSE)
# TCMN specific indicators ----------------------------
TCMN_indic <- read.csv("/Users/asanchez3/shinyTCMN/data/TCMN_Indicators.csv", stringsAsFactors = FALSE)
# WITS Imports ----------------------------
mWits <- read.csv("/Users/asanchez3/shinyTCMN/data/mWits.csv", colClasses = c(rep("character",3),rep("numeric",2),rep("character",2)))
# WITS Exports ----------------------------
xWits <- read.csv("/Users/asanchez3/shinyTCMN/data/xWits.csv", colClasses = c(rep("character",3),rep("numeric",2),rep("character",2)))
# IBRD T&C projects portfolio --------------
TCprojects <- read.csv("/Users/asanchez3/shinyTCMN/data/TCprojects.csv", stringsAsFactors = FALSE)
# IFC projects portfolio --------------
IFCprojects <- read.csv("/Users/asanchez3/shinyTCMN/data/IFCprojects.csv", stringsAsFactors = FALSE)

#
#
# general purpose helper functions ----------------------------------------------------

#####
##### Auxiliary functions
#####

.getISO2 <- function(couName){
  
  #countryISO2 <- tolower(as.character(countries[countries$CountryCodeISO3==couName,]$CountryCodeISO2))
  countryISO2 <- tolower(as.character(filter(countries,Country==couName)$CountryCodeISO2))
  #   if (length(countryISO2)==1){
  #     return(countryISO2)
  #   } else{
  #     return(0)
  #   }
  #return(countryISO2)
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
  #return(countryCode)
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

# Used in PDF report generation ------------------------
.getImportsPeriod <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(mWits, CountryCode == cou) #select country, region and world
  return(max(data$Period))
}

.getExportsPeriod <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(xWits, CountryCode == cou) #select country, region and world
  return(max(data$Period))
}

.generatePDFReports <- function(couNameList){
  
  #for (c in countryNames$Country) {
  for (c in couNameList) {
    
    #knit2pdf('reporting/TCMN_PDF_Local.Rnw', clean = TRUE, 
    #       output = paste0("reporting/TCMN_",c,".pdf"))
    print(paste("Report generated successfully for",c))
  }
  
}
# filter IBRD T&C relevant projects ---------------
# filter IBRD T&C relevant projects ---------------
.filterTCProjects <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  dataTC <- filter(TCprojects, tolower(WBG_CNTRY_KEY)==couISO2) #select country
  # calculate total amount per project
  dataTC <- dataTC %>%
    group_by(PROJ_ID) %>%
    mutate(Project_Amount = IBRD_CMT_USD_AMT + GRANT_USD_AMT + IDA_CMT_USD_AMT,
           Prod_Line = ifelse(tolower(substr(PROD_LINE_TYPE_NME,1,4))=="lend","Financing",
                              ifelse(tolower(substr(PROD_LINE_TYPE_NME,1,3))=="aaa",
                                     "Advisory Services and Analytics (ASA) IBRD",PROD_LINE_TYPE_NME)),
           ProjectOrder = ifelse(PROJECT_STATUS_NME=="Active",1,ifelse(PROJECT_STATUS_NME=="Pipeline",2,3)),
           url = paste0("http://operationsportal2.worldbank.org/wb/opsportal/ttw/about?projId=",PROJ_ID)) %>%
    select(-IBRD_CMT_USD_AMT, -GRANT_USD_AMT, -IDA_CMT_USD_AMT) %>%
    filter(PROJECT_STATUS_NME %in% c("Closed","Active","Pipeline")) #%>%
  #filter(!(tolower(substr(Prod_Line,1,8))=="standard"))
  
  return(dataTC)
}

# filter IFC T&C relevant projects ---------------
.filterIFCProjects <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  dataIFC <- filter(IFCprojects, COUNTRY_CODE==cou) #select country
  # projects in active, pipeline or closed status
  dataIFC <- filter(dataIFC, (PROJECT_STAGE %in% c("PIPELINE","PORTFOLIO")) | (PROJECT_STATUS %in% c("ACTIVE", "HOLD", "CLOSED")),
                    PROJECT_TYPE == "AS PROJECTS WITH CLIENT(S)")
  dataIFC <- mutate(dataIFC, Prod_Line = "Advisory Services and Analytics (ASA) IFC",
                    Project_Status = ifelse(PROJECT_STAGE=="PIPELINE","Pipeline",ifelse(PROJECT_STATUS=="CLOSED","Closed","Active")))
  dataIFC <- mutate(dataIFC, ProjectOrder = ifelse(Project_Status=="Active",1,ifelse(Project_Status=="Pipeline",2,3)),
                    url = paste0("http://ifcext.ifc.org/ifcext/spiwebsite1.nsf/%20AllDocsAdvisory?SearchView&Query=(FIELD ProjectId=",PROJ_ID))
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  
  return(dataIFC)
}
# Prepare sectors data ------
.projectsSectors <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, 
                   MAJORSECTOR_NAME1, SECTOR_PCT1, MAJORSECTOR_NAME2, SECTOR_PCT2, 
                   MAJORSECTOR_NAME3, SECTOR_PCT3, MAJORSECTOR_NAME4, SECTOR_PCT4,
                   MAJORSECTOR_NAME5, SECTOR_PCT5)
  # calculate total percentage per sector
  # first, put them in the same column
  dataTC2 <- gather(dataTC, sectorOrder, sectorName, -PROJ_ID,-Prod_Line,-contains("PCT"))
  dataTC2 <- gather(dataTC2, sectorPctOrder, sectorPct, -PROJ_ID,-Prod_Line,-sectorOrder,-sectorName)
  
  dataTC2 <- dataTC2 %>%
    group_by(sectorName) %>%
    mutate(sectorPctTotal = sum(sectorPct,na.rm=TRUE))
  
  # remove duplicates
  dataTC2 <- select(dataTC2, sectorName,sectorPctTotal)
  sectors <- as.data.frame(dataTC2[!duplicated(dataTC2),])
  
  # aggregate sectors
  sectors <- sectors[!duplicated(sectors),]
  sectors <- sectors %>%
    filter(!is.na(sectorName)) %>%
    mutate(sectorPct = sectorPctTotal/sum(sectorPctTotal)) %>%
    arrange(desc(sectorPct))
  
  return(sectors)
  
}

#############
# Prepare themes data ------
.projectsThemes <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, 
                   MAJORTHEME_NAME1, THEME_PCT1, MAJORTHEME_NAME2, THEME_PCT2, 
                   MAJORTHEME_NAME3, THEME_PCT3, MAJORTHEME_NAME4, THEME_PCT4,
                   MAJORTHEME_NAME5, THEME_PCT5)
  # calculate total percentage per sector
  # first, put them in the same column
  dataTC2 <- gather(dataTC, themeOrder, themeName, -PROJ_ID,-Prod_Line,-contains("PCT"))
  dataTC2 <- gather(dataTC2, themePctOrder, themePct, -PROJ_ID,-Prod_Line,-themeOrder,-themeName)
  
  dataTC2 <- dataTC2 %>%
    group_by(themeName) %>%
    mutate(themePctTotal = sum(themePct,na.rm=TRUE))
  
  # remove duplicates
  dataTC2 <- select(dataTC2, themeName,themePctTotal)
  themes <- as.data.frame(dataTC2[!duplicated(dataTC2),])
  
  # aggregate themes
  themes <- themes[!duplicated(themes),]
  themes <- themes %>%
    filter(!is.na(themeName)) %>%
    mutate(themePct = themePctTotal/sum(themePctTotal)) %>%
    arrange(desc(themePct))
  
  return(themes)
  
}


