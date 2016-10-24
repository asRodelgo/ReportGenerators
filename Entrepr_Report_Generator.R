# Run this script to generate all the country PDF reports and load them into the 
# read folder of the shinyTCMN app
##################################
# setwd() to handle images and other files
setwd('/Users/asanchez3/Desktop/Work/TCMN/ReportGenerators/')
source('Entrepr_generic_functions.R') # data and functions needed
source('helper_functions.R') # data and functions needed
# Create the data reports --------------------------------------
#for (c in c("Sint Maarten (Dutch part)")){
#for (c in c("Bosnia and Herzegovina")) {
for (c in countryNames$Country) {
  if (!(substr(c,1,1)=="(") & !(countryNames$CountryCodeISO3=="")){
    iso3 <- .getCountryCode(c)
    knit2pdf('Entrepr_PDF_Local.Rnw', clean = TRUE,
             encoding = "UTF-8",
             output = paste0("Entrepr_",iso3,".tex"))
    # copy file to pdf directory
    file.copy(paste0("Entrepr_",iso3,".pdf"), "/Users/asanchez3/Desktop/Work/TCMN/ReportGenerators/Entrepr_PDF/",overwrite=TRUE)
    file.remove(paste0("/Users/asanchez3/Desktop/Work/TCMN/ReportGenerators/","Entrepr_",iso3,".pdf"))
    file.remove(paste0("/Users/asanchez3/Desktop/Work/TCMN/ReportGenerators/","Entrepr_",iso3,".tex"))
  }
}
