# Run this script to generate all the country PDF reports and load them into the 
# read folder of the shinyTCMN app
##################################
# setwd() to handle images and other files
setwd('/Users/asanchez3/Desktop/Work/TCMN/ReportGenerators/')
source('generic_functions.R') # data and functions needed
# Create the data reports --------------------------------------
#for (c in c("Andorra")) {
for (c in countryNames$Country) {
    iso3 <- .getCountryCode(c)
    knit2pdf('TCMN_PDF_Local.Rnw', clean = TRUE,
             encoding = "UTF-8",
           output = paste0("TCMN_",iso3,".tex"))
    # copy file to shinyTCMN pdf directory
    file.copy(paste0("TCMN_",iso3,".pdf"), "/Users/asanchez3/shinyTCMN/pdf/",overwrite=TRUE)
}
  
# Create the operations reports --------------------------------------
for (c in countryNames$Country) {
#for (c in c("Brazil")) {
  iso3 <- .getCountryCode(c)
  knit2pdf('TCMN_Operations_PDF_Local.Rnw', clean = TRUE,
           encoding = "UTF-8",
           output = paste0("TCMN_Operations_",iso3,".tex"))
  # copy file to shinyTCMN pdf directory
  file.copy(paste0("TCMN_Operations_",iso3,".pdf"), "/Users/asanchez3/shinyTCMN/pdf/",overwrite=TRUE)
}

# Create the combined reports --------------------------------------
for (c in countryNames$Country) {
#for (c in c("Brazil","Bolivia", "Angola")) {
  iso3 <- .getCountryCode(c)
  knit2pdf('TCMN_Full_PDF_Local.Rnw', clean = TRUE,
           encoding = "UTF-8",
           output = paste0("TCMN_Full_",iso3,".tex"))
  # copy file to shinyTCMN pdf directory
  file.copy(paste0("TCMN_Full_",iso3,".pdf"), "/Users/asanchez3/shinyTCMN/pdf/",overwrite=TRUE)
}

# Create the Combined Region Departments reports --------------------------------------
for (couDep in unique(countryDeps$CMU)) {
#for (couDep in c("AFCS1")) {
  knit2pdf('TCMN_RegionDeps_PDF_Local.Rnw', clean = TRUE,
           encoding = "UTF-8",
           output = paste0("TCMN_RegionDeps_",couDep,".tex"))
  # copy file to shinyTCMN pdf directory
  file.copy(paste0("TCMN_RegionDeps_",couDep,".pdf"), "/Users/asanchez3/shinyTCMN/pdf/",overwrite=TRUE)
}

# Create the RMarkdown operations reports --------------------------------------
# for (c in countryNames$Country) {
# #for (c in c("Brazil")) {
#   iso3 <- .getCountryCode(c)
#   knit2html('TCMN_Operations_TEST.Rmd',
#            output = paste0("TCMN_Operations_TEST_",iso3,".html"))
#   # copy file to shinyTCMN pdf directory
#   #file.copy(paste0("TCMN_Operations_",iso3,".pdf"), "/Users/asanchez3/shinyTCMN/pdf/",overwrite=TRUE)
# }
