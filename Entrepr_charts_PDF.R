#######################################################
# R functions to generate charts and tables in Entrepreneurship report
#
# asanchezrodelgo@ifc.org - Jun 2016
#######################################################
# Each R code chunk LaTeX will read is delimited by: ## ---- label ----

## ---- parameters ----
couName <- countryNames[countryNames$Country==c,]$Country
couISO2 <- .getISO2(couName)

########## Header ##########

## ---- figure_sparkline1 ----
figure_sparkline(couName,"figure1")

## ---- figure_sparkline2 ----
figure_sparkline(couName, "figure2")

## ---- figure_sparkline3 ----
figure_sparkline(couName, "figure3")

## ---- figure_sparkline4 ----
figure_sparkline(couName, "figure4")

## ---- figure_sparkline5 ----
figure_sparkline(couName, "figure5")

## ---- figure_sparkline6 ----
figure_sparkline(couName, "figure6")


########## Policy ##########

## ---- line_chart_Policy ----
line_chart(couName,"Policy","figure5")

## ---- table_time_avg_Policy ----
table_time_avg(couName,"Policy","table1")

## ---- sparklines_Policy ----
sparklines(couName,"Policy","table1")

## ---- table_time_Policy ----
table_time(couName,"Policy","table1")

## ---- doing_business_table ----
doing_business_table(couName)


########## Human capital ##########

########## Finance ##########

########## Markets ##########

## ---- radar_chart_Markets ----
radar_chart(couName, "Markets","radar1")
## ---- table_region_Markets ----
table_region(couName, "Markets","table1")
## ---- combo_percent_Markets1 ----
combo_percent(couName, "Markets","combo1")
## ---- combo_percent_Markets2 ----
combo_percent(couName, "Markets","combo2")

