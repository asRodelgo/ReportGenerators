#######################################################
# R functions to generate charts and tables in TCMN report
#
# asanchezrodelgo@ifc.org - Dec 2015
#######################################################
# Each R code chunk LaTeX will read is delimited by: ## ---- label ----

## ---- loaddata ----
#country_loop <- c("Tunisia","Spain")

## ---- parameters ----
#source(file.path("helper_functions","generic_functions.R"),local=TRUE)
# init_Params <- function(){
#   #library(knitr)
couName <- countryNames[countryNames$Country==c,]$Country
couISO2 <- .getISO2(couName)
#cou <- ""
#   #couISO2 <- .getISO2(input$inCouSel)
#   #cou <- .getCountryCode(input$inCouSel)
#   couISO2 <- .getISO2(couName)
#   cou <- .getCountryCode(couName)
#   impYear <- .getImportsPeriod(cou)
#   expYear <- .getExportsPeriod(cou)
#   month <- substr(Sys.Date(),6,7)
#   year <- substr(Sys.Date(),1,4)
# }
#init_Params()

## ---- ExpImp_HF ----
ExpImp_HF <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="chart1")
  data <- data[!(is.na(data$Observation)),]
  if (nrow(data)>0){
    data <- arrange(data,Period)
    ggplot(data, aes(x=Period, y=Observation, group=IndicatorShort)) +
      geom_line(aes(linetype=IndicatorShort,colour=IndicatorShort),size=1.5,stat="identity") +
      scale_linetype_manual(values = c(1,2))+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.text.x = element_text(angle = 90, hjust = 1)) + 
      labs(x="",y=""#,title="Goods Export and Import volume growth, 2012-2015"
           ) + 
      scale_x_discrete(breaks = unique(data$Period)[seq(1,length(unique(data$Period)),3)]) 
  
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }
  
  
}
ExpImp_HF(couName)

## ---- top5constraintsES ----
top5constraintsES <- function(couName){      
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="chart3")
  
  if (nrow(data)>0){
    # compute top 5 constraints
    data <- head(arrange(data, desc(Observation)),5)
    # order the factors
    data$IndicatorShort = factor(as.character(data$IndicatorShort), 
                          levels = data$IndicatorShort[order(data$Observation)])
    
    
    ggplot(data, aes(x=factor(IndicatorShort), y=Observation)) +
      geom_bar(fill="blue",stat="identity") +
      geom_text(aes(label=Observation,y=Observation + max(Observation)*.06),
                size=6) + 
      coord_flip()+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 15)) + 
            labs(x="",y=""#,title="Top 5 constraints according to 2013 Enterprise Survey (in percent)"
                 )
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }
          
}
top5constraintsES(couName)

## ---- top5constraintsWEF ----
top5constraintsWEF <- function(couName){      
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="chart4")
  
  if (nrow(data)>0){
    # compute top 5 constraints
    data <- head(arrange(data, desc(Observation)),5)
    # order the factors
    data$IndicatorShort = factor(as.character(data$IndicatorShort), 
                                 levels = data$IndicatorShort[order(data$Observation)])
    
    
    ggplot(data, aes(x=factor(IndicatorShort), y=Observation)) +
      geom_bar(fill="green",stat="identity") +
      geom_text(aes(label=Observation,y=Observation + max(Observation)*.06),
                size=6) + 
      coord_flip()+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 15)) + 
      labs(x="",y=""#,title="Top 5 constraints according to 2013 Enterprise Survey (in percent)"
      )
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }
  
}
top5constraintsWEF(couName)

## ---- WGIindicators ----
WGIindicators <- function(couName){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  
  couRegion <- countries[countries$CountryCodeISO3==cou,]$RegionCodeByIncome  # obtain the region for the selected country
  neighbors <- countries[countries$RegionCodeByIncome==couRegion,]$CountryCodeISO3 # retrieve all countries in that region
  neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
  
  # hardcode for now
  #neighbors <- c("DZA","JOR","MAR","EGY","TUN")
  
  data <- filter(TCMN_data, CountryCode %in% c(cou,neighbors), Subsection=="chart6")
  
  if (nrow(filter(data, CountryCode==cou))>0){
    
    data <- merge(data, countries[,c("Country","CountryCodeISO3")], by.x="CountryCode", by.y="CountryCodeISO3") # add country name
    data <- filter(data, Period == max(Period))
    # select top 4 countries from the neighborhood based on their income level
    income <- filter(TCMN_data, CountryCode %in% neighbors, Subsection=="table2head", Key=="M03")
    income <- income %>%
      group_by(CountryCode) %>%
      filter(Period < thisYear) %>%
      filter(Period == max(Period))
    
    topNeighbors <- head(arrange(as.data.frame(income), desc(Observation)),4)$CountryCode
    data <- filter(data, CountryCode %in% c(cou,topNeighbors))
    
    # order the factors
    data$Country = factor(as.character(data$Country), 
                                 levels = c(unique(as.character(data[data$CountryCode==cou,]$Country)), 
                                            as.character(unique(data[data$CountryCode %in% topNeighbors,]$Country))))
    
    ggplot(data, aes(x=Country,y=Observation,fill=Country)) +
      geom_bar(position="dodge",stat="identity") +
      facet_wrap(~IndicatorShort) +
      #coord_flip()+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) + 
      labs(x="",y="")+#,title="World Governance Indicators")+
      scale_fill_manual(values = c("darkblue", "lightblue", "orange", "yellow","lightgreen"))

  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }
  
}
WGIindicators(couName)

## ---- LPIindicators ----
LPIindicators <- function(couName){      
  
  cou <- .getCountryCode(couName)
  
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="chart8")

  if (nrow(data)>0){  
    # two last periods to plot
    maxPeriod <- max(data$Period)
    secMaxPeriod <- max(data[!(data$Period==maxPeriod),]$Period)
    
    data <- filter(data, Period %in% c(maxPeriod,secMaxPeriod))
    
    ggplot(data, aes(x=IndicatorShort,y=Observation,fill=factor(Period))) +
      geom_bar(position="dodge",stat="identity") +
      coord_flip()+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.position="top",
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.text.y = element_text(size=15)#, axis.text.x = element_blank()
            ) + 
      labs(x="",y="")+#,title="Logistics Performance Index (1-5)"
      scale_fill_manual(values = c("lightblue", "darkblue"),guide = guide_legend(reverse=TRUE))
  
} else {
  plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  graphics::text(1.5, 1,"Data not available", col="red", cex=2)
}
  
}
LPIindicators(couName)

## ---- WEFradar ----
WEFradar <- function(couName){      
  
  cou <- .getCountryCode(couName) # This chart needs to query neighbouring countries also
  
  couRegion <- countries[countries$CountryCodeISO3==cou,]$RegionCodeByIncome  # obtain the region for the selected country
  neighbors <- countries[countries$RegionCodeByIncome==couRegion,]$CountryCodeISO3 # retrieve all countries in that region
  neighbors <- as.character(neighbors[!(neighbors==cou)]) # exclude the selected country
  
  # country and Region descriptors
  country <- as.character(countries[countries$CountryCodeISO3==cou,]$Country)
  region <- as.character(countries[countries$CountryCodeISO3==cou,]$RegionShortIncome) 
  
  # filter the data
  data <- filter(TCMN_data, CountryCode %in% c(cou,neighbors), Subsection=="chart7")
  
  if (nrow(filter(data, CountryCode==cou))>0){  
    # calculate the average for the region
    data <- data %>%
      group_by(Key) %>%
      mutate(regionAvg = mean(Observation, na.rm=TRUE))
    
    # remove all countries except cou
    data <- filter(data, CountryCode==cou)
    
    # Keep last period
    data <- filter(data, Period == max(Period))
    
    # as.numeric
    #countries <- names(data[,3:ncol(data)])
    #data <- data %>% mutate_each_(funs(as.character), countries) %>% mutate_each_(funs(as.numeric), countries)
    
    # I must add the max and min columns to make it work:
    max<-7
    min <-1
    data <- cbind(data,max,min)
    
    # order labels ad-hoc:
    order <- c(8,10,6,4,7,3,1,9,5,2,11,12)
    data <- cbind(data,order)
    data <- arrange(data,order)
    data <- select(data, -order)# drop order
    
    # final tweaking
    data <- select(data, IndicatorShort, max, min, Observation, regionAvg)
    
    # transpose the data for radarchart to read
    dataTrans <- as.data.frame(t(data[,2:ncol(data)]))
    
    radarchart(dataTrans, axistype=1, caxislabels=seq(from=1,to=max,by=1),
               plty=c(1,2),plwd=c(6,3),pcol=c("darkblue","red"),pdensity=c(0, 0),
               cglwd=2,axislabcol="navy", vlabels=data$IndicatorShort, cex.main=1,cex=2.5)
    #title="WEF Competitiveness Indicators, stage of development (1-7)",
    legend(-2.1,-0.8, legend=c(country,region), seg.len=0.5, pch=3, 
           bty="n" ,lwd=3, y.intersp=1.5, horiz=FALSE, col=c("darkblue","red"))
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }  
  
}
WEFradar(couName)

## ---- GVA_Treemap ----
GVA_Treemap <- function(couName){      
  
  cou <- .getCountryCode(couName)
  
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="chart2")
  if (nrow(filter(data, CountryCode==cou))>0){
    data <- filter(data, Period==max(Period))
    data <- select(data, IndicatorShort, Observation)
    data <- rbind(data, c("Other",0)) # add "Other" category
    data <- filter(data, !(IndicatorShort=="Industry")) # remove "industry" category
    data$Observation <- round(as.numeric(data$Observation),2)
    data$color <- rainbow(length(data$IndicatorShort)) # add the color
    data[data$IndicatorShort=="Other",]$Observation <- 100 - sum(data$Observation)
    
    # format numbers
    data$Observation <- format(data$Observation, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
    data$Observation <- as.numeric(data$Observation)
    
    treemap(data,
            index=c("IndicatorShort","Observation"),
            vSize="Observation",
            fontsize.labels=c(24, 24), 
            align.labels=list(c("left", "top"), c("right","bottom")),
            lowerbound.cex.labels=0.5,
            vColor="color",
            type="color",
            title="")
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }  
  
}
GVA_Treemap(couName)

## ---- ImpExp_Treemap-1 ----
ImpExp_Treemap <- function(couName, type){
  
  cou <- .getCountryCode(couName)
  if (type=="m"){
    
    data <- filter(mWits, CountryCode == cou) #select country, region and world
  } else {
    
    data <- filter(xWits, CountryCode == cou) #select country, region and world
  }
  
  if (nrow(filter(data, CountryCode==cou))>0){
    # prepare for table
    data <- select(data, ProductDescription, ProductCode, Period, TradeValue)
    # keep the latest period
    data <- filter(data, Period==max(Period))
    # compute the percentage of total value
    data <- mutate(data, percTotalValue = 100*TradeValue/sum(TradeValue, na.rm = TRUE))
    data$percTotalValue <- round(as.numeric(data$percTotalValue),2)
    
    data$color <- terrain.colors(length(data$ProductCode)) # add the color
    if (type=="x"){
      data$color <- rainbow(length(data$ProductCode)) # add the color
    }
    # format numbers
    # format numbers
    data$TradeValue <- format(data$TradeValue, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
    data$percTotalValue <- format(data$percTotalValue, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    data$percTotalValue <- as.numeric(data$percTotalValue)
    
    
    data <- select(data, -Period)
    
    if (type=="x"){
      data <- head(arrange(as.data.frame(data), desc(TradeValue)),5)
    }
    
    treemap(data,
            index=c("ProductDescription","percTotalValue"),
            vSize="percTotalValue",
            fontsize.labels=c(24, 24), 
            align.labels=list(c("left", "top"), c("right","bottom")),
            lowerbound.cex.labels=0.5,
            vColor="color",
            type="color",
            title="")
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }  

}
ImpExp_Treemap(couName,"m")

## ---- ImpExp_Treemap-2 ----
ImpExp_Treemap <- function(couName, type){
  
  cou <- .getCountryCode(couName)
  if (type=="m"){
    
    data <- filter(mWits, CountryCode == cou) #select country, region and world
  } else {
    
    data <- filter(xWits, CountryCode == cou) #select country, region and world
  }
  
  if (nrow(filter(data, CountryCode==cou))>0){
    # prepare for table
    data <- select(data, ProductDescription, ProductCode, Period, TradeValue)
    # keep the latest period
    data <- filter(data, Period==max(Period))
    # compute the percentage of total value
    data <- mutate(data, percTotalValue = 100*TradeValue/sum(TradeValue, na.rm = TRUE))
    data$percTotalValue <- round(as.numeric(data$percTotalValue),2)
    
    data$color <- terrain.colors(length(data$ProductCode)) # add the color
    if (type=="x"){
      data$color <- rainbow(length(data$ProductCode)) # add the color
    }
    # format numbers
    data$TradeValue <- format(data$TradeValue, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
    data$percTotalValue <- format(data$percTotalValue, digits=0, decimal.mark=".",
                                  big.mark=",",small.mark=".", small.interval=3)
    data$percTotalValue <- as.numeric(data$percTotalValue)
    
    data <- select(data, -Period)
    
    if (type=="x"){
      data <- head(arrange(data, desc(TradeValue)),5)
    }
    
    treemap(data,
            index=c("ProductDescription","percTotalValue"),
            vSize="percTotalValue",
            fontsize.labels=c(24, 24), 
            align.labels=list(c("left", "top"), c("right","bottom")),
            lowerbound.cex.labels=0.5,
            vColor="color",
            type="color",
            title="")
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }  
  
}
ImpExp_Treemap(couName,"x")

## ---- macroInd ----
macroInd <- function(couName){      
  
  cou <- .getCountryCode(couName)
  
  tableKeys <- unique(filter(TCMN_data, Subsection=="table2head")[,c("Key","IndicatorShort")])
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="table2head")
  if (nrow(data)>0){
    #data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
    # keep the latest period (excluding projections further than 2 years)
    data <- filter(data, Period <= (as.numeric(thisYear) + 1))
    
    data <- data %>%
      group_by(Key) %>%
      filter(Period == max(Period))
    # add Period to Indicator name
    data$IndicatorShort <- paste(data$IndicatorShort, " (",data$Period,")", sep="")
    # Scale Observations
    data <- mutate(data, ObsScaled = Scale*Observation)
    # format numbers
    data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    
    data <- arrange(data, Key)
    data <- data[,c("IndicatorShort", "ObsScaled")] # short indicator name and scaled data
    data <- as.data.frame(t(data)) # transpose the data
    # I have to add a dummy column so the alignment works (align)
    for (j in (ncol(data)+1):6){
      data[,j] <- ""
      names(data)[j] <- ""
    }
    data$dummy <- rep("",nrow(data))
    
    #bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
    
    data.table <- xtable(data)
    align(data.table) <- c('l',rep('>{\\centering}p{1.5in}',ncol(data.table)-1),'l')
    print(data.table, include.rownames=FALSE,include.colnames=FALSE, floating=FALSE, 
          size="\\LARGE", #sanitize.text.function=bold,
          booktabs = FALSE, table.placement="", hline.after = NULL ,latex.environments = "center")
  }
  
}
macroInd(couName)

## ---- macroInd_Big ----
macroInd_Big <- function(couName){      
  
  cou <- .getCountryCode(couName)

  data <- filter(TCMN_data, CountryCode==cou, Subsection=="table2")
  # keep the latest period (excluding projections further than 2 years)
  data <- filter(data, Period <= (as.numeric(thisYear) + 1))
  # remove NAs rows
  data <- filter(data, !is.na(Observation))
  # calculate average for 1st column
  data_avg <- data %>%
      group_by(Key) %>%
      filter(Period < (as.numeric(thisYear)-3)) %>%
      mutate(historical_avg = mean(Observation))
  # add average as one of the time periods
  data_avg <- mutate(data_avg, Period = paste("Avg ",as.numeric(thisYear)-13,"-",as.numeric(thisYear)-4,sep=""),
                               Observation = historical_avg, ObsScaled = Scale*historical_avg)
  
  data_avg <- data_avg[!duplicated(data_avg$Key),] # remove duplicates
  data_avg <- select(data_avg, -historical_avg, -ObsScaled) # remove some variables
  
  #keep only periods of interest in data
  data <- filter(data, Period > (as.numeric(thisYear) - 4))
  data <- rbind(data, data_avg) # add rows to data
  # Scale Observations
  data <- mutate(data, ObsScaled = Scale*Observation)
  data <- arrange(data, Key)
  data <- select(data, Key, IndicatorShort, Period, ObsScaled)
  # restrict to 2 decimal places
  data$ObsScaled <- round(data$ObsScaled,2)
  # format numbers
  data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  
  for (i in 1:nrow(data)){
  
    data$IndicatorShort[i] <- ifelse(!is.na(merge(data,TCMN_indic[TCMN_indic$Subsection=="table2",], by="Key")$Note[i]),
                                     paste0(data$IndicatorShort[i]," \\large{[", merge(data,TCMN_indic[TCMN_indic$Subsection=="table2",], by="Key")$Note[i],"]}"),
                                     data$IndicatorShort[i])  
  }
  # escape reserved characters
  data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
  
  # final table format
  data <- spread(data, Period, ObsScaled)
  data <- data[,-1] #drop the Key column
  data <- data[,c(1,ncol(data),2:(ncol(data)-1))] # reorder columns
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  # modify column names
  names(data) <- c("",names(data)[2:(ncol(data)-1)],"")
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"

  data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large",
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}
macroInd_Big(couName)

## ---- macroInd_Big_macro ----
macroInd_Split <- function(couName,table){      
  
  cou <- .getCountryCode(couName)
  
  tableKeys <- unique(filter(TCMN_data, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(TCMN_data, CountryCode==cou, Subsection==table)
  data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
  # keep the latest period (excluding projections further than 2 years)
  data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period))
  data <- filter(data, Period <= (as.numeric(thisYear) + 1))
  # remove NAs rows
  #data <- filter(data, !is.na(Observation))
  # calculate average for 1st column
  data_avg <- data %>%
    group_by(Key) %>%
    filter(Period < (as.numeric(thisYear)-3)) %>%
    mutate(historical_avg = mean(Observation,na.rm=TRUE))
  # add average as one of the time periods
  data_avg <- mutate(data_avg, Period = paste("Avg ",as.numeric(thisYear)-13,"-",as.numeric(thisYear)-4,sep=""),
                     Observation = historical_avg, ObsScaled = Scale*historical_avg)
  
  data_avg <- data_avg[!duplicated(data_avg$Key),] # remove duplicates
  data_avg <- select(data_avg, -historical_avg, -ObsScaled) # remove some variables
  
  #keep only periods of interest in data
  data <- filter(data, Period > (as.numeric(thisYear) - 4))
  data <- rbind(data, data_avg) # add rows to data
  # Scale Observations
  data <- mutate(data, ObsScaled = Scale*Observation)
  data <- arrange(data, Key)
  data <- select(data, Key, IndicatorShort, Period, ObsScaled)
  # restrict to 2 decimal places
  data$ObsScaled <- round(data$ObsScaled,2)
  
  # format numbers
  data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  
  for (i in 1:nrow(data)){
    
    data$IndicatorShort[i] <- ifelse(!is.na(merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i]),
                                     paste0(data$IndicatorShort[i]," \\large{[", merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i],"]}"),
                                     data$IndicatorShort[i])  
  }
  # escape reserved characters
  data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
  data$IndicatorShort <- gsub("&", "\\&", data$IndicatorShort, fixed=TRUE)
  
  # final table format
  data <- spread(data, Period, ObsScaled)
  data <- data[,-1] #drop the Key column
  if (ncol(data)>2){
    data <- data[,c(1,ncol(data),2:(ncol(data)-1))] # reorder columns
    # rid of characters in numeric columns
    data[,ncol(data)] <- gsub("NA", "---", data[,ncol(data)], fixed=TRUE)
  } 
  
  # dummy columns in to keep the pdf layout
  if (ncol(data)<=6){
    for (j in (ncol(data)+1):7){
      data[,j] <- "---"
      names(data)[j] <- as.character(as.numeric(thisYear)-6+j)
    }
  }
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  # modify column names
  names(data) <- c("",names(data)[2:(ncol(data)-1)],"")
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','>{\\raggedright}p{6in}','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large",
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}
macroInd_Split(couName,"table2macro")

## ---- macroInd_Big_invest ----
macroInd_Split <- function(couName,table){      
  
  cou <- .getCountryCode(couName)
  
  tableKeys <- unique(filter(TCMN_data, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(TCMN_data, CountryCode==cou, Subsection==table)
  data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
  # keep the latest period (excluding projections further than 2 years)
  data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period))
  data <- filter(data, Period <= (as.numeric(thisYear) + 1))
  # remove NAs rows
  #data <- filter(data, !is.na(Observation))
  # calculate average for 1st column
  data_avg <- data %>%
    group_by(Key) %>%
    filter(Period < (as.numeric(thisYear)-3)) %>%
    mutate(historical_avg = mean(Observation,na.rm=TRUE))
  # add average as one of the time periods
  data_avg <- mutate(data_avg, Period = paste("Avg ",as.numeric(thisYear)-13,"-",as.numeric(thisYear)-4,sep=""),
                     Observation = historical_avg, ObsScaled = Scale*historical_avg)
  
  data_avg <- data_avg[!duplicated(data_avg$Key),] # remove duplicates
  data_avg <- select(data_avg, -historical_avg, -ObsScaled) # remove some variables
  
  #keep only periods of interest in data
  data <- filter(data, Period > (as.numeric(thisYear) - 4))
  data <- rbind(data, data_avg) # add rows to data
  # Scale Observations
  data <- mutate(data, ObsScaled = Scale*Observation)
  data <- arrange(data, Key)
  data <- select(data, Key, IndicatorShort, Period, ObsScaled)
  # restrict to 2 decimal places
  data$ObsScaled <- round(data$ObsScaled,2)
  # format numbers
  data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  
  for (i in 1:nrow(data)){
    
    data$IndicatorShort[i] <- ifelse(!is.na(merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i]),
                                     paste0(data$IndicatorShort[i]," \\large{[", merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i],"]}"),
                                     data$IndicatorShort[i])  
  }
  # escape reserved characters
  data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
  data$IndicatorShort <- gsub("&", "\\&", data$IndicatorShort, fixed=TRUE)
  
  # final table format
  data <- spread(data, Period, ObsScaled)
  data <- data[,-1] #drop the Key column
  if (ncol(data)>2){
    data <- data[,c(1,ncol(data),2:(ncol(data)-1))] # reorder columns
    # rid of characters in numeric columns
    data[,ncol(data)] <- gsub("NA", "---", data[,ncol(data)], fixed=TRUE)
  } 
  
  # dummy columns in to keep the pdf layout
  if (ncol(data)<=6){
    for (j in (ncol(data)+1):7){
      data[,j] <- "---"
      names(data)[j] <- as.character(as.numeric(thisYear)-6+j)
    }
  }
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  # modify column names
  names(data) <- c("",names(data)[2:(ncol(data)-1)],"")
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','>{\\raggedright}p{6in}','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large",
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}
macroInd_Split(couName,"table2invest")

## ---- macroInd_Big_trade ----
macroInd_Split <- function(couName,table){      
  
  cou <- .getCountryCode(couName)
  
  tableKeys <- unique(filter(TCMN_data, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(TCMN_data, CountryCode==cou, Subsection==table)
  data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
  # keep the latest period (excluding projections further than 2 years)
  data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period))
  data <- filter(data, Period <= (as.numeric(thisYear) + 1))
  # remove NAs rows
  #data <- filter(data, !is.na(Observation))
  # calculate average for 1st column
  data_avg <- data %>%
    group_by(Key) %>%
    filter(Period < (as.numeric(thisYear)-3)) %>%
    mutate(historical_avg = mean(Observation,na.rm=TRUE))
  # add average as one of the time periods
  data_avg <- mutate(data_avg, Period = paste("Avg ",as.numeric(thisYear)-13,"-",as.numeric(thisYear)-4,sep=""),
                     Observation = historical_avg, ObsScaled = Scale*historical_avg)
  
  data_avg <- data_avg[!duplicated(data_avg$Key),] # remove duplicates
  data_avg <- select(data_avg, -historical_avg, -ObsScaled) # remove some variables
  
  #keep only periods of interest in data
  data <- filter(data, Period > (as.numeric(thisYear) - 4))
  data <- rbind(data, data_avg) # add rows to data
  # Scale Observations
  data <- mutate(data, ObsScaled = Scale*Observation)
  data <- arrange(data, Key)
  data <- select(data, Key, IndicatorShort, Period, ObsScaled)
  # restrict to 2 decimal places
  data$ObsScaled <- round(data$ObsScaled,2)
  # format numbers
  data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  
  for (i in 1:nrow(data)){
    
    data$IndicatorShort[i] <- ifelse(!is.na(merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i]),
                                     paste0(data$IndicatorShort[i]," \\large{[", merge(data,TCMN_indic[TCMN_indic$Subsection==table,], by="Key")$Note[i],"]}"),
                                     data$IndicatorShort[i])  
  }
  # escape reserved characters
  data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
  data$IndicatorShort <- gsub("&", "\\&", data$IndicatorShort, fixed=TRUE)
  
  # final table format
  data <- spread(data, Period, ObsScaled)
  data <- data[,-1] #drop the Key column
  if (ncol(data)>2){
    data <- data[,c(1,ncol(data),2:(ncol(data)-1))] # reorder columns
    # rid of characters in numeric columns
    data[,ncol(data)] <- gsub("NA", "---", data[,ncol(data)], fixed=TRUE)
  } 
  
  # dummy columns in to keep the pdf layout
  if (ncol(data)<=6){
    for (j in (ncol(data)+1):7){
      data[,j] <- "---"
      names(data)[j] <- as.character(as.numeric(thisYear)-6+j)
    }
  }
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  # modify column names
  names(data) <- c("",names(data)[2:(ncol(data)-1)],"")
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','>{\\raggedright}p{6in}','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large",
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}
macroInd_Split(couName,"table2trade")

## ---- createSparklines ----
createSparklines <- function(couName){      
  
  cou <- .getCountryCode(couName)
  ## Examples like Edward Tufte's sparklines:
  
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="table2")
  # keep the latest period (excluding projections further than 2 years)
  data <- filter(data, Period <= (as.numeric(thisYear) + 1), Period > (as.numeric(thisYear) - 14))
  data <- filter(data, !is.na(Observation)) # remove NAs rows
  # keep relevant columns
  data <- select(data, Key, Period, Observation)
  data <- arrange(data, Key, Period)
  
  x <- spread(data, Key, Observation)
  x <- x[,-1] # don't need Period column anymore
  
  # impute NAs and standardize so all sparklines are scales
  for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
    
    x[is.na(x[,i]),i] <- mean(x[,i],na.rm = TRUE)  #impute NAs to the mean of the column
  }
  x <- scale(x) # standardize x
  
  par(mfrow=c(ncol(x)+2,1), #sets number of rows in space to number of cols in data frame x
      mar=c(1,0,0,0), #sets margin size for the figures
      oma=c(1,2,1,1)) #sets outer margin
  
  for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
    
    plot(x[,i], #use col data, not rows from data frame x
         col="darkgrey",lwd=4, #color the line and adjust width
         axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
    
    axis(2,yaxp=c(min(x[,i],na.rm = TRUE),max(x[,i],na.rm = TRUE),2),col="white",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
    ymin<-min(x[,i],na.rm = TRUE); tmin<-which.min(x[,i]);ymax<-max(x[,i], na.rm = TRUE);tmax<-which.max(x[,i]); # 
    points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("red","green"),cex=5) # add coloured points at max and min
  }
}
createSparklines(couName)

## ---- createSparklines_macro ----
createSparklines_Split <- function(couName,table){      
  
  cou <- .getCountryCode(couName)
  ## Examples like Edward Tufte's sparklines:
  
  tableKeys <- unique(filter(TCMN_data, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(TCMN_data, CountryCode==cou, Subsection==table)
  
  if (nrow(data)>0){
    data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
    # keep the latest period (excluding projections further than 2 years)
    data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period))
    data <- filter(data, Period <= (as.numeric(thisYear) + 1), Period > (as.numeric(thisYear) - 14))
    #data <- filter(data, !is.na(Observation)) # remove NAs rows
    # keep relevant columns
    data <- select(data, Key, Period, Observation)
    data <- arrange(data, Key, Period)
    
    x <- spread(data, Key, Observation)
    x <- x[,-1] # don't need Period column anymore
    
    # impute NAs and standardize so all sparklines are scales
    for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
      
      x[is.na(x[,i]),i] <- mean(x[,i],na.rm = TRUE)  #impute NAs to the mean of the column
      if (sum(x[,i],na.rm = TRUE)==0){ 
        x[,i] <- 0
        #x[1,i] <- -10
        x[nrow(x),i] <- 10
        }
    }
    #x <- scale(x) # standardize x
    
    par(mfrow=c(ncol(x)+2,1), #sets number of rows in space to number of cols in data frame x
        mar=c(1,0,0,0), #sets margin size for the figures
        oma=c(1,2,1,1)) #sets outer margin
    
    for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
      
      if (sum(x[1:(nrow(x)-1),i])==0){ # paint in white empty rows
        plot(x[,i], #use col data, not rows from data frame x
             col="white",lwd=4, #color the line and adjust width
             axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
        
        axis(2,yaxp=c(min(x[,i],na.rm = TRUE),max(x[,i],na.rm = TRUE),2),col="white",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
        ymin<-min(x[,i],na.rm = TRUE); tmin<-which.min(x[,i]);ymax<-max(x[,i], na.rm = TRUE);tmax<-which.max(x[,i]);
        points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("white","white"),cex=5) # add coloured points at max and min# 
      } else {
        plot(x[,i], #use col data, not rows from data frame x
             col="darkgrey",lwd=4, #color the line and adjust width
             axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
        
        axis(2,yaxp=c(min(x[,i],na.rm = TRUE),max(x[,i],na.rm = TRUE),2),col="white",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
        ymin<-min(x[,i],na.rm = TRUE); tmin<-which.min(x[,i]);ymax<-max(x[,i], na.rm = TRUE);tmax<-which.max(x[,i]); # 
        points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("red","green"),cex=5) # add coloured points at max and min
      }
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    #graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  } 
  
}
createSparklines_Split(couName,"table2macro")

## ---- createSparklines_invest ----
createSparklines_Split <- function(couName,table){      
  
  cou <- .getCountryCode(couName)
  ## Examples like Edward Tufte's sparklines:
  
  tableKeys <- unique(filter(TCMN_data, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(TCMN_data, CountryCode==cou, Subsection==table)
  if (nrow(data)>0){
    data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
    # keep the latest period (excluding projections further than 2 years)
    data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period))
    data <- filter(data, Period <= (as.numeric(thisYear) + 1), Period > (as.numeric(thisYear) - 14))
    #data <- filter(data, !is.na(Observation)) # remove NAs rows
    # keep relevant columns
    data <- select(data, Key, Period, Observation)
    data <- arrange(data, Key, Period)
    
    x <- spread(data, Key, Observation)
    x <- x[,-1] # don't need Period column anymore
    
    # impute NAs and standardize so all sparklines are scales
    for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
      
      x[is.na(x[,i]),i] <- mean(x[,i],na.rm = TRUE)  #impute NAs to the mean of the column
      if (sum(x[,i],na.rm = TRUE)==0){ 
        x[,i] <- 0
        #x[1,i] <- -10
        x[nrow(x),i] <- 10
      }
    }
    #x <- scale(x) # standardize x
    
    par(mfrow=c(ncol(x)+2,1), #sets number of rows in space to number of cols in data frame x
        mar=c(1,0,0,0), #sets margin size for the figures
        oma=c(1,2,1,1)) #sets outer margin
    
    for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
      
      if (sum(x[1:(nrow(x)-1),i])==0){ # paint in white empty rows
        plot(x[,i], #use col data, not rows from data frame x
             col="white",lwd=4, #color the line and adjust width
             axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
        
        axis(2,yaxp=c(min(x[,i],na.rm = TRUE),max(x[,i],na.rm = TRUE),2),col="white",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
        ymin<-min(x[,i],na.rm = TRUE); tmin<-which.min(x[,i]);ymax<-max(x[,i], na.rm = TRUE);tmax<-which.max(x[,i]);
        points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("white","white"),cex=5) # add coloured points at max and min# 
      } else {
        plot(x[,i], #use col data, not rows from data frame x
             col="darkgrey",lwd=4, #color the line and adjust width
             axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
        
        axis(2,yaxp=c(min(x[,i],na.rm = TRUE),max(x[,i],na.rm = TRUE),2),col="white",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
        ymin<-min(x[,i],na.rm = TRUE); tmin<-which.min(x[,i]);ymax<-max(x[,i], na.rm = TRUE);tmax<-which.max(x[,i]); # 
        points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("red","green"),cex=5) # add coloured points at max and min
      }
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    #graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }
}
createSparklines_Split(couName,"table2invest")


## ---- createSparklines_trade ----
createSparklines_Split <- function(couName,table){      
  
  cou <- .getCountryCode(couName)
  ## Examples like Edward Tufte's sparklines:
  
  tableKeys <- unique(filter(TCMN_data, Subsection==table)[,c("Key","IndicatorShort")])
  data <- filter(TCMN_data, CountryCode==cou, Subsection==table)
  if (nrow(data)>0){
    data <- merge(tableKeys,select(data,-IndicatorShort),by="Key",all.x=TRUE)
    # keep the latest period (excluding projections further than 2 years)
    data <- mutate(data, Period = ifelse(is.na(Period),max(as.numeric(Period),na.rm=TRUE),Period))
    data <- filter(data, Period <= (as.numeric(thisYear) + 1), Period > (as.numeric(thisYear) - 14))
    #data <- filter(data, !is.na(Observation)) # remove NAs rows
    # keep relevant columns
    data <- select(data, Key, Period, Observation)
    data <- arrange(data, Key, Period)
    
    x <- spread(data, Key, Observation)
    x <- x[,-1] # don't need Period column anymore
    
    # impute NAs and standardize so all sparklines are scales
    for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
      
      x[is.na(x[,i]),i] <- mean(x[,i],na.rm = TRUE)  #impute NAs to the mean of the column
      if (sum(x[,i],na.rm = TRUE)==0){ 
        x[,i] <- 0
        #x[1,i] <- -10
        x[nrow(x),i] <- 10
      }
    }
    #x <- scale(x) # standardize x
    
    par(mfrow=c(ncol(x)+2,1), #sets number of rows in space to number of cols in data frame x
        mar=c(1,0,0,0), #sets margin size for the figures
        oma=c(1,2,1,1)) #sets outer margin
    
    for (i in 1:ncol(x)){ # setup for statement to loop over all elements in a list or vector
      
      if (sum(x[1:(nrow(x)-1),i])==0){ # paint in white empty rows
        plot(x[,i], #use col data, not rows from data frame x
             col="white",lwd=4, #color the line and adjust width
             axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
        
        axis(2,yaxp=c(min(x[,i],na.rm = TRUE),max(x[,i],na.rm = TRUE),2),col="white",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
        ymin<-min(x[,i],na.rm = TRUE); tmin<-which.min(x[,i]);ymax<-max(x[,i], na.rm = TRUE);tmax<-which.max(x[,i]);
        points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("white","white"),cex=5) # add coloured points at max and min# 
      } else {
        plot(x[,i], #use col data, not rows from data frame x
             col="darkgrey",lwd=4, #color the line and adjust width
             axes=F,ylab="",xlab="",main="",type="l"); #suppress axes lines, set as line plot
        
        axis(2,yaxp=c(min(x[,i],na.rm = TRUE),max(x[,i],na.rm = TRUE),2),col="white",tcl=0,labels=FALSE)  #y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
        ymin<-min(x[,i],na.rm = TRUE); tmin<-which.min(x[,i]);ymax<-max(x[,i], na.rm = TRUE);tmax<-which.max(x[,i]); # 
        points(x=c(tmin,tmax),y=c(ymin,ymax),pch=19,col=c("red","green"),cex=5) # add coloured points at max and min
      }
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    #graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }
}
createSparklines_Split(couName,"table2trade")

## ---- ESTable ----
ESTable <- function(couName){      
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode==cou & Subsection=="table3") #select country, region and world
  if (nrow(data[data$CountryCode==cou,])>0){
    
    couRegion <- as.character(countries[countries$CountryCodeISO3==cou,]$RegionCodeES)  # obtain the region for the selected country
    data <- filter(TCMN_data, CountryCode %in% c(cou,couRegion, "RWe") & Subsection=="table3") #select country, region and world
    
    # country, Region, World descriptors
    country <- as.character(countries[countries$CountryCodeISO3==cou,]$Country)
    region <- as.character(countries[countries$CountryCodeISO3==cou,]$Region) 
    world <- "All Countries"
    
    neighbors <- data.frame(CountryCode=c(cou,couRegion,"RWe"),colName=c(country,region,world), stringsAsFactors = FALSE)
    
    # keep the latest period (excluding projections further than 2 years)
    data <- filter(data, Period <= (as.numeric(thisYear) + 1))
    # remove NAs rows
    data <- filter(data, !is.na(Observation))
    
    # prepare for table
    data <- merge(data, neighbors, by="CountryCode")
    data <- select(data, IndicatorShort, Observation, colName)
    data <- spread(data, colName, Observation)
    data <- data[,c(1,4,3,2)]# reorder columns
    names(data)[1] <-""
    
    # I have to add a dummy column so the alignment works (align)
    data$dummy <- rep("",nrow(data))
    names(data)[ncol(data)] <-""
    
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
    
    data.table <- xtable(data)
    align(data.table) <- c('l','l',rep('r',(ncol(data)-2)),'l')
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\Large",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center")
  
  } else{
    
    data[!is.na(data)] <- ""
    #data <- select(data, Key)
    names(data) <- c(" ",rep(" ",ncol(data)-1))
    data.table <- xtable(data)
    align(data.table) <- rep('l',ncol(data)+1)
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\tiny",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center")
  } 
  
  
  
}
ESTable(couName)

## ---- PolicyTable ----
PolicyTable <- function(couName){      
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode == cou, Subsection=="table4") #select country, region and world
  if (nrow(data[data$CountryCode==cou,])>0){
  
    # prepare for table
    data <- select(data, IndicatorShort, Period, Observation)
    # format numbers
    data$Observation <- format(data$Observation, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    
    data$Observation <- as.numeric(data$Observation)
    data <- spread(data, Period, Observation)
    
    
    # calculate difference in Rank
    data$ChangeRank <- data[,2] - data[,3]
    
    # red for negative, green for positive changes
    data <- mutate(data, ChangeRank = ifelse(ChangeRank<0, paste0("\\color{red}{\\Large",ChangeRank,"}"),
                                             ifelse(ChangeRank>0, paste0("\\color{green}{\\Large",ChangeRank,"}"),ChangeRank)))
    
    names(data) <- c("",paste("DB",names(data)[2],"Rank"),paste("DB",names(data)[3],"Rank"),"Change in Rank")
    
    # I have to add a dummy column so the alignment works (align)
    data$dummy <- rep("",nrow(data))
    names(data)[ncol(data)] <-""
    
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
  } else{
    
    data[!is.na(data)] <- ""
  }   
    
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l',rep('r',(ncol(data)-2)),'r')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\large", 
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control format like colors
  
}
PolicyTable(couName)

## ---- PolicyFacilTable ----
PolicyFacilTable <- function(couName){      
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode == cou, Subsection=="table5") #select country, region and world
  
  if (nrow(data[data$CountryCode==cou,])>0){    
  # prepare for table
    data <- select(data, Key, IndicatorShort, Period, Observation)
    # format numbers
    data$Observation <- format(data$Observation, digits=2, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    
    data$Observation <- as.numeric(data$Observation)
    data <- filter(data, Period %in% c(min(Period),max(Period)))
    
    for (i in 1:nrow(data)){
      
      data$IndicatorShort[i] <- ifelse(!is.na(merge(data,TCMN_indic[TCMN_indic$Subsection=="table5",], by="Key")$Note[i]),
                                       paste0(data$IndicatorShort[i]," \\large{[", merge(data,TCMN_indic[TCMN_indic$Subsection=="table5",], by="Key")$Note[i],"]}"),
                                       data$IndicatorShort[i])  
    }
    
    # escape ampersands
    data$IndicatorShort <- gsub("%", "\\%", data$IndicatorShort, fixed=TRUE)
    
    data <- select(data, -Key)
    data <- spread(data, Period, Observation)
    
    names(data)[1] <- ""
  } else{
    
    data[!is.na(data)] <- ""
  }
    
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <-""
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l',rep('r',(ncol(data)-2)),'r')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large", 
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}
PolicyFacilTable(couName)

## ---- topExportsTable ----
topExportsTable <- function(couName){      
  
  cou <- .getCountryCode(couName)
  
  data <- filter(xWits, CountryCode == cou) #select country, region and world
  
  # prepare for table
  data <- select(data, ProductDescription, ProductCode, Period, TradeValue)
  
  # keep the latest period
  data <- filter(data, Period==max(Period))
  
  # compute the percentage of total value
  data <- mutate(data, percTotalValue = 100*TradeValue/sum(TradeValue, na.rm = TRUE))
  
  # format numbers
  data$TradeValue <- format(data$TradeValue, digits=0, decimal.mark=".",
                            big.mark=",",small.mark=".", small.interval=3)
  
  data$percTotalValue <- format(data$percTotalValue, digits=0, decimal.mark=".",
                            big.mark=",",small.mark=".", small.interval=3)
  
  # get top 5
  data <- head(arrange(data, desc(TradeValue)),5)
  data <- select(data, -Period)
  names(data) <- c("Product (SITC4)", "Code", "Trade Value (millions of US$)", "Percent of total export value")
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <-""
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  data.table <- xtable(data) #, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l',rep('>{\\raggedleft}p{1.5in}',(ncol(data)-2)),'r')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large", 
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center")
  
}
topExportsTable(couName)

## ---- topImportsTable ----
topImportsTable <- function(couName){      
  
  cou <- .getCountryCode(couName)
  
  data <- filter(mWits, CountryCode == cou) #select country, region and world
  
  # prepare for table
  data <- select(data, ProductDescription, ProductCode, Period, TradeValue)
  
  # keep the latest period
  data <- filter(data, Period==max(Period))
  
  # compute the percentage of total value
  data <- mutate(data, percTotalValue = 100*TradeValue/sum(TradeValue, na.rm = TRUE))
  
  # format numbers
  data$TradeValue <- format(data$TradeValue, digits=0, decimal.mark=".",
                            big.mark=",",small.mark=".", small.interval=3)
  
  data$percTotalValue <- format(data$percTotalValue, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  
  data <- select(data, -Period)
  names(data) <- c("Category (HS 88/92)", "Code", "Trade Value (millions of US$)", "Percent of total import value")
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <-""
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  data.table <- xtable(data) #, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l',rep('>{\\raggedleft}p{1.5in}',(ncol(data)-2)),'r')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large", 
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center")
  
}
topImportsTable(couName)



# DBradar <- function(){
#   
#   data8 <- read.csv("/Users/asanchez3/Desktop/Work/TCMN/TCMN_DB.csv")
#   data <- filter(data8, Year == 2016)
#   data <- data[grepl(": Rank",data$Indicator, fixed=TRUE),]
#   data$Indicator <- gsub(": Rank", "", data$Indicator)
#   data <- select(data, Indicator, Year, Malaysia, Tunisia, Jordan, Morocco)
#   # as.numeric
#   countries <- names(data[,3:ncol(data)])
#   data <- data %>% mutate_each_(funs(as.character), countries) %>% mutate_each_(funs(as.numeric), countries)
#   # I must add the max and min columns to make it work:
#   max<-200
#   min <-1
#   data <- cbind(data,max,min)
#   data <- data[,c(1:2,ncol(data)-1,ncol(data),3:(ncol(data)-2))]
#   # order labels ad-hoc:
#   order <- c(1,2,3,7,5,6,8,4,9,10,11)
#   data <- cbind(data,order)
#   data <- arrange(data,order)
#   data <- select(data, -order)# drop order
#   
#   # transpose the data for radarchart to read
#   dataTrans <- as.data.frame(t(data[,3:ncol(data)]))
#   
#   
#   radarchart(dataTrans, axistype=1, caxislabels=seq(from=0,to=max,by=50),
#              plty=c(2,1,2,2),plwd=c(3,6,3,3),pcol=c("orange","darkblue","red","green"),pdensity=c(0, 0, 0, 0),
#              cglwd=2,axislabcol="navy", vlabels=data$Indicator,cex.main=1.6,cex=2.5)
#   # title="Doing Business Indicator Ranks"
#   legend(-2.1,-0.5, legend=names(data[,5:ncol(data)]), seg.len=0.5, pch=3, 
#          bty="n" ,lwd=3, y.intersp=1.5, horiz=FALSE, col=c("orange","darkblue","red","green"))
#   
# }

######################

