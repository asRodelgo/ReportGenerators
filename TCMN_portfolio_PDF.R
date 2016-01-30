# TCMN projects portfolio and country representatives ----------------------------


# # Country projects table ----------------
# projectsTable <- function(couName){
#   
#   cou <- .getCountryCode(couName)
#   couISO2 <- .getISO2(couName)
#   
#   ### IBRD T&C projects -----------------
#   dataTC <- .filterTCProjects(couName)
#   # select relevant variables
#   dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
#                    Approval_Date = BD_APPRVL_DATE, Project_Status = PROJECT_STATUS_NME,
#                    Major_Sector = MAJORSECTOR_NAME1,
#                    Major_Theme = MAJORTHEME_NAME1,Project_Amount, ProjectOrder)
#   # filter by date range
#   dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
#   # arrange
#   dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
#   dataTC <- select(dataTC,-ProjectOrder) # drop ProjectOrder
#   
#   ### IFC projects ----------
#   dataIFC <- .filterIFCProjects(couName)
#   # keep relevant columns
#   dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
#                     Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_FUNDS_MANAGED_BY_IFC,
#                     ProjectOrder
#   )
#   # remove duplicates
#   dataIFC <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
#   # projects within 3 fiscal years in the past
#   dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
#   
#   # arrange
#   dataIFC <- arrange(as.data.frame(dataIFC), ProjectOrder)
#   dataIFC <- select(dataIFC,-ProjectOrder) # drop ProjectOrder
#   
#   # Append both ----------------------
#   data <- rbind_list(dataTC, dataIFC)
#   # remove duplicates
#   data <- data[!duplicated(data$PROJ_ID),]
#   # format Amount
#   data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
#                                 big.mark=",",small.mark=".", small.interval=3)
#   
#   # substitute NAs for "---" em-dash
#   data[is.na(data)] <- "---"
#   names(data) <- c("Project ID","Line", "Project Name", "Approval Date", "Status", "Major Sector", "Major Theme", "Amount (in US$)")
#   
#   return(data)
# }

#############

## ---- testRMD ----

x <- rnorm(100)
y <- rnorm(100)
plot(x,y)

## ---- financeTable ----
projectsTableFinancing <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Approval_Date = BD_APPRVL_DATE, Project_Status = PROJECT_STATUS_NME,
                   Major_Sector = MAJORSECTOR_NAME1,
                   Major_Theme = MAJORTHEME_NAME1,Project_Amount, ProjectOrder)
  # filter by date range
  dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # Financing products
  dataTC <- filter(dataTC, Prod_Line == "Financing")
  
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line) # drop ProjectOrder
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # format Amount
  data <- mutate(data, Project_Amount = ifelse(!is.na(Project_Amount),Project_Amount/1000,NA))
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  
  names(data) <- c("Project ID", "Project Name", "Approval Date", "Status", "Major Sector", "Major Theme", "Amount (in US\\$ K)")
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{1in}','l','l','>{\\raggedright}p{1in}','>{\\raggedright}p{1in}','r','l')
  if (count_ibrd>6){ # squeeze tables in case they are too long
   
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\small",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
}

projectsTableFinancing(couName)

## ---- ASATable ----
projectsTableASA <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Approval_Date = BD_APPRVL_DATE, Project_Status = PROJECT_STATUS_NME,
                   Major_Sector = MAJORSECTOR_NAME1,
                   Major_Theme = MAJORTHEME_NAME1,Project_Amount, ProjectOrder)
  # filter by date range
  dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # Advisory (ASA) products
  dataTC <- filter(dataTC, substr(Prod_Line,1,3) == "Adv")
  
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line) # drop ProjectOrder
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # format Amount
  data <- mutate(data, Project_Amount = ifelse(!is.na(Project_Amount),Project_Amount/1000,NA))
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  
  names(data) <- c("Project ID","Project Name", "Approval Date", "Status", "Major Sector", "Major Theme", "Amount (in US\\$ K)")
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{1in}','l','l','>{\\raggedright}p{1in}','>{\\raggedright}p{1in}','r','l')
  if (count_ibrd>6){ # squeeze tables in case they are too long
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\small",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
  
}

projectsTableASA(couName)

## ---- ASA_IFCTable ----
projectsTableASA_IFC <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_FUNDS_MANAGED_BY_IFC,
                    ProjectOrder
  )
  # projects within 3 fiscal years in the past
  dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), ProjectOrder)
  dataIFC <- select(dataIFC,-ProjectOrder, -Prod_Line) # drop ProjectOrder
  # remove duplicates
  data <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # format Amount
  data <- mutate(data, Project_Amount = ifelse(!is.na(Project_Amount),Project_Amount/1000,NA))
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  
  names(data) <- c("Project ID", "Project Name", "Approval Date", "Status","Amount (in US\\$ K)")
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{2.5in}','l','l',rep('>{\\raggedright}p{1in}',ncol(data.table)-6),'r','l')
  if (nrow(data)>3){
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\small",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
}

projectsTableASA_IFC(couName)

## ---- projectStatus_count ---- 
projectsStatus <- function(couName, count_type){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Status=PROJECT_STATUS_NME, 
                   Approval_Date = BD_APPRVL_DATE, Project_Amount, ProjectOrder)
  # filter by date range
  dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # remove duplicates
  dataTC <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder,-Approval_Date) # drop ProjectOrder
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_FUNDS_MANAGED_BY_IFC,
                    ProjectOrder)
  # remove duplicates
  dataIFC <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # filter by date range
  dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), ProjectOrder)
  dataIFC <- select(dataIFC,-ProjectOrder,-Project_Name, -Approval_Date) # drop ProjectOrder
  
  # Append both -----------------------
  data <- rbind_list(dataTC, dataIFC)
  
  data[is.na(data)] <- "0"
  # remove duplicates
  data <- data[!duplicated(data$PROJ_ID),]
  
  data$Prod_Line <- as.character(data$Prod_Line)
  data <- mutate(data, Prod_Line=ifelse(substr(Prod_Line,nchar(Prod_Line)-2,nchar(Prod_Line))=="IFC",
                                        "ASA IFC",ifelse(Prod_Line=="Financing","Financing","ASA IBRD")))
  data$Prod_Line <- factor(data$Prod_Line, levels=c("Financing", "ASA IBRD", "ASA IFC"))
  
  data <- data %>%
    group_by(Prod_Line,Project_Status) %>%
    mutate(totalAmount = sum(as.numeric(Project_Amount), na.rm=TRUE)/1000, 
           countProjects = n_distinct(PROJ_ID)) %>%
    select(-PROJ_ID,-Project_Amount)
  data <- data[!duplicated(data),]
  # format Amount
  data$totalAmount <- format(data$totalAmount, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  # generate plot
  if (nrow(data)>0){
    # Faceted chart
    if (count_type=="count"){ # plot number of projects
      
      ggplot(data, aes(x=Project_Status, y=countProjects,fill=Project_Status)) +
        geom_bar(stat="identity")+ #stat="identity") +
        facet_grid(~ Prod_Line)+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5)) + 
        labs(x="",y="")
      
    } else{ # plot $ amount of projects
      
      data <- filter(data, !(trimws(totalAmount)=="0"))
      if (nrow(data)>0){
        ggplot(data, aes(x=Project_Status, y=totalAmount, fill=Project_Status)) +
          geom_bar(stat="identity") +
          facet_wrap(~ Prod_Line)+
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                legend.position="top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                plot.title = element_text(lineheight=.5)) + 
          labs(x="",y="")
      } else {
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1.5, 1,"No data available", col="red", cex=1.5)    
      }
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"No data available", col="red", cex=1.5)
  }
  
}

projectsStatus(couName, "count")

## ---- projectStatus_amount ---- 
projectsStatus <- function(couName, count_type){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Status=PROJECT_STATUS_NME, 
                   Approval_Date = BD_APPRVL_DATE, Project_Amount, ProjectOrder)
  # filter by date range
  dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # remove duplicates
  dataTC <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder,-Approval_Date) # drop ProjectOrder
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_FUNDS_MANAGED_BY_IFC,
                    ProjectOrder)
  # remove duplicates
  dataIFC <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # filter by date range
  dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), ProjectOrder)
  dataIFC <- select(dataIFC,-ProjectOrder,-Project_Name, -Approval_Date) # drop ProjectOrder
  
  # Append both -----------------------
  data <- rbind_list(dataTC, dataIFC)
  
  data[is.na(data)] <- "0"
  # remove duplicates
  data <- data[!duplicated(data$PROJ_ID),]
  
  data$Prod_Line <- as.character(data$Prod_Line)
  data <- mutate(data, Prod_Line=ifelse(substr(Prod_Line,nchar(Prod_Line)-2,nchar(Prod_Line))=="IFC",
                                        "ASA IFC",ifelse(Prod_Line=="Financing","Financing","ASA IBRD")))
  data$Prod_Line <- factor(data$Prod_Line, levels=c("Financing", "ASA IBRD", "ASA IFC"))
  
  data <- data %>%
    group_by(Prod_Line,Project_Status) %>%
    mutate(totalAmount = sum(as.numeric(Project_Amount), na.rm=TRUE)/1000, 
           countProjects = n_distinct(PROJ_ID)) %>%
    select(-PROJ_ID,-Project_Amount)
  data <- data[!duplicated(data),]
  # format Amount
  data$totalAmount <- format(data$totalAmount, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  # generate plot
  if (nrow(data)>0){
    # Faceted chart
    if (count_type=="count"){ # plot number of projects
      
      ggplot(data, aes(x=Project_Status, y=countProjects,fill=Project_Status)) +
        geom_bar(stat="identity")+ #stat="identity") +
        facet_grid(~ Prod_Line)+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5)) + 
        labs(x="",y="")
      
    } else{ # plot $ amount of projects
      
      data <- filter(data, !(trimws(totalAmount)=="0"))
      if (nrow(data)>0){
        ggplot(data, aes(x=Project_Status, y=totalAmount, fill=Project_Status)) +
          geom_bar(stat="identity") +
          facet_wrap(~ Prod_Line)+
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                legend.position="top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                plot.title = element_text(lineheight=.5)) + 
          labs(x="",y="")
      } else {
        plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
        graphics::text(1.5, 1,"No data available", col="red", cex=1.5)    
      }
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"No data available", col="red", cex=1.5)
  }
  
}

projectsStatus(couName, "amount")

## ---- projectsTreemap_sectors ----
projectsTreemap <- function(couName, sectTheme){
  
  if (sectTheme=="sector"){
    data <- .projectsSectors(couName) 
    
  } else {
    data <- .projectsThemes(couName) 
  }
  
  if (nrow(data)>0){
    
    names(data) <- c("Name","PctTotal","Pct")
    #data$color <- terrain.colors(length(data$Name)) # add the color
    data$color <- rainbow(length(data$Name)) # add the color
    # format numbers
    data$Pct <- data$Pct*100
    data$PctTotal <- format(data$PctTotal, digits=0, decimal.mark=".",
                            big.mark=",",small.mark=".", small.interval=3)
    data$Pct <- format(data$Pct, digits=1, decimal.mark=".",
                       big.mark=",",small.mark=".", small.interval=3)
    data$Pct <- as.numeric(data$Pct)
    #data <- select(data, -Period)
    
    treemap(data,
            index=c("Name","Pct"),
            vSize="Pct",
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
projectsTreemap(couName, "sector")

## ---- projectsTreemap_themes ----
sectThemesTable <- function(couName,sectTheme){
  
  if (sectTheme=="sector"){
    data <- .projectsSectors(couName) 
    
  } else {
    data <- .projectsThemes(couName) 
  }
  
  names(data) <- c("Name","PctTotal","Pct")
  data <- select(data, -PctTotal)
  # format numbers
  data$Pct <- data$Pct*100
  data$Pct <- format(data$Pct, digits=1, decimal.mark=".",
                     big.mark=",",small.mark=".", small.interval=3)
  data$Pct <- as.numeric(data$Pct)
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  
  data.table <- xtable(data, digits=rep(1,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','r',rep('>{\\raggedleft}p{0.8in}',ncol(data.table)-3),'l')
  print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
        size="\\Large",
        booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
        sanitize.text.function = function(x){x}) # include sanitize to control formats
  
}

projectsTreemap(couName, "themes")

## ---- projectsPeople ----
projectsPeople <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, Staff_Name = FULL_NAME, job_title, 
                   Location = CITY_NAME, Role = UPI_ROLE_DESC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Project_Status=PROJECT_STATUS_NME, grade, duty_country, practice,
                   ProjectOrder,WORK_ALPHA,Approval_Date=BD_APPRVL_DATE)
  
  dataTC <- filter(dataTC, practice == "TAC", grade %in% c("GF","GG","GH","EC1","EC2","EC3")) #, position_type == "PRIMARY"
  dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  dataTC <- select(dataTC, -grade,-duty_country,-practice, -Prod_Line, -Project_Status,-Approval_Date)
  dataTC <- dataTC[!duplicated(dataTC),]
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, Staff_Name = FULL_NAME,job_title, 
                    Location = CITY_NAME, Role = UPI_ROLE_DESC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Project_Status, grade, duty_country, practice, emplyment_type,
                    Approval_Date = ASIP_APPROVAL_DATE,ProjectOrder,WORK_ALPHA)
  # projects within 3 fiscal years in the past
  dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # apply filters
  dataIFC <- filter(dataIFC, practice == "TAC",grade %in% c("GF","GG","GH","EC1","EC2","EC3")) #, position_type == "PRIMARY"
  dataIFC <- select(dataIFC, -duty_country,-practice,-emplyment_type,-Approval_Date, -Prod_Line, -Project_Status)
  dataIFC <- dataIFC[!duplicated(dataIFC),]
  
  # Append both --------------------------
  data <- rbind_list(dataTC, dataIFC)
  data <- filter(data, tolower(substr(WORK_ALPHA,1,3))=="gtc")
  # arrange
  data <- arrange(as.data.frame(data), Staff_Name, PROJ_ID, ProjectOrder, Role)
  data <- data[!(duplicated(data[,c("Staff_Name","PROJ_ID")])),]
  data <- select(as.data.frame(data), -ProjectOrder, -grade, -WORK_ALPHA)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  # make sure staff info appear only once
  i <- 1
  while (i <= nrow(data)){
    j <- i + 1
    while(data$Staff_Name[j]==data$Staff_Name[i]){
      data$Staff_Name[j]<-""
      data$job_title[j]<-""
      data$Location[j]<-""
      j <- j + 1
      if (j > nrow(data)) j <- j - 1
    }
    i <- ifelse(j == nrow(data),j+1,j)
  }
  
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$job_title <- gsub("&", "\\&", data$job_title, fixed=TRUE)
  data$Location <- gsub("&", "\\&", data$Location, fixed=TRUE)
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data) <- c("Name","Job title","Location","Role","Project ID","Project name","")
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('>{\\raggedright}p{1.4in}','>{\\raggedright}p{1.4in}',rep('>{\\raggedright}p{1in}',ncol(data.table)-4),'l','>{\\raggedright}p{1.5in}','l')
  if (nrow(data)>13){
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x}) # include sanitize to control formats
  } else {
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\small",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x}) # include sanitize to control formats
  }
}
projectsPeople(couName)

## ---- peopleGrades ----
projectsStaffStats <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Project_Status=PROJECT_STATUS_NME, Staff_Name = FULL_NAME, grade,
                   practice)
  # apply filters
  dataTC <- filter(dataTC, practice == "TAC")
  
  dataTC <- dataTC %>%
    filter(grade %in% c("GF","GG","GH","EC1","EC2","EC3")) %>%
    group_by(grade) %>%
    mutate(grade_count = length(grade)) %>%
    select(grade,grade_count)
  
  dataTC <- dataTC[!duplicated(dataTC),]
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Staff_Name = FULL_NAME, grade, 
                    Approval_Date = ASIP_APPROVAL_DATE, practice)
  # projects within 3 fiscal years in the past
  dataIFC <- filter(dataIFC, Approval_Date >= "2013-07-01") #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # apply filters
  dataIFC <- filter(dataIFC, practice == "TAC") #, position_type == "PRIMARY"
  
  dataIFC <- dataIFC %>%
    filter(grade %in% c("GF","GG","GH","EC1","EC2","EC3")) %>%
    group_by(grade) %>%
    mutate(grade_count = length(grade)) %>%
    select(grade,grade_count)
  
  dataIFC <- dataIFC[!duplicated(dataIFC),]
  
  # Append both --------------------------
  data <- rbind_list(dataTC, dataIFC)
  data <- data %>%
    group_by(grade) %>%
    mutate(grade_count = sum(grade_count))
  data <- data[!duplicated(data),]
  # arrange
  data <- arrange(as.data.frame(data), grade)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  # bar chart
  ggplot(data, aes(x=grade,y=grade_count,fill=as.factor(grade))) +
    geom_bar(position="dodge",stat="identity") +
    coord_flip() +
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position="top",
          panel.border = element_blank(),
          panel.background = element_blank(),plot.title = element_text(lineheight=.5),
          axis.text.y = element_text(size=15)#, axis.text.x = element_blank()
    ) + 
    labs(x="",y="")
  
}

projectsStaffStats(couName)


