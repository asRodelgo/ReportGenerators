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
#   names(data) <- c("Project ID","Line", "Project Name", "Approval Date", "Status", "Major Sector", "Major Theme", "Amount (in US\\$)")
#   
#   return(data)
# }

#############

## ---- testRMD ----

#x <- rnorm(100)
#y <- rnorm(100)
#plot(x,y)

## ---- lendingPipeline ----
projectsTableLendingPipeline <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Lending_Inst_Type = LENDING_INSTR_TYPE_NME,
                   Begin_Appraisal = BEGIN_APPRAISAL_DATE,Project_Amount,
                   Latest_Sort = rate_indicator, FY_Expenses = CURRENT_FY_COST,
                   Cum_Expenses = CUMULATIVE_FY_COST,FY_Prob = FY_PROB_TYPE_CODE,
                   ProjectOrder,url)
  # Financing products in Pipeline (ProjectOrder==2)
  dataTC <- filter(dataTC, Prod_Line == "Financing" & ProjectOrder==2)
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  #data <- mutate(data, PROJ_ID = 
  #                 paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$FY_Expenses <- data$FY_Expenses/1000
  data$Cum_Expenses <- data$Cum_Expenses/1000

    # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$FY_Expenses <- format(data$FY_Expenses, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$Cum_Expenses <- format(data$Cum_Expenses, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  
  names(data) <- c("Project ID", "Project Name", "Team Leader", "Approval Date", "Lending Inst. Type",
                   "Begin Appraisal", "Commitment (US\\$M)","Latest Sort Overall Risk Rating","FY Expenses (US\\$K)",
                   "Cum Expenses (US\\$K)","FY Prob")
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{1in}','>{\\raggedright}p{1in}',rep('>{\\raggedright}p{0.5in}',3),'>{\\raggedright}p{0.6in}',rep('>{\\raggedright}p{0.5in}',2),rep('>{\\raggedleft}p{0.5in}',2),'l')
  if (count_ibrd>6){ # squeeze tables in case they are too long
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\scriptsize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
}
projectsTableLendingPipeline(couName)

## ---- portfolioActive ----
projectsTablePortfolioActive <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Lending_Inst_Type = LENDING_INSTR_TYPE_NME,
                   Closing_Date = REVISED_CLS_DATE,Project_Amount,
                   Undisb_Bal = total_undis_balance,DO_RATING, IP_RATING,
                   Latest_Sort = rate_indicator,
                   Months_Problem = No_of_Months_in_problem_status,
                   ProjectOrder,url)
  # Financing products in Active (ProjectOrder==1)
  dataTC <- filter(dataTC, Prod_Line == "Financing" & ProjectOrder==1)
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  #data <- mutate(data, PROJ_ID = 
  #                 paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$Undisb_Bal <- data$Undisb_Bal/1000000
  
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$Undisb_Bal <- format(data$Undisb_Bal, digits=0, decimal.mark=".",
                            big.mark=",",small.mark=".", small.interval=3)
  data$Months_Problem <- format(data$Months_Problem, digits=1, decimal.mark=".",
                            big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  
  names(data) <- c("Project ID", "Project Name", "Team Leader", "Approval Date", "Lending Inst. Type",
                   "Closing Date", "Commitment (US\\$M)","Undisbursed Balance (US\\$M)",
                   "Project Rating DO", "Project Rating IP","Overall Risk",
                   "Months in Problem Status")
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{1in}','>{\\raggedright}p{1in}',rep('>{\\raggedright}p{0.5in}',3),rep('>{\\raggedleft}p{0.5in}',2),rep('>{\\raggedright}p{0.4in}',3),'>{\\raggedleft}p{0.4in}','l')
  if (count_ibrd>6){ # squeeze tables in case they are too long
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\scriptsize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
}
projectsTablePortfolioActive(couName)

## ---- portfolioClosed ----
projectsTablePortfolioClosed <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Lending_Inst_Type = LENDING_INSTR_TYPE_NME,
                   Closing_Date = REVISED_CLS_DATE,Project_Amount,
                   DO_RATING, IP_RATING, ieg_Outcome,
                   ProjectOrder,url)
  # Financing products in Closed (ProjectOrder==3)
  dataTC <- filter(dataTC, Prod_Line == "Financing" & ProjectOrder==3)
  # compute IEG_Ouctome codes:
  dataTC <- mutate(dataTC, ieg_Outcome = ifelse(is.na(ieg_Outcome),"---",
                                                ifelse(substr(ieg_Outcome,1,1)=="H",
                                                       ifelse(substr(ieg_Outcome,8,8)=="S","HS","HU"),
                                                       ifelse(substr(ieg_Outcome,1,1)=="M",
                                                              ifelse(substr(ieg_Outcome,12,12)=="S","MS","MU"),
                                                              ifelse(substr(ieg_Outcome,1,1)=="N",
                                                                     ifelse(substr(ieg_Outcome,5,6)=="AP","NAP",ifelse(substr(ieg_Outcome,5,6)=="AV","NA","NR")),
                                                                     substr(ieg_Outcome,1,2))))))
  
  # filter by date range. Last 2 years
  dataTC <- filter(dataTC, (Closing_Date >= (Sys.Date() - 730)) | (is.na(Closing_Date)))
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Closing_Date),desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  #data <- mutate(data, PROJ_ID = 
  #                 paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  #
  names(data) <- c("Project ID", "Project Name", "Team Leader", "Approval Date", "Lending Inst. Type",
                   "Closing Date", "Commitment (US\\$M)",
                   "Project Rating DO", "Project Rating IP", "IEG Outcome Rating")
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{1.5in}','>{\\raggedright}p{1in}',rep('>{\\raggedright}p{0.5in}',3),'>{\\raggedleft}p{0.6in}',rep('>{\\raggedright}p{0.5in}',3),'l')
  if (count_ibrd>6){ # squeeze tables in case they are too long
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\scriptsize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
}
projectsTablePortfolioClosed(couName)


# ---- ASAActive ----
projectsTableASAActive <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Prod_Line, PROD_LINE_CODE, RAS, Current_ExpBB = CURRENT_BB_COST,
                   FY_Expenses = CURRENT_FY_COST, Cum_ExpBB = CUMULATIVE_BB_COST,
                   Cum_Expenses = CUMULATIVE_FY_COST,
                   ProjectOrder,url)
  # AAA in Active (ProjectOrder==1)
  dataTC <- filter(dataTC, Prod_Line == "Advisory Services and Analytics (ASA) IBRD" 
                   & ProjectOrder==1)
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder,-Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  data <- as.data.frame(data)
  #data <- mutate(data, PROJ_ID = 
  #                 paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$Current_ExpBB <- data$Current_ExpBB/1000
  data$Cum_ExpBB <- data$Cum_ExpBB/1000
  data$FY_Expenses <- data$FY_Expenses/1000
  data$Cum_Expenses <- data$Cum_Expenses/1000
  
  # format Amount
  data$FY_Expenses <- format(data$FY_Expenses, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$Cum_Expenses <- format(data$Cum_Expenses, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
  data$Current_ExpBB <- format(data$Current_ExpBB, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
  data$Cum_ExpBB <- format(data$Cum_ExpBB, digits=0, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  
  names(data) <- c("Task ID", "Task Name", "Team Leader", "Management Approval Date", 
                   "Product Line","RAS (Y/N)","Current Expenditure BB (US\\$K)", "Current Expenditure Total (US\\$K)",
                   "Lifetime Expenditure BB (US\\$K)","Lifetime Expenditure Total (US\\$K)")
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{1in}','>{\\raggedright}p{1in}',rep('>{\\raggedright}p{0.6in}',3),rep('>{\\raggedleft}p{0.6in}',4),'l')
  if (count_ibrd>6){ # squeeze tables in case they are too long
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\scriptsize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
}
projectsTableASAActive(couName)

# ---- ASAClosed ----
projectsTableASAClosed <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Prod_Line, PROD_LINE_CODE, RAS, Current_ExpBB = CURRENT_BB_COST,
                   FY_Expenses = CURRENT_FY_COST, Cum_ExpBB = CUMULATIVE_BB_COST,
                   Cum_Expenses = CUMULATIVE_FY_COST,
                   ProjectOrder,url)
  # AAA in Active (ProjectOrder==1)
  dataTC <- filter(dataTC, Prod_Line == "Advisory Services and Analytics (ASA) IBRD" 
                   & ProjectOrder==3)
  count_ibrd <- nrow(dataTC) # will determine the size of the table
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Approval_Date))
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  data <- as.data.frame(data)
  #data <- mutate(data, PROJ_ID = 
  #                 paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$Current_ExpBB <- data$Current_ExpBB/1000
  data$Cum_ExpBB <- data$Cum_ExpBB/1000
  data$FY_Expenses <- data$FY_Expenses/1000
  data$Cum_Expenses <- data$Cum_Expenses/1000

  # format Amount
  data$FY_Expenses <- format(data$FY_Expenses, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$Cum_Expenses <- format(data$Cum_Expenses, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
  data$Current_ExpBB <- format(data$Current_ExpBB, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
  data$Cum_ExpBB <- format(data$Cum_ExpBB, digits=0, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  
  names(data) <- c("Task ID", "Task Name", "Team Leader", "Management Approval Date", 
                   "Product Line","RAS (Y/N)","Current Expenditure BB (US\\$K)", "Current Expenditure Total (US\\$K)",
                   "Lifetime Expenditure BB (US\\$K)","Lifetime Expenditure Total (US\\$K)")
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{1in}','>{\\raggedright}p{1in}',rep('>{\\raggedright}p{0.6in}',3),rep('>{\\raggedleft}p{0.6in}',4),'l')
  if (count_ibrd>6){ # squeeze tables in case they are too long
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\scriptsize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
}
projectsTableASAClosed(couName)


# ---- ASA_IFCActive ----
projectsTableASA_IFC <- function(couName, status){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Project_Name = PROJECT_NAME, Team_Leader = PROJECT_LEADER,
                    Approval_Date = ASIP_APPROVAL_DATE, Closing_Date = IMPLEMENTATION_END_DATE,
                    Project_Status, Project_Amount = TOTAL_FUNDING,
                    Current_Exp = PRORATED_TOTAL_FYTD_EXPENSE, ProjectOrder)
  dataIFC <- filter(dataIFC, ProjectOrder == 1)
  #dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  count_ifc <- nrow(dataIFC) # will determine the size of the table
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), desc(Approval_Date))
  dataIFC <- select(dataIFC,-ProjectOrder, -Project_Status) # drop ProjectOrder
  # remove duplicates
  data <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # Scale amounts
  data$Project_Amount <- data$Project_Amount/1000
  data$Current_Exp <- data$Current_Exp/1000
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$Current_Exp <- format(data$Current_Exp, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataIFC)-1)))
  }
  names(data) <- c("Project ID", "Project Name", "Team Leader","IP Approval Date", 
                   "Expected End Date","Approval Value (in US\\$K)", "Current Expenditure (in US\\$K)")
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{1.6in}','>{\\raggedright}p{1.5in}',rep('>{\\raggedright}p{0.7in}',2),rep('>{\\raggedleft}p{0.7in}',2),'l')
  if (count_ibrd>6){ # squeeze tables in case they are too long
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\scriptsize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
}
projectsTableASA_IFC(couName,"Closed")

# ---- ASA_IFCClosed ----
projectsTableASA_IFC <- function(couName, status){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Project_Name = PROJECT_NAME, Team_Leader = PROJECT_LEADER,
                    Approval_Date = ASIP_APPROVAL_DATE, Closing_Date = IMPLEMENTATION_END_DATE,
                    Project_Status, Project_Amount = TOTAL_FUNDING,
                    Current_Exp = PRORATED_TOTAL_FYTD_EXPENSE, ProjectOrder)
  dataIFC <- filter(dataIFC, ProjectOrder == 3)
  #dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  count_ifc <- nrow(dataIFC) # will determine the size of the table
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), desc(Closing_Date))
  dataIFC <- select(dataIFC,-ProjectOrder, -Project_Status) # drop ProjectOrder
  # remove duplicates
  data <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # Scale amounts
  data$Project_Amount <- data$Project_Amount/1000
  data$Current_Exp <- data$Current_Exp/1000
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$Current_Exp <- format(data$Current_Exp, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  nrow <- nrow(data)
  data <- sapply(data, function(x) gsub("NA", "---", x, fixed=TRUE))
  if (nrow==1){
    data <- as.data.frame(t(data))
  } else{
    data <- as.data.frame(data)
  }
  # escape reserved characters
  data$Project_Name <- gsub("%", "\\%", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("&", "\\&", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("_", "\\_", data$Project_Name, fixed=TRUE)
  data$Project_Name <- gsub("#", "\\#", data$Project_Name, fixed=TRUE)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataIFC)-1)))
  }
  names(data) <- c("Project ID", "Project Name", "Team Leader","IP Approval Date", 
                   "Expected End Date","Approval Value (in US\\$K)", "Current Expenditure (in US\\$K)")
  
  # I have to add a dummy column so the alignment works (align)
  data$dummy <- rep("",nrow(data))
  names(data)[ncol(data)] <- ""
  
  data.table <- xtable(data, digits=rep(0,ncol(data)+1)) #control decimals
  align(data.table) <- c('l','l','>{\\raggedright}p{1.6in}','>{\\raggedright}p{1.5in}',rep('>{\\raggedright}p{0.7in}',2),rep('>{\\raggedleft}p{0.7in}',2),'l')
  if (count_ibrd>6){ # squeeze tables in case they are too long
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\scriptsize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize", scalebox = 0.85,
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  }
}
projectsTableASA_IFC(couName,"Active")
#############














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
          size="\\scriptsize",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize",
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
  dataTC <- filter(dataTC, Prod_Line != "Financing")
  
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
          size="\\scriptsize",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize",
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
          size="\\scriptsize",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x} # include sanitize to control formats
    )
  } else{
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize",
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
  if (nrow(data)>1){
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
          size="\\scriptsize",
          booktabs = FALSE, table.placement="", hline.after = c(0) ,latex.environments = "center",
          sanitize.text.function = function(x){x}) # include sanitize to control formats
  } else {
    
    print(data.table, include.rownames=FALSE,include.colnames=TRUE, floating=FALSE, 
          size="\\footnotesize",
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


