superSectorGen<- function(totalEffectsOutput,Tax,df_superSector2,df_superSector2_woHH,totalEffectsEarnings,totalEffectsJobs1,CDF,totals_woHH,totalDirects_woHH,
                   df_new2,taxaccounts,EPMIE,compensationtbl,GDP,GDP_woHH,totalEffectsJobs1_woHH,totalEffectsEarnings_woHH,totalEffectsOutput_woHH,wb_1,df_tax2){
  #Super SEctor time
  print("A")
  df_tax2[1] <- totalEffectsOutput * Tax[1]
  df_tax2[2] <- totalEffectsOutput * Tax[2]
  df_tax2[3] <- totalEffectsOutput * Tax[3]
  
  df_tax2 = df_tax2 [1:403,] #Cutting out Compensation row. 
  
  #Here all the taxes are being calculated. Yes, df_tax2 was overwritten in df_gen2. I believe I will get rid of that in df_gen2 once the code 
  #is running optimally. There are certain things I want to get working before I take liberty in removing code that 
  #my predecessor put in. This is to streamline the troubleshooting process. 
  colnames(df_superSector2) <- c("Output", "Earnings", "Employment", "GDP")
  #set col names.
  df_superSector2_woHH <- as.data.frame(matrix(0, nrow = 14, ncol = 4))
  #this supersector variable is temporary and will only be used for calculation purposes. 
  colnames(df_superSector2_woHH) <- c("Output", "Earnings", "Employment", "GDP")
  #set col names. 
  aggregate <- function(matrixFit, col, df){
    #The Aggregate function is used to create the first table in the SuperSector sheet. The indeces in matrixFit portion were carefully gotten from the sample
    # GA2016 spreadsheet. They are based on the values for every industry and subindustry. These may change from state to state, so that's something to take a look at. 
    df_superSector <- df
    df_superSector[1,col]  <- sum(matrixFit[1:13,]) #agric
    df_superSector[2,col]  <- sum(matrixFit[14:21,]) #mining
    df_superSector[3,col]  <- sum(matrixFit[22:24,]) #utilities
    df_superSector[4,col]  <- sum(matrixFit[25:36,]) #construction
    df_superSector[5,col]  <- sum(matrixFit[37:271,]) #manufacturing 
    df_superSector[6,col]  <- sum(matrixFit[272:282,]) #wholesale
    
    #BElow is transportation and wareheousing
    df_superSector[7,col]  <- sum(matrixFit[283:291,]) #retail trade
    
    df_superSector[8,col]  <- sum(matrixFit[292:300,]) #transportation and warehousing
    #Below is finance insurance real estate
    df_superSector[9,col]  <- sum(matrixFit[301:315,]) #info
    df_superSector[10,col] <- sum(matrixFit[c(316:323,324:330),]) #finance insurance HAVE TO ADD 324-330
    df_superSector[11,col] <- sum(matrixFit[c(331:345,346:354),]) #Professional...
    df_superSector[12,col] <- sum(matrixFit[c(355:357,358:370),]) #Educational Services...
    df_superSector[13,col] <- sum(matrixFit[c(371:378,379:382),]) #Arts
    df_superSector[14,col] <- sum(matrixFit[c(383:394,395:403),]) #Other
    return (df_superSector)
    
  }
  print("C.9")
  #First Table
  df_superSector2 <- as.data.frame(matrix(0, nrow = 14, ncol = 4))
  #The code above is actually very important for resetting df_superSector in instances where the Update button is pressed multiple times. 
  df_superSector_test <- as.data.frame(matrix(0, nrow = 14, ncol = 4))
  print("C.92")
  #This variable was used for troubleshooting. 
  df_superSector2 <-aggregate((GDP),4, (aggregate((totalEffectsJobs1),3,(aggregate(((totalEffectsEarnings)),2,(aggregate((totalEffectsOutput),1,df_superSector2)))))))
#(aggregate((totalEffectsJobs1),3,(aggregate(((totalEffectsEarnings)),2,(aggregate((totalEffectsOutput),1,df_superSector2))))))
  #print(df_superSector2)
    print("C.93")
    #aggregate((GDP),4, (aggregate((totalEffectsJobs1),3,(aggregate(((totalEffectsEarnings)),2,(aggregate((totalEffectsOutput),1,df_superSector2)))))))
  print("C.93")
  #Aggregate is used to combine the jobs, earnings, output and gdp values for every super sector group. 
  testing<-aggregate((totalEffectsJobs1),1,df_superSector_test)
  print("C.95")
  
  #testing was used for troubleshooting. 
  totals <- c(sum(df_superSector2[1]), sum(df_superSector2[2]), sum(df_superSector2[3]), sum(df_superSector2[4]))
  #sum() adds up all the inputs in every column. We are combining all the values for Jobs, Earnings, Output, GDP sums into the last row, the totals row. 
   df_superSector2 <- rbind(df_superSector2, totals)
 # df_superSector2[,2]<-lapply(df_superSector2[,2],strtoi)#strtoi(df_superSector2[,2])
  #row binding happens above. 
  colnames(df_superSector2) <- c("Output", "Earnings", "Employment", "GDP")
  #Added column names. 
  print("D")
  
  df_superSector2_woHH <- aggregate((GDP_woHH),4, (aggregate((totalEffectsJobs1_woHH),3,(aggregate(((totalEffectsEarnings_woHH)),2,(aggregate((totalEffectsOutput_woHH),1,df_superSector2_woHH)))))))
  #without household supersector values calculated for later calculations. It's not global since it's not used for much outside of this function.
  print("D.2")
  
  totals_woHH <- c(sum(df_superSector2_woHH[1]), sum(df_superSector2_woHH[2]), sum(df_superSector2_woHH[3]), sum(df_superSector2_woHH[4]))
  print("D.3")
  df_superSector2_woHH <- rbind(df_superSector2_woHH, totals_woHH)
  print("D.4")
  #print(wb_1[,2])
  colnames(df_superSector2_woHH) <- c("Output", "Earnings", "Employment", "GDP")
  print("D.45")
  
  #The above code is just repetition from the normal supersector code. 
  labels1<-wb_1[4:18,2] 
   #labels1<-read.xlsx(wb,sheet = 1,startRow = 1, rows = 4:18, cols = 2,skipEmptyRows = F) 
  print("E")
  #Second Table
  temp <- CDF[4]
  
  totalDirects_woHH <- c(sum(df_new2[,4]),sum(df_new2[,3]),sum(df_new2[,2]),sum(df_new2[,4]*temp))
  #These sums just find totals again. 
  indirect_woHH <- totals_woHH - totalDirects_woHH 
  #totalDirects stays the same for with and without households. 
  #totals without households - total directs without households
  multipliers_woHH <- totals_woHH/totalDirects_woHH
  df_effectsDistribution_woHH <- rbind(totalDirects_woHH, indirect_woHH, totals_woHH, multipliers_woHH)
  #This variable binds all these rows into the second table values except without households.
  print("F")
  totalDirects <- c(sum(df_new2[,4]),sum(df_new2[,3]),sum(df_new2[,2]),sum(df_new2[,4]*temp))
  indirect <- indirect_woHH
  induced <- totals-totalDirects-indirect
  multipliers <- totals/totalDirects
  df_effectsDistribution <- rbind(totalDirects, indirect, induced, totals, multipliers)
  #####^2nd tab;e
  #This variable binds all these rows into the second table values
  
  print("G")
  colnames(df_effectsDistribution) <- c("Output", "Earnings", "Employment", "GDP")
  colnames(df_effectsDistribution_woHH) <- c("Output", "Earnings", "Employment", "GDP")
  #adding column names for these final tables. 
  
  
  #Third Table
  #We are working with compensationtbl
  print("G.1")
  comp <-wb_1[,30]
  #comp <-read.xlsx(wb,sheet = 1,startRow = 1,cols = c(30),skipEmptyRows = F) 
  print("G.2")
  compensation <- sum(totalEffectsOutput*(comp)[1:403,])
  #The range from 1 to 403 is provided to exclude "Compensation of Employees" on row 404
  fed <- sum(df_tax2[1:403,1])
  sta <- sum(df_tax2[1:403,2])
  loc <- sum(df_tax2[1:403,3])
  #The above three variables are for taxes. 
  print("G.3")
  tottax <- fed + sta + loc
  PDR <- sum(df_superSector2[15,4])-(compensation + tottax)
  compensationtbl[4] <- unlist(c(compensation, tottax, loc, sta,fed,  PDR, df_superSector2[15,4])) #sum(df_superSector2[4])
  print("G.4")
  compensationtbl[,3]<-""
  compensationtbl[,2]<-""
  compensationtbl[,1]<-""
  
  colnames(compensationtbl) <- c("Output", "Earnings", "Employment", "GDP")
  #If confused, please refer to GA2016.xlsx SuperSector spreadshete. 
  print("H")
  
  
  #Fourth Table
  #Make temp2 read its value from the faile. 
  temp2<-0
  HLI <- round(.88863 * df_superSector2[15,2],2)
  H_loc <- round(HLI * Tax[404,3],2) #round(loc * Tax[404,3],2)
  H_sta <- round(HLI * Tax[404,2],2)#round(sta * Tax[404,2],2)  
  H_fed <- round(HLI * Tax[404,1],2) #round(fed * Tax[404,1],2)
  H_tottax <- H_loc + H_sta + H_fed
  household <- c(HLI, H_tottax, H_loc, H_sta, H_fed)
  business <- c(compensation, tottax, loc, sta, fed)
  total <- c(0,(H_tottax+tottax),(loc + H_loc),(sta + H_sta),(fed + H_fed)) #instead of 0 it was (HLI + compensation)
  taxaccounts[3] <- unlist(household)
  taxaccounts[2] <- unlist(business)
  taxaccounts[4] <- unlist(total)
  #Unlist just makes the variables into modifiable vectors  
  colnames(taxaccounts) <- c("Output", "Earnings", "Employment", "GDP")
  print("I")
  
  Init_Expend <- 1000 * sum(df_new2[,4])
  taxaccounts[,1]<-""
  taxaccounts[1,4]<-""
  
  
  #Fifth Table
  EmPM <- round(df_superSector2[15,3]/(Init_Expend/1000000),2)
  EPM  <- round((df_superSector2[15,2]*1000)/(Init_Expend/1000000),2)
  STPM <- round(((H_sta + sta) * 1000) / (Init_Expend/1000000),2)
  LTPM <- round(((H_loc + loc) * 1000) / (Init_Expend/1000000),2)
  GDPPM <- round((df_superSector2[15,4] * 1000) / (Init_Expend/1000000),2)
  EPMIE[4] <- unlist(c(EmPM,EPM, STPM, LTPM, GDPPM))
  colnames(EPMIE) <- c("Output", "Earnings", "Employment", "GDP")
  EPMIE[,1]<-""
  EPMIE[,2]<-""
  EPMIE[,3]<-""
  print("J")
  
  
  #Sixth Tabular Value 
  initial_exp<-0
  
  for ( j in 1:length(df_new2[,4]))
  { 
    
    
    initial_exp <- initial_exp + df_new2[j,4]
  }
  
  hashbind <- function(..., hash = " ") {
    #Hasbind combines tables of various sizes together. 
    lst <- list(...)
    Nchar <- max(rapply(lst, function(y) nchar(as.character(y)))) + 2
    do.call(
      rbind, 
      lapply(lst, function(x) {
        rbind(x, substr(paste(rep(hash, Nchar), collapse = ""), 1, Nchar))
      }))
  }
  df_effectsDistribution <- rbind(c(" "),df_effectsDistribution)
  #We are adding spaces between the rows to make the tables more readable in the spreadsheet.. 
  compensationtbl <- rbind(c(" "),compensationtbl)
  taxaccounts <- rbind(c(" "),taxaccounts)
  EPMIE <- rbind(c(" "),EPMIE)
  initial_exp<-initial_exp*1000
  initial_exp<- cbind("","","",initial_exp)
  colnames(initial_exp) <- c("Output", "Earnings", "Employment", "GDP")
  print(hashbind(df_superSector2))#,df_effectsDistribution,compensationtbl,taxaccounts,EPMIE, initial_exp))
  df_superSector2 <- rbind(c(" "),df_superSector2)
  main4 <- hashbind(df_superSector2,df_effectsDistribution,compensationtbl,taxaccounts,EPMIE, initial_exp)#       , taxaccounts, EPMIE)
  #main4 is the aggregate of all tha tables. It is the dataframe that will be posted in the SuperSector spreadsheet. 
   main4_woHH <- hashbind(df_superSector2_woHH,df_effectsDistribution_woHH)
   #main4_woHH was made for reference for easier troubleshooting. 
  super_sector_woHH<-main4_woHH
  names <- c("I.  Total Effects (Direct + Indirect/Induced)","Agriculture,Forestry,Fishing,and Hunting", "Mining", "Utilities", "Construction", "Manufacturing", "Wholesale Trade","Retail Trade", "Transportation and Warehousing","Information","Finance, Insurance, Real Estate, Rental, and Leasing","Professional and Business Services" ,"Educational Services, Health Care, and Social Assistance", "Arts, Entertainment, Recreation, and Hospitality","Other Services (including Government)","Total Effects","  ","II. Distribution of Effects and Multipliers", "Direct Effects", "IndirectEffects","Induced Effects", "Total Effects", "Multipliers(= 4 / 1)", " ","III. Composition of GDP", "Compensation", "Taxes", "a. Local", "b. State", "c. Federal", "Profits, Dividends, Rents, and Other", "Total GDP", " ", "IV. Tax Accounts                   Business  Local  Total", "Labor Income", "Taxes", "a. Local", "b. State", "c. Federal", " ","Effects per Million Dollars of Initial Expenditure(in Dollars)", "Employment/Jobs", "Earnings", "State Taxes", "Local Taxes", "GDP"," ", "Initial Expenditures", " ") #, " ", 
  #These names are important for labeling.    
  main4 <- cbind(names, main4)
  colnames(main4)<-c(" ","Output","Earnings","Employment","GDP")
  #Adding categories to main4.
  return (list(main4,super_sector_woHH,main4_woHH ))
}
