source("SuperSector.R")
library("xlsx")
library("readxl")
source("ThreeDigitIndustry.R")
df_gen1a<-0
df_gen3a<-0
df_gen4a<-0
RPC_test<<-0

directEffects<-as.data.frame(matrix(0, nrow = 404, ncol = 3))
I_pAa<-0
A<-0
test_variable<-0
test_variable2<-0

Ma<-0
df_gen2a<-0
totalEffectsa<-0
detailedIndustrya<-0
super_sector_woHHa<-0
three_digit_industry_woHHa<-0
#De-Globalize Later

#Get Rid Later
directEffectsOutput<-0
directEffectsJobs<-0

fund_calc <- function(state,rehab_output,jobs_retail,df_new2,NAICS) {
  M<-0
  M_woHH<-0
  #df_new2 currently probably isn't needed. Will wait for the future reviews. 
  test_variable<-jobs_retail
  df_new2 <- as.data.frame(matrix(0, nrow = 404, ncol = 3)) 
  df_new2[,3]<-rehab_output[,3]
  
  #Jobs, Earnings, Output
  df_new2<-cbind(df_new2,matrix(1,nrow=404,ncol=1))
  test_variable<-df_new2
  df_new2[291,1]<-jobs_retail #Jobs section from main street
  
  #Setting RPCs equal to zero for materials and manufacturing
  #=====================
    df_new2[105,4]<-0
    df_new2[241,4]<-0
    df_new2[248,4]<-0
    df_new2[262,4]<-0
    df_new2[286,4]<-0
    df_new2[40:51,4]<-0
    df_new2[53,4]<-0
    df_new2[54,4]<-0
    df_new2[57,4]<-0
    df_new2[66,4]<-0
    df_new2[67:73,4]<-0
    df_new2[75,4]<-0
    df_new2[77,4]<-0
    df_new2[79:114,4]<-0
    df_new2[116:158,4]<-0
    df_new2[160,4]<-0
    df_new2[162:165,4]<-0
    df_new2[167,4]<-0
    df_new2[170:176,4]<-0
    df_new2[180:184,4]<-0
    df_new2[186:194,4]<-0
    df_new2[198:205,4]<-0
    df_new2[209:220,4]<-0
    df_new2[227:228,4]<-0
    df_new2[232:236,4]<-0
    df_new2[240:270,4]<-0
  
  #For now, the fourth column just added will show that RPC should be used for all. 
  #   #Input$Start is the ID of the "Generate" button.  
  temp<-paste(as.vector(state), "_vectors",".xlsx", sep="") 
  # print("STATES IS GOOD 2")
  #print(c("Temporary file ",temp))
  #temp is the variable that holds the vectors file for whatever state that is selected by the user. 
  temp2<-paste("A_",toupper(as.vector(state)),".xlsx", sep="")
  wb2<-read_excel(file.path(getwd(),state,temp2),sheet=1)
  #temp2 is used For A matrix of given state. That's why the state_variable() function is used. 
  #wb<-loadWorkbook(file.path(getwd(),state_variable(),temp))
  #Where the spreadsheet in the state vectors file will be stored. 
  wb2<-wb2[,2:405]
  #print(state)
  #Prepared wb2 for manipulation. It's 404x404
  A<-as.data.frame(wb2)    
  A_matrix_reac<- reactiveVal(1)
  #Wb2 holds the A matrix spreadsheet for the selected state. 
  wb_1<-read_excel(file.path(getwd(),state_variable,temp),sheet=1)
  #state_vectors_reac<-reactiveVal(1)
  #   #wb_1 is the actual dataframe representation of wb1. 
  #   #Both are used because there was a moment of troubleshooting in the code where I relied on two separate commands because read.xlsx(wb) was giving me trouble
  #   #and i had a deadline to meet. In the next gen of the code, this could be simplified. 
  three_digit_categories<-read_excel(file.path(getwd(),state_variable,"3_digit_industries.xlsx"),sheet=1)
  #   #3_digit_industries.xlsx is a spreadsheet that stores the IO codes for ease of access. 
  CDF <-wb_1[,c(28, 29, 16,31, 30)] 
  #   #CDF stores the conversion ratios like Jobs/Output. This is me reading the values from the spreadsheet into the variable. 
  RPC<-wb_1[,27]#read.xlsx(wb, sheet=1,startRow=1, cols=27) 
  RPC_test<<-RPC
  #   #Reads in RPC values. They have not been used yet. 
  Tax<-matrix(.001, nrow = 404, ncol = 3)
  #   #I made a Tax matrix to fit all the values into. 
  CDF[is.na(CDF)] <- 0
  #   #This code means that any value in the CDF dataframe that is empty, or "NA" will just be converted to 0 to prevent code errors. 
  CDF[1] <- as.numeric(unlist(CDF[1])) 
  CDF[2] <- as.numeric(unlist(CDF[2]))  
  CDF[3] <- as.numeric(unlist(CDF[3]))
  fed_tax <- wb_1[,34]#read.xlsx(wb, sheet=1,startRow=1, cols=34)
  #This reads in federal taxes. 
  state_tax <-wb_1[,35]# read.xlsx(wb, sheet=1,startRow=1, cols=35)
  #   #This reads in state taxes. 
  local_tax <- wb_1[,36]#read.xlsx(wb, sheet=1,startRow=1, cols=36)
  #   #This reads in local taxes. 
  Tax<- cbind(fed_tax,state_tax,local_tax)
  #   #Cbind combines the three tax varibales together into a three column list. 
  CDF[CDF==0]<-1
  #   #This command sees what CDF values are 0 and then turns those into 1. 
   NAICS<-wb_1[,4]#read.xlsx(wb,sheet = 1,startRow = 1,cols = 4,skipEmptyRows = F) 
  #   #Reads in NAICS codes. 
  valueAddedPerOutput<-wb_1[,21]#read.xlsx(wb,sheet = 1,startRow = 1,cols = 21,skipEmptyRows = F) #Check this
  #   #Reads in the Values added per output
  #   print("End")
  #   #Rewrites df_new2, which in future iterations should be simplified. 
  
  RPC_selected<-(df_new2[,4]==1)
  #   RPC_Selected is a boolean that keeps track of which RPC values the user wants to use based on the user inputs on column 5 of the table. 
#  RPC[(RPC_selected),]<-1 
  #This is where we will convert output to jobs, jobs to output, output to earnings, etc. 
  df_new2 <- comparison(df_new2,CDF)
  #   #This command sees what CDF values are not selected and then turns those into 1. That's a default decision that could be reworked later.  
  
  directEffectsOutput<-as.matrix((df_new2[,3]))# RPC*as.matrix((df_new2[,3])) #Output    
  
  #   #Direct Effects Output is obtained by multiplying RPC by the Output. 
  #   #Currently this piece of code has not been tested because RPC has not been used yet. 
  
  directEffectsEarnings<-as.data.frame((df_new2[,2]))# as.data.frame(RPC*(df_new2[,2])) #Earnings 
  ##   #Direct Effects Earnings is obtained by multiplying RPC by the Output. 
  directEffectsJobs<-as.data.frame(df_new2[,1]) #Jobs
  directEffectsJobs[291,1]<-jobs_retail
  #   #Direct Effects Earnings
  directEffectsCategory<-as.data.frame((wb_1[,5])[,1])
 # A<-directEffectsCategory
  colnames( directEffectsCategory)<-c('Categories')
  colnames(directEffectsJobs)<-c('Direct Effects Jobs')
  colnames(directEffectsEarnings)<-'Direct Effects Earnings'
  colnames(directEffectsOutput)<-'Direct Effects Output'
  #colnames updates the names of the list variables. 
  df_new2<-cbind(directEffectsCategory,df_new2)
  #print(c("Dimensions ",dim(df_new2)))
  #   #The new df_new2 is just all the calculated output variables with the proper category labeling. 
  #   #The old dataframe, used to compare the new variables with the old ones every time the "Update" button is pressed, is the new df WITHOUT the cateogries column.
  #   #that's the significance of the -1. 
  colnames(df_new2) <- c("Categories", "Output", " Jobs", "Earnings", paste("RPC\n Y=1, N=0"))
  directEffects<- cbind(directEffectsCategory, directEffectsJobs, directEffectsEarnings, directEffectsOutput)
  #   #Just doing the column names for df_new2
    Tax[1] <- as.numeric(unlist(Tax[1]))
    Tax[2] <- as.numeric(unlist(Tax[2]))
    Tax[3] <- as.numeric(unlist(Tax[3]))
    df_gen1_list<-DetailedIndustry(df_new2, directEffects,directEffectsOutput, directEffectsEarnings, directEffectsJobs, wb2, M, M_woHH,directEffectsCategory, NAICS, CDF,A,RPC)
    directEffects<-as.data.frame(df_gen1_list[1])
    colnames(directEffects)<-c("Categories","Direct Effects Jobs","Direct Effects Earnings","Direct Effects Output")
    # print("2")
    # 
     M<-as.data.frame(df_gen1_list[2])
     M_woHH<-as.data.frame(df_gen1_list[3])
     detailedIndustry<-as.data.frame(df_gen1_list[4])
     print("3")
     totalEffectsJobs1<-as.data.frame(df_gen1_list[5])
     totalEffectsEarnings<-as.data.frame(df_gen1_list[6])
     totalEffectsOutput<-as.data.frame(df_gen1_list[7])
    # print("4")
     directEffectsOutput<-as.data.frame(df_gen1_list[8])
     totalEffectsOutput_woHH<-as.data.frame(df_gen1_list[9])
     GDP<-as.data.frame(df_gen1_list[10])
     GDP_woHH<-as.data.frame(df_gen1_list[11])
     totalEffectsEarnings_woHH<-as.data.frame(df_gen1_list[12])
     totalEffectsJobs1_woHH<-as.data.frame(df_gen1_list[13])
     totalEffectsOutput_woHH<-as.data.frame(df_gen1_list[14])
     A<-as.data.frame(df_gen1_list[15])
     Ipa<-as.data.frame(df_gen1_list[16])
     pA<-as.data.frame(df_gen1_list[17])
     L<-as.data.frame(df_gen1_list[18]) #M
     #Creates Detailed Industry. 
     df_tax2 <- as.data.frame( matrix(0, nrow = 404, ncol = 3))
     taxSheet<-TaxGen(df_tax2,totalEffectsOutput,Tax,category,NAICS)
     df_gen2a<-as.data.frame(taxSheet)
     print("After taxSheet")
     df_superSector2 <- as.data.frame(matrix(0, nrow = 14, ncol = 4))
     taxaccounts <- as.data.frame(matrix(0, nrow = 5, ncol = 4))
     EPMIE <- as.data.frame(matrix(0, nrow = 5, ncol = 4))
     compensationtbl <- as.data.frame(matrix(0, nrow = 7, ncol = 4))

     df_gen3_list<-superSectorGen(df_gen1_list[[7]],Tax,
                           df_superSector2
                           ,df_superSector2_woHH,
                           df_gen1_list[[6]],
                           df_gen1_list[[5]],
                           CDF,
                           totals_woHH,
                           totalDirects_woHH,
                           df_new2,
                           taxaccounts,
                           EPMIE,
                           compensationtbl,GDP,GDP_woHH,totalEffectsJobs1_woHH,totalEffectsEarnings_woHH,totalEffectsOutput_woHH,wb_1,df_tax2)
     main4<-df_gen3_list[1]
     super_sector_woHH<-df_gen3_list[2]
     super_sector_woHHa<-as.data.frame(super_sector_woHH)
     main4_woHH<-df_gen3_list[3]
    # #Super Sector 
     
     #Df_3digit is zeroed here after the table is calculated multiple rounds because df_gen4 keeps making df_3digit much bigger each time. 
     df_3digit <- as.data.frame(matrix(0, nrow = 75, ncol = 4)) #was 75
     three_digit_industry_woHH<-as.data.frame(matrix(0, nrow = 76, ncol = 4)) #was 75
     df_gen4_list<-threeDigitIndustryGen(GDP,totalEffectsJobs1,totalEffectsEarnings,df_3digit,GDP_woHH,totalEffectsJobs1_woHH,totalEffectsEarnings_woHH,totalEffectsOutput_woHH,three_digit_industry_woHH,totalEffectsOutput)
     df_3digit<-df_gen4_list[1]
   # test<-df_3digit
     three_digit_industry_woHH<-df_gen4_list[2]
     three_digit_industry_woHHa<-as.data.frame(three_digit_industry_woHH)
     #df_gen1a<-df_gen1_list[4]
     df_gen1a<-as.data.frame(df_gen1_list[4]) #detailedINdustry
     colnames(df_gen1a)<-c("Industry Description", "NAICS", "Total Output", "Total Jobs","Total Earnings","Total GDP")
     df_gen3a<-as.data.frame(main4)
     #test<-df_gen3a
     df_gen4a<-df_3digit
     df_gen4a <- cbind(three_digit_categories,df_gen4a)
     #df_gen4a[75,3]<- sum(df_gen4a[1:74,3]) #Total, Output
     #df_gen4a[75,4]<- sum(df_gen4a[1:74,4]) #Total Employment
     #df_gen4a[75,5]<- sum(df_gen4a[1:74,5]) #Total Earnings
     #df_gen4a[75,6]<- sum(df_gen4a[1:74,6]) #Total GDP
     is.num <- sapply( df_gen4a, is.numeric)
     df_gen4a[is.num] <- lapply( df_gen4a[is.num], round, 1)
     for(i in 1:dim(df_gen4a)[1]){
       #This loop is meant to make jobs round to the nearest integer. I couldn't get the lapply to work, so I needed to make a loop. 
       if(is.numeric(df_gen4a[i,5])==TRUE)
       {
         df_gen4a[i,5]<-round(df_gen4a[i,5],0)
       }
     }      
   list_of_datasets <- list("Detailed Industry" = df_gen1a, "Super Sectors" = df_gen3a,"3-digit Industry" =df_gen4a,"Direct Effects"=directEffects, "I-pA"=Ipa, "A"=A, "pA"=pA,L)#,#"I-pA"=I_pAa,"M"=Ma,"Taxes" = df_gen2a,"totalEffects"=totalEffectsa,"detailedIndustry"=detailedIndustrya,"Super Sector woHH"=super_sector_woHHa, "3 digit industry woHH"=three_digit_industry_woHHa)##,  "Etc." = ditto) # , "3 digit industry woHH"=three_digit_industry_woHH,
     
  return(list_of_datasets)
}


comparison <- function(df_new2,CDF){
  df_old <- as.data.frame( matrix(0, nrow = 404, ncol = 4))
  #Comparison compares the old table values to the new ones the user puts in. The function goes column by column to identify which column was modified. 
  #The function is reactive because we are using df_gen() which is a reactive variable. Whenever df_gen() changes, comparison will re-run itself. 
  #df_new2 will be either all filled with zeros or with the current values that the user just put in. 
  if (dim(df_new2)[2]>4){
    df_new2<-df_new2[,-1]
  }
  if (all(df_old == df_new2))
    #I believe this old boolean statement was just used for troubleshooting. 
    #I'll get rid of it once everything is up and running perfectly. 
  {
    #Useless boolean, as mentioned. 
  }
  
  if ( any(df_new2[,1]!= df_old[,1]))
  {   
    #In this case, "Jobs" is given. "Earnings" and "Output" must be calculated.
    
    for (i in 1:dim(df_new2)[1])
    { #Now we're going row by row, value by value. 
      if(df_new2[i,1]!=df_old[i,1]){
        #This is a value-by-value operation, NOT a matrix operation. Both matrices must be of the same size.
        df_new2[i, 3] <- round(df_new2[i, 1] / CDF[i, 1], digits=2) 
        #Finding Output. Basically dividing Jobs by Jobs/Output
        df_new2[i, 2] <-round( df_new2[i, 3] * CDF[i, 2], digits=2)
        #Finding Earnings. Same philosophy as the one before. 
      }}
    df_old[,1]<-df_new2[,1]
    #Once I get the new values, I assign them to the old data frame so I can update it. 
    
  }
  
  if (any(df_new2[,2]!= df_old[,2]))
  {     
    #In this case, "Earnings" is given. "Jobs" and "Output" must be calculated.
    
    
    #   #This is a value-by-value operation, NOT a matrix operation. Both matrices must be of the same size.
    for (i in 1:dim(df_new2)[1])
    {
      
      if(df_new2[i,2]!=df_old[i,2])
      {
        df_new2[i, 3] <- round(df_new2[i, 2] / CDF[i,2],digits=2) 
        #Output is found by doing Earnings/ Earnings/Output
        df_new2[i, 1] <- round(df_new2[i, 3] * CDF[i,1],digits=2)
        #Jobs is found by doing Output* Jobs/Output
      }}
    df_old[,2]<-df_new2[,2]
    #Once I get the new values, I assign them to the old data frame so I can update it. 
  } 
  if ( any(df_new2[,3]!= df_old[,3]))
  {
    #In this case, "Output" is given. "Jobs" and "Earnings" must be calculated.
    for (i in 1:dim(df_new2)[1])
    {
      if(df_new2[i,3]!=df_old[i,3]){
        df_new2[i, 2] <- round(df_new2[i, 3] * CDF[i, 2], digits=4)
        #Finding Earnings. Earnings <- Output* Earnings/Output
        df_new2[i, 1] <- round(df_new2[i, 3] * CDF[i, 1], digits=4) 
        #Finding Jobs. Jobs<- Output*Jobs/Output
      }}
    #Once I get the new values, I assign them to the old data frame so I can update it. 
  }

  #This is for the RPC value. It'll automatically toggle between 1 and 0 depending on whether or not the button is toggled. 
  #It has no use as of right now in the code. (RPC is not used.)
  
  #if(input$somevalue==F){
  #  df_new2[,4]<-0
  #  }
  #  else{
  #    df_new2[,4]<-1}
  # defsomevalue<-input$somevalue
  #  }
  #  print("comp 40 ")
  
  return(df_new2)
} 
TaxGen <- function(df_tax2,totalEffectsOutput,Tax,category,NAICS){
  #Creates the taxes. 
  colnames(df_tax2) <-c("Federal", "State", "Local")
  #Tax Calculations are below. Above are the column names. This will be used in SuperSectors for sure. 
  df_tax2[1] <- totalEffectsOutput * Tax[1]
  df_tax2[2] <- totalEffectsOutput * Tax[2]
  df_tax2[3] <- totalEffectsOutput * Tax[3]#Not sure if right
  taxSheet <- cbind(category, NAICS, df_tax2)
  colnames(taxSheet) <-c("Categoryl","NAICS", "Federal", "State", "Local")
  #I am rewriting the taxSheet variable. 
  #The return function below is not necessary. I just want to see the print for troubleshooting purposes. Get rid of return(taxSheet) once everything is 
  #running optimally. 
  return (taxSheet)
  
}

DetailedIndustry <- function(df_new2, directEffects,directEffectsOutput, directEffectsEarnings, directEffectsJobs, wb2, M, M_woHH,directEffectsCategory, NAICS, CDF,A,RPC){
  #Creates Detailed Industry. 
  print("df_gen1d")
  directEffects[,1]<-directEffectsCategory
  directEffects[,2]<-directEffectsJobs
  directEffects[,3]<-directEffectsEarnings
  directEffects[,4]<-directEffectsOutput
  #Created a directEffects matrix
  #Modifying the A matrix to be a square matrix without description, NAICS, RIO
  #Creating the identity matrix
  I<-diag(dim(A)[1])
  pA<-sweep(as.matrix(A),1,as.matrix(RPC),"*") 
  #404x404 p is a vector. row of P is same element. Diag p to ake it 404x404 p%*%A 
  #Sweeps don't work on data frames. M stands for multiplier. 
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1CHANGED I-pA TO I-A!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  I_pA<-I-pA
  

  I_pA_woHH<-I_pA[1:403,1:403]
  I_pA[404,404]<-1
  I_pAa<-as.data.frame(I_pA)
  
  M <- solve(I_pA)
  Ma<-as.data.frame(M)
  M_woHH<-solve(I_pA_woHH)
  #Rewriting the global variables with new, calculated variables. Reminder, woHH stands for without households. It's important to...
  #calculate these values for the later tables in the SuperSector spreadsheet. 

  totalEffectsOutput <- as.matrix(M) %*% as.matrix(directEffectsOutput)
  totalEffectsOutput_woHH<-as.matrix(M_woHH) %*% as.matrix(directEffectsOutput[1:403,1])
  totalEffectsOutput <- round(totalEffectsOutput,1)
  #directEffectsOutput is from 1 to 403 to remove the household values. 
  totalEffectsEarnings <- c(totalEffectsOutput) * (CDF[2])
  totalEffectsEarnings <- as.matrix(totalEffectsEarnings)
  totalEffectsEarnings <- round(totalEffectsEarnings,0)
  totalEffectsEarnings_woHH <-c(totalEffectsOutput_woHH) * (CDF[2])
  totalEffectsJobs1 <- as.matrix((totalEffectsOutput) * (CDF[1]))
  totalEffectsJobs1 <- round(totalEffectsJobs1,0)
  totalEffectsJobs1_woHH<-(totalEffectsOutput_woHH) * (CDF[1])
  GDP <- c(totalEffectsOutput) * (CDF[4])
  GDP <- round(GDP, 1)
  GDP_woHH<-c(totalEffectsOutput_woHH) * (CDF[4])
  #Recalculate all the totalEffects variables with and without households. CDF[4] is GDP/Output, by the way. 
  #print("in function")
  totalEffects <- cbind(totalEffectsOutput,totalEffectsJobs1,totalEffectsEarnings,GDP)
  #Combine the totaleffects variables into a single DF. 
  colnames(totalEffects) <- c("Total Output","Total Jobs","Total Earnings","GDP")
  totalEffectsa<-totalEffects
  #Set column names. 
  ditto <- cbind(category,NAICS,df_new2)
  #Ditto was just a testing variable. Can be removed. Haven't yet because I may need it. 
  detailedIndustry<-cbind(directEffectsCategory,NAICS, totalEffects)
  #DetailedIndustry final DF is with the industry names, the NAICS codes, and totalEffects variables. 
  colnames(detailedIndustry)<-c("Industry Description","NAICS","Total Output", "Total Jobs", "Total Earnings","GDP")
  detailedIndustrya<-detailedIndustry
  #There is no return function currently. The main reason for this is to simplify the calculating process to reduce possible mistakes. 
  #Additionally, there is a strong reliance on global variables. This is just to make the troubleshooting process easy as I get the code to deliver 
  #what it needs to. I believe one issue that may happen as a cosnequence of this is that data will keep being overwritten as people access the server.
  #By that stage, when everything else is working, it will be ideal to maybe make less variables global. 
  #print("end function")
  
  return (list(directEffects, M, M_woHH, detailedIndustry, totalEffectsJobs1,totalEffectsEarnings,totalEffectsOutput,directEffectsOutput,totalEffectsOutput_woHH,GDP,GDP_woHH, totalEffectsEarnings_woHH, totalEffectsJobs1_woHH, totalEffectsOutput_woHH,A,I_pA,pA,M))
}
