source("getStates.R")
source("df_gen1.R")
source("df_gen3.R")
source("df_gen4.R")

#Initializing global variables below. 
#The point of these global variables is twofold. The first is for easy troubleshooting. I can access these from the console. 
#The second is I can have other functions access these variables without having to have return option. That's just preference for now. 
testing<<-0
test<<-0
#Test is meant to store temporary variables. 
initial_exp<<-0
#Initial Expenditures
wb<<-0
#The first workbook will be used to access the Vectors spreadsheet. 
wb2<<-0
#Wb2 will be used to access the A matrix for a specific state. 
M<<-0
#The M matrix
I_pA<<-0
#I-pA matrix
pA<<-0
#the rho*A matrix
taxSheet<<-0
#Stores tax data
I_pA_woHH<<-0
#I-pA for without households
M_woHH<<-0
#M matrix for without households
totalEffectsOutput_woHH<<-0
#Total Effects Output without households
totalEffectsJobs1_woHH<<-0
#Total Effects Jobs without households
totalEffectsEarnings_woHH<<-0
#Total Effects Earnings without households
GDP_woHH<<-0
#Total Effects GDP without households
three_digit_industry_woHH<<-as.data.frame(matrix(0, nrow = 75, ncol = 4))
#three digit industry without households
super_sector_woHH<<-0
#Super Sector without households
df_tax2_woHH<<-as.data.frame( matrix(0, nrow = 404, ncol = 3))
#Will be used later to find Local, State and Federal taxes for the Super Sector page. 
compensation<<-0
#Compensation of GDP
defsomevalue<-F
#defSomeVal is used to determine whether somevalue RPC checkbox has changed. False because that's the checkbox default value. 
totalEffects<<-0
#This variable will hold Total Effects Earnings, Jobs, Output and GDP
directEffects<<-as.data.frame(matrix(0, nrow = 404, ncol = 3))
#This variable will store the Direct Effects. It doesn't matter if I initialzie as a single variable that will
#later be column bound (cbind) or if I just define it as an existing matrix. 

df_superSector2 <<- as.data.frame(matrix(0, nrow = 14, ncol = 4))
#df_superSector2 will be used to calculate the total effects table  in 'Super Sector'
df_effectsDistribution <- as.data.frame(matrix(0, nrow = 5, ncol = 4))
#This will calculate Distribution of Effects and Multipliers in SS
compensationtbl <<- as.data.frame(matrix(0, nrow = 7, ncol = 4))
#Composition of GDP in SS
taxaccounts <- as.data.frame(matrix(0, nrow = 5, ncol = 4))
# Tax Accounts in SS
EPMIE <- as.data.frame(matrix(0, nrow = 5, ncol = 4))
#EPMIE table in SS
df_3digit <<- as.data.frame(matrix(0, nrow = 75, ncol = 4))
#this is the dataframe for three digit industry values. 
df_old <- as.data.frame( matrix(0, nrow = 404, ncol = 4))
#df_old is used to compare values when the table is renewed in the app. The values before "Update" will be saved in df_old. 
#if the new values in df_new are different from df_old, the program knows to update the values when the user clicks the button. 
df_tax2 <<- as.data.frame( matrix(0, nrow = 404, ncol = 3))
#df_tax2 will be used to store the products of output*tax values. 
directEffectsJobs<<-0
#Jobs calculated in Direct Effects table based on the input. 
directEffectsOutput<<-0
#Output calculated in Direct Effects table based on the input. 
directEffectsEarnings<<-0
#Earnings calculated in Direct Effects table based on the input. 
totalEffectsJobs1<<-0
totalEffectsOutput<<-0
totalEffectsEarnings<<-0
#These total effects variables are present in Detailed Industry excel sheet. They are pretty self-explanatory. 
CDF<<-0
#CDF is going to be a 3 column matrix. It's used to CONVERT the input the user puts in to calculate Jobs, Earnings, Output and GDP. 
#More information will be given once CDF value is assigned. But the first column, for example, is Jobs/Output. So given output, 
#I am able to figure out Jobs. 
category<-0
#Inudstry names.
A<<-matrix(0, nrow = 404, ncol = 404)
#The A matrix
GDP<<-0
#GDP values will go into this column. (total effects)
three_digit_categories<<-matrix(0,nrow=75,ncol=2)

ditto<-0


states <<- getStates()


server <- function(input, output){
  df_new2_reactive<- reactiveVal(0)
  state_vectors_reac<-reactiveVal(0)
  
  print("1")
  login = FALSE
  USER <- reactiveValues(login = login)
  print("1.5")
  
  output$sidebarpanel <- renderUI({
    
    print("sidebarPanel")
    if (USER$login == TRUE ){ 
      sidebarMenu(
        div( selectInput(inputId="states",label="", choices=states, selected = NULL, multiple = FALSE, selectize = TRUE, width = '100%', size = NULL), style="font-size: 15px; font-weight:bold; font-family: 'times' ;")  ,
        # div( style="width: 50%;", downloadButton('downloadData', '')),
        div( downloadButton('downloadData', 'Download') )  ,
        
        #div( style="width: 50%; text-align:center", downloadButton(width=2,'downloadData', 'Download')),
        menuItem("Direct Effects", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("A Matrix", tabName = "second", icon = icon("th")),
        menuItem("State Vectors", tabName = "third", icon = icon("th"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        
        # First tab
        tabItem(tabName ="dashboard", class = "active",
                fluidRow(
                  box(width = 12, rHandsontableOutput('tblz')
                  )
                )),
        
        # Second tab
        tabItem(tabName = "second",
                fluidRow(
                  box(width = 12, rHandsontableOutput('A_Matrix'))
                )),
        tabItem(tabName = "third",
                fluidRow(
                  box(width = 12, rHandsontableOutput('State_Vectors'))
                ))
        
        
      )
      
    }
    else {
      loginpage
    }
  })
  
  output$A_Matrix <-  renderRHandsontable({
    
    #temp<-df_new2_reactive()
    #  print("BEG 2")
    # if(all(df_new2[,2:4]==0)){
    #  print("BEG 3")
    temp<-A_matrix_reac()
    
    
    rhandsontable(A)
    #}
    #RenderRHandsontable makes the table.
    # else{
    # print("REpeate")
    # print("We're in the blz")
    #rhandsontable(temp)}
    
    
  })
  
  output$State_Vectors <-  renderRHandsontable({
    
    #temp<-df_new2_reactive()
    #  print("BEG 2")
    # if(all(df_new2[,2:4]==0)){
    #  print("BEG 3")
    temp<-state_vectors_reac
    
    
    rhandsontable(wb_1)
    
  })
  
  #DELETE: Making the handsontable. 
  output$tblz <-  renderRHandsontable({
    print("THE BEGINNING")
    #This code gets run from the very beginning because df_new2_reactive() is defined up top. However, it doesn't go anywhere just yet. 
    #The reason why temp is defined is because renderRhandsontable and rhandsontable() does not register changes in the reactive df_new2_reactive variable
    #unless it is is USED to define new variable in this function
    temp<-df_new2_reactive()
    print("BEG 2")
    if(all(df_new2[,2:4]==0)){
      print("BEG 3")
      
      rhandsontable(df_new2)
    }
    #RenderRHandsontable makes the table.
    else{
      # print("REpeate")
      print("We're in the blz")
      rhandsontable(temp)}
    
    
  })
 

  comparison <- reactive({
    print("comp 1")
    #Comparison compares the old table values to the new ones the user puts in. The function goes column by column to identify which column was modified. 
    df_new<-df_gen()
    print("comp 2")
    
    #The function is reactive because we are using df_gen() which is a reactive variable. Whenever df_gen() changes, comparison will re-run itself. 
    #df_new will be either all filled with zeros or with the current values that the user just put in. 
    if (dim(df_new)[2]>4){
      df_new<-df_new[,-1]
    }
    if (all(df_old == df_new))
      #I believe this old boolean statement was just used for troubleshooting. 
      #I'll get rid of it once everything is up and running perfectly. 
    {
      print("point 2")
      #Useless boolean, as mentioned. 
    }
    
    if ( any(df_new[,1]!= df_old[,1]))
    {   
      #In this case, "Jobs" is given. "Earnings" and "Output" must be calculated.
      
      for (i in 1:dim(df_new)[1])
      { #Now we're going row by row, value by value. 
        if(df_new[i,1]!=df_old[i,1]){
          #This is a value-by-value operation, NOT a matrix operation. Both matrices must be of the same size.
          df_new[i, 3] <- round(df_new[i, 1] / CDF[i, 1], digits=2) 
          #Finding Output. Basically dividing Jobs by Jobs/Output
          df_new[i, 2] <-round( df_new[i, 3] * CDF[i, 2], digits=2)
          #Finding Earnings. Same philosophy as the one before. 
        }}
      df_old[,1]<<-df_new[,1]
      #Once I get the new values, I assign them to the old data frame so I can update it. 
      
    }
    
    if (any(df_new[,2]!= df_old[,2]))
    {     
      #In this case, "Earnings" is given. "Jobs" and "Output" must be calculated.
      
      
      #   #This is a value-by-value operation, NOT a matrix operation. Both matrices must be of the same size.
      for (i in 1:dim(df_new)[1])
      {
        
        if(df_new[i,2]!=df_old[i,2])
        {
          df_new[i, 3] <- round(df_new[i, 2] / CDF[i,2],digits=2) 
          #Output is found by doing Earnings/ Earnings/Output
          df_new[i, 1] <- round(df_new[i, 3] * CDF[i,1],digits=2)
          #Jobs is found by doing Output* Jobs/Output
        }}
      df_old[,2]<-df_new[,2]
      #Once I get the new values, I assign them to the old data frame so I can update it. 
      
    } 
    
    
    if ( any(df_new[,3]!= df_old[,3]))
    {
      #In this case, "Output" is given. "Jobs" and "Earnings" must be calculated.
      for (i in 1:dim(df_new)[1])
      {
        if(df_new[i,3]!=df_old[i,3]){
          df_new[i, 2] <- round(df_new[i, 3] * CDF[i, 2], digits=2)
          #Finding Earnings. Earnings <- Output* Earnings/Output
          df_new[i, 1] <- round(df_new[i, 3] * CDF[i, 1], digits=2) 
          #Finding Jobs. Jobs<- Output*Jobs/Output
          
        }}
      df_old[,3]<-df_new[,3]
      #Once I get the new values, I assign them to the old data frame so I can update it. 
      print("comp 3")
      
      
    }
    print("comp 3.5")
    
    # if(input$somevalue != defsomevalue){
    #    print("comp 4")
    
    #This is for the RPC value. It'll automatically toggle between 1 and 0 depending on whether or not the button is toggled. 
    #It has no use as of right now in the code. (RPC is not used.)
    
    #if(input$somevalue==F){
    #  df_new[,4]<-0
    #  }
    #  else{
    #    df_new[,4]<-1}
    # defsomevalue<<-input$somevalue
    #  }
    #  print("comp 40 ")
    
    return(df_new)
  }) 
  
  
  #df_new2_reactive<-eventReactive(input$tbl$changes$changes,{
  
  
  
  
  
  
  df_gen2 <- function(){
    #Creates the taxes. 
    colnames(df_tax2) <<- c("Federal", "State", "Local")
    #Tax Calculations are below. Above are the column names. This will be used in SuperSectors for sure. 
    df_tax2[1] <<- totalEffectsOutput * Tax[1]
    df_tax2[2] <<- totalEffectsOutput * Tax[2]
    df_tax2[3] <<- totalEffectsOutput * Tax[3]#Not sure if right
    taxSheet <<- cbind(category, NAICS, df_tax2)
    #I am rewriting the taxSheet variable. 
    #The return function below is not necessary. I just want to see the print for troubleshooting purposes. Get rid of return(taxSheet) once everything is 
    #running optimally. 
    return (taxSheet)
    
  }
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
      #The name just shows the date. I should change the name in the near future. 
    },
    
    content = function(file) {print("A")
      df_gen1<-detailedIndustry
      is.num <- sapply( df_gen1, is.numeric)
      df_gen1[is.num] <- lapply( df_gen1[is.num], round, 1)
      for(i in 1:dim(df_gen1)[1]){df_gen1[i,5]=round(df_gen1[i,5],0)}
      #The above code is essentially used to do rounding to 1 decimal and 0 if its jobs. 
      print("B")
      
      df_gen3<-as.data.frame(main4)
      print("B.1")
      
      df_gen3[,2:5]<-lapply(df_gen3[,2:5],as.numeric) 
      #The above code makes the variables numbers. 
      print("B.2")
      
      test<<-df_gen3
      is.num <- sapply( df_gen3, is.numeric)
      print("B.3")
      
      df_gen3[is.num] <- lapply( df_gen3[is.num], round, 1)
      #Rounds to one decimal place. 
      print("C")
      
      for(i in 1:dim(df_gen3)[1]){
        #This loop is meant to make jobs round to the nearest integer. I couldn't get the lapply to work, so I needed to make a loop. 
        if(is.numeric(df_gen3[i,4])==TRUE)
        {
          df_gen3[i,4]<-round(df_gen3[i,4],0)
        }
      }
      
      df_gen4<-df_3digit
      df_gen4 <- cbind(three_digit_categories,df_gen4)
      is.num <- sapply( df_gen4, is.numeric)
      df_gen4[is.num] <- lapply( df_gen4[is.num], round, 1)
      
      for(i in 1:dim(df_gen4)[1]){
        #This loop is meant to make jobs round to the nearest integer. I couldn't get the lapply to work, so I needed to make a loop. 
        
        if(is.numeric(df_gen4[i,5])==TRUE)
        {
          df_gen4[i,5]<-round(df_gen4[i,5],0)
        }
      }      
      
      list_of_datasets <- list("Detailed Industry" = df_gen1, "Super Sectors" = df_gen3,"3-digit Industry" =df_gen4 ,"I-pA"=I_pA,"L"=M,  "Etc." = ditto) #"Taxes" = df_gen2, "test"=totalEffects,"test2"=detailedIndustry, "Super Sector woHH"=super_sector_woHH, "3 digit industry woHH"=three_digit_industry_woHH,
      #compiling all the tables together into a list. 
      write.xlsx(list_of_datasets, file)
      #writing all the tables into a file. 
      
    })
  comparison_A<- reactive({
    print("comp 1")
    #Comparison compares the old table values to the new ones the user puts in. The function goes column by column to identify which column was modified. 
    new_A<-df_gen_A()
    print("comp 2")
    
    #The function is reactive because we are using df_gen() which is a reactive variable. Whenever df_gen() changes, comparison will re-run itself. 
    #df_new will be either all filled with zeros or with the current values that the user just put in. 
    if (all(new_A == A))
      #I believe this old boolean statement was just used for troubleshooting. 
      #I'll get rid of it once everything is up and running perfectly. 
    {
      print("point 2")
      #Useless boolean, as mentioned. 
    }
    
    if ( any(new_A[,1]!= A[,1])||any(new_A[,2]!= A[,2]) )
    {   
      A<<-new_A
      
    }
    
    # if(input$somevalue != defsomevalue){
    #    print("comp 4")
    
    #This is for the RPC value. It'll automatically toggle between 1 and 0 depending on whether or not the button is toggled. 
    #It has no use as of right now in the code. (RPC is not used.)
    
    #if(input$somevalue==F){
    #  df_new[,4]<-0
    #  }
    #  else{
    #    df_new[,4]<-1}
    # defsomevalue<<-input$somevalue
    #  }
    #  print("comp 40 ")
    
    return(df_new)
  }) 
  
}



