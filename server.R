library(shiny)
library(shinydashboard)
library(shinyjs)
library(sodium)
library(DT)
library(rhandsontable)
library(shinyBS)
library(shinyWidgets)
library(writexl)
library(shinytitle)
library(xlsx)


source("main_street.R")
source("getStates.R")
source("calculations.R")
source("rehab.R")
source("calculations.R")
source("rehab_radio_tables.R")
source("tourism.R")


options(java.parameters = "-Xmx8000m")

test<<-0
test_variable_main<<-0
#Delete test once I'm done. It's a troubleshooting variable
states <- getStates()
m<-get_m1_rehab()
dtWithRadioButton <- reactiveValues(dt = m)
m2<-get_m1_rehab2()
dtWithRadioButton2 <- reactiveValues(dt = m2)
m_mf<-get_m1_rehab()
dtWithRadioButton_mf<-reactiveValues(dt = m_mf)
m2_mf<-get_m1_rehab2()
dtWithRadioButton2_mf<-reactiveValues(dt = m2_mf)
m_com<-get_m1_rehab()
dtWithRadioButton_com<-reactiveValues(dt = m_com)
m2_com<-get_m1_rehab2()
dtWithRadioButton2_com<-reactiveValues(dt = m2_com)
m_civ<-get_m1_rehab()
dtWithRadioButton_com<-reactiveValues(dt = m_civ)
m2_civ<-get_m1_rehab2()
dtWithRadioButton2_civ<-reactiveValues(dt = m2_civ)


server <- function(input, output, session) {

  
#=================================================================================================================================
  #Scoping of Vars.
  print("A")
  wb2<-0
  #De-vlobalize after
  heritageValues<-0
  rehab_value<-0

  #Arbtirary Default Values
  rehab_output<-2
  total_output<-0
  

  detailed_industry<-as.data.frame(matrix(0, nrow = 404, ncol = 3))
  df_heritageb<- reactiveVal(as.data.frame(matrix(0, nrow = 1, ncol = 2)))
  df_heritagea<- reactiveVal(as.data.frame(matrix(0, nrow = 3, ncol = 2)))
  df_heritagec<- reactiveVal(as.data.frame(matrix(0, nrow = 1, ncol = 1)))
  df_constructionA<-reactiveVal(data.frame(cur=c(0,0,0,0)))
  
  df_jobsA<- reactiveVal(as.data.frame(matrix(0, nrow=1,ncol=1)))
  df_jobsB<-reactiveVal(as.data.frame(matrix(0, nrow=1,ncol=1)))
  df_spending<-reactiveVal(as.data.frame(matrix(0, nrow = 3, ncol = 1)))
  
  #Table 6 / historic building rehabilitation Single Fam
  df_table6<- reactiveVal(as.data.frame(matrix(0, nrow = 18, ncol = 1)))
  df_table7<- reactiveVal(as.data.frame(matrix(0, nrow = 10, ncol = 1)))
  df_table8<- reactiveVal(as.data.frame(matrix(0, nrow = 15, ncol = 1)))
  #df_table9<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  
  #Multi Family
  df_table6_mf<- reactiveVal(as.data.frame(matrix(0, nrow = 18, ncol = 1)))
  #df_table7_mf<<- reactiveVal(as.data.frame(matrix(0, nrow = 10, ncol = 1)))
  df_table8_mf<- reactiveVal(as.data.frame(matrix(0, nrow = 15, ncol = 1)))
  #df_table9_mf<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  #Commercial
  df_table6_com<- reactiveVal(as.data.frame(matrix(0, nrow = 18, ncol = 1)))
 # df_table7_com<<- reactiveVal(as.data.frame(matrix(0, nrow = 10, ncol = 1)))
  df_table8_com<- reactiveVal(as.data.frame(matrix(0, nrow = 15, ncol = 1)))
  #df_table9_com<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  #Civic
  df_table6_civ<- reactiveVal(as.data.frame(matrix(0, nrow = 18, ncol = 1)))
 # df_table7_civ<<- reactiveVal(as.data.frame(matrix(0, nrow = 10, ncol = 1)))
  df_table8_civ<- reactiveVal(as.data.frame(matrix(0, nrow = 15, ncol = 1)))
  #df_table9_civ<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  df_new2 <- as.data.frame(matrix(0, nrow = 404, ncol = 4)) 
  A_matrix_reac<- reactiveVal(0)

 

#====================================================================================================================================
  
  observeEvent(input$states,{
    
    
  })
  
  initial_detailed_industry <- function(state_variable="AL", CDF=0) {
    #Input$Start is the ID of the "Generate" button.  
    wb2<-read_excel(file.path(getwd(),"recon_info.xlsx"))#,rowIndex=3:408, colIndex=1:2)
    wb2<-wb2[-c(1,2,3),]
    wb2<-wb2[1:404,]
    rehab_output<-cbind(sapply(wb2[,1],as.numeric),wb2[,2],0)
    colnames(rehab_output)<-c("REcon","Industries","Output")
    return(rehab_output)
  }
  
  heritageCalculator<-function(rehab_output,state, df_heritagea,df_heritageb,df_heritagec)
  {

    #Does all the heritage calculations. 
#    if(input$checkbox2==1&&input$checkbox1==1){
##      df_heritageb<-as.matrix(df_heritageb)
#      df_heritagea<-as.matrix(df_heritagea)
##      df_heritagec<-as.data.frame(t(as.matrix(c(df_heritagec[1,1],0))))
      
#      print(df_heritagec)
#      names<-colnames(df_heritageb)
#      colnames(df_heritagea)<-names
#      colnames(df_heritagec)<-names
      
      #I just added an empty value to the third heritage table for total person night
#      heritage_total<-rbind(df_heritageb, df_heritagea, df_heritagec)
#      value1<- heritage_total[3,1]*heritage_total[3,2]*df_heritagec[1]
#      value2<- heritage_total[2,1]*heritage_total[4,2]*df_heritagec[1]
  #    print("WOW MYKE")
#      value3<- heritage_total[4,1]*heritage_total[4,2]*df_heritagec[1]
#      value4<-df_heritageb[1,1]*df_heritageb[1,2]
      #Add day activities
      
#    }
    if (input$checkbox2==1){
      #Day Trips
      heritageDay = df_heritageb()[1,1]*df_heritageb()[1,2]
      day_wb<-read_excel(file.path(getwd(), state,"Tourism_Info.xlsx"),sheet=2)
      rehab_output<-tourismCalculator(as.data.frame(day_wb),rehab_output,heritageDay)
    }
    
    if (input$checkbox1==1){
      #Tourism Values 
      heritageFriends<-df_heritagea()[3,1]*df_heritagea()[3,2]*df_heritagec()[1,1]
      tourism_wb<-read_excel(file.path(getwd(), state,"Tourism_Info.xlsx"),sheet=1)
      rehab_output<-tourismCalculator(as.data.frame(tourism_wb),rehab_output,heritageFriends)
      
      heritageCamping<-df_heritagea()[1,1]*df_heritagea()[1,2]*df_heritagec()[1,1]
      camping_wb<-read_excel(file.path(getwd(), state,"Tourism_Info.xlsx"),sheet=3)
      rehab_output<-tourismCalculator(as.data.frame(camping_wb),rehab_output,heritageCamping)
      
      heritageLodging<-df_heritagea()[2,1]*df_heritagea()[2,2]*df_heritagec()[1,1]
      lodging_wb<-read_excel(file.path(getwd(), state,"Tourism_Info.xlsx"),sheet=4)
      rehab_output<-tourismCalculator(as.data.frame(lodging_wb),rehab_output,heritageLodging)

    }
    
    return(rehab_output)
    
  }
  
  manipulator<-0
  user_input<-0
  
  spending_bool=FALSE
  construction_bool=FALSE
  jobs_bool=FALSE
  heritage_bool=FALSE
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        print("A")
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }
  })
  
  
  
  
  
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            #style="display:inline-block;;background-color: #eee !important; border: 0;font-weight: bold; margin:5px; padding: 10px;"
            style = "background-color: #eee !important; border: 0;font-weight: bold;padding: 6px;" #margin:5px; padding: 10px;"
    )
  })
  
  output$downloadbtn <- renderUI({
    print("download button")
    req(USER$login)
    #div(style="display:inline-block;",
    downloadButton('downloadData', 'Tables of Impacts')
    #  filename = function() {
    #    paste("data-", Sys.Date(), ".xlsx", sep="")
    #The name just shows the date. I should change the name in the near future. 
    #}
    # div( downloadButton('downloadData', 'Download'),style = "margin-bottom:-35px;margin-left:-200px;color:black" )
  })
  
  output$downloadData <- downloadHandler( 
    
    filename = function() {
    paste("data-", Sys.Date(), ".xlsx", sep="")}
    ,
    #The name just shows the date. I should change the name in the near future. 
    content = function(file) {
      state=state_variable()
      rehab_output<-initial_detailed_industry(state_variable=state)
      #Initializing all the tables ========================================================
      table6<-df_table6()
      colnames(table6)<-"A"
      rownames(table6)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
      table7<-df_table7()
      colnames(table7)<-"A"
      #rownames(table7)<-c("Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
      combined_table6<-table6 #Currently done because before combined_table6 was an rbind of table6 and table 7. I used this assignment to keep things as constant as possible while doing the modification. 
      table8<-df_table8()
      colnames(table8)<-"A"
      rownames(table8)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      #print("Single Family D")
      #table9<-df_table9()
      #colnames(table9)<-"A"
      #rownames(table9)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      #combined_table8<-table8#rbind(table8,table9)
      table6_mf<-df_table6_mf()
      rownames(table6_mf)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
      combined_table6_mf<-table6_mf#rbind(table6_mf,table7_mf)
      # table7_mf<-df_table7_mf()
     # table8_mf<-df_table8_mf()
      #rownames(table8_mf)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
       #table9_mf<-df_table9_mf()
      #  rownames(table9_mf)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      #combined_table8_mf<-table8_mf#rbind(table8_mf,table9_mf)
      table6_com<-df_table6_com()
      rownames(table6_com)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
      combined_table6_com<-table6_com#rbind(table6_com,table7_com)
      #rownames(table6_com)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
      #table7_com<-df_table7_com()
      #table8_com<-df_table8_com()
      #rownames(table8_com)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      #table9_com<-df_table9_com()
      # print("Table 9 _com")
      #rownames(table9_com)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      #combined_table8_com<-table8_com#rbind(table8_com,table9_com)
      table6_civ<-df_table6_civ()
      rownames(table6_civ)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
      combined_table6_civ<-table6_civ#rbind(table6_civ,table7_civ)
      #table7_civ<-df_table7_civ()
      #table8_civ<-df_table8_civ()
      #rownames(table8_civ)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      #table9_civ<-df_table9_civ()
      #rownames(table9_civ)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      #combined_table8_civ<-table8_civ#rbind(table8_civ,table9_civ)
      withProgress(message = 'Calculating...', value = 0, {
        # Number of times we'll go through the loop
        j<-7
        n <- 1
        # for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        # Increment the progress bar, and update the detail text.
        incProgress(1/j, detail = paste("Doing part", n))
        #This is to give each output df a template with industry names. WIthout this template, glitches appeared. 
         blank_rehab = rehab_output
         rehab_output<-rehab_calculations(blank_rehab,combined_table6, j=11, sheetIndex=1 , input$single_amount )
      
         addition = rehab_calculations(blank_rehab,combined_table6_mf, j=21, sheetIndex=2, input$multi_amount )[,3]
         rehab_output[,3]<-rehab_output[,3]+ addition
         addition = rehab_calculations(blank_rehab,combined_table6_com, j=31, sheetIndex=3,input$comm_amount )[,3]
         rehab_output[,3]<-rehab_output[,3]+addition
         addition = rehab_calculations(blank_rehab,combined_table6_civ, j=41, sheetIndex=4,input$civil_amount )[,3]
         rehab_output[,3]<-rehab_output[,3]+addition
         
         
         
         #rehab_calculations(rehab_output,combined_table6,11)
      #   rehab_output<-rehab_calculations(rehab_output,combined_table6, j=12, sheetIndex=1 )
      #  rehab_output<-rehab_calculations(rehab_output,combined_table6mf, j=11, sheetIndex=1 )#rehab_calculations(rehab_output,combined_table6,11)
      #  rehab_output<-rehab_calculations(rehab_output,combined_table6civ, j=11, sheetIndex=1 )#rehab_calculations(rehab_output,combined_table6,11)
      #  rehab_output<-rehab_calculations(rehab_output,combined_table6com, j=11, sheetIndex=1 )#rehab_calculations(rehab_output,combined_table6,11)
        
         #REVIEW THIS M:
       # rehab_output<-rehab_calculations(rehab_output,table8, j=12, sheetIndex=1 )#rehab_calculations(rehab_output,combined_table6,11)
      #  rehab_output<-rehab_output+rehab_output_p2
        
        #11 refers to the 17 value table on first table
        n <- 2
        incProgress(1/j, detail = paste("Doing part", n))
        #rehab_output[,3]<-rehab_output[,3]*rehab_value
    #    rehab_output_mf<-rehab_calculations(rehab_output_template,combined_table6_mf, j=21,sheetIndex=2)[,3]*strtoi(input$multi_amount)
        n <- 3
        incProgress(1/j, detail = paste("Doing part", n))
        #rehab_output_com<-rehab_calculations(rehab_output_template,combined_table6_com,j=31,sheetIndex=3)[,3]*strtoi(input$comm_amount)
        #rehab_output_civil<-rehab_calculations(rehab_output,combined_table6_civ, j=41,sheetIndex=4)[,3]*strtoi(input$civil_amount)#Labor percents will be ignored.
        #rehab_output[,3]<-rehab_output[,3]+rehab_output_mf+rehab_output_com+rehab_output_civil
        n <- 4
        incProgress(1/j, detail = paste("Doing part", n))
        #Transferring rehab_output into total_output
        rehab_output[,3]<-as.numeric(rehab_output[,3])
        #total_output[,3]<<- as.numeric(rehab_output[,3])
        # #   #Part 1 Main Street
        df_main_street_one<-df_constructionA()
        n <- 5
        incProgress(1/j, detail = paste("Doing part", n))
        rehab_output[27,3]<-rehab_output[27,3]+as.numeric(df_main_street_one[1,1])#/1000 #Rehab   #Nonresidential Maintenance
        rehab_output[29,3]<- rehab_output[29,3]+as.numeric(df_main_street_one[2,1])#/1000 #Office and Comm
        rehab_output[33,3]<- rehab_output[33,3]+.5*as.numeric(df_main_street_one[3,1])#/1000 #Other non res
        rehab_output[36,3]<- rehab_output[36,3]+.5*as.numeric(df_main_street_one[3,1])#/1000 #Transportation struc
        
        #! For continuous jobs, all other retail !
        
        incProgress(1/j, detail = paste("Doing part", n))
        #   #Part 3 Tourism CHECK ALL OF THIS ALLOCATION
        rehab_output<-heritageCalculator(rehab_output,state,df_heritagea=df_heritagea(),df_heritageb=df_heritageb(),df_heritagec=df_heritagec())
        #rehab_output[365,3]<-heritageValues[1] ## df_heritage<<- reactiveVal(as.data.frame(matrix(0, nrow = 5, ncol = 2)))
        #rehab_output[364,3]<-heritageValues[2] #hoteling, retail, 
        #rehab_output[394,3]<-heritageValues[3]#I attempted to pick the Household Sector
        #rehab_output[395,3]<-heritageValues[4]#Arbitrary pick
        
        incProgress(1/j, detail = paste("Doing part", n))
        #   #Part 4 Historic Museums DOUBLE CHECK
        rehab_output[375,3]<-df_spending()[1,1]
        rehab_output[28,3]<-df_spending()[2,1]
        n <- 6
        #Tourism and Museum Relationship
        if(input$checkbox2 || input$checkbox1 || input$checkbox1&&input$checkbox2 )
        {

          rehab_output[375,3] = rehab_output[375,3]-.9*df_spending()[3,1] #visitor's revenue generated
          rehab_output[291,3] = rehab_output[375,3]-.1*df_spending()[3,1] #visitor's revenue generated
          if (rehab_output[375,3] <0)
          {
            rehab_output[375,3] = 0
          }
          if (rehab_output[291,3] <0)
          {
            rehab_output[291,3] = 0
          }
          
        }

        
        jobs_retail<-df_jobsA()[1,1] #This variable is the only Jobs variable reported. So it will go into the Jobs column. 
        list_of_datasets<-fund_calc(state,rehab_output,jobs_retail,df_new2,NAICS)
        n <- 7
         })
      wb<-createWorkbook()
      addWorksheet(wb, sheetName = "Super Sectors")
      addWorksheet(wb, sheetName="3-Digit Industry")
      addWorksheet(wb, sheetName = "Detailed Industry")
      addWorksheet(wb, sheetName="Direct Effects")
      #addWorksheet(wb, sheetName="I-pA")
      #addWorksheet(wb, sheetName="A")
      #addWorksheet(wb, sheetName="pA")
      #addWorksheet(wb, sheetName="L")
      
      directEffectsTotals=data.frame("Totals",colSums(list_of_datasets[[4]][2]),colSums(list_of_datasets[[4]][3]),colSums(list_of_datasets[[4]][4]))
      colnames(directEffectsTotals)<-c("Categories","Direct Effects Jobs","Direct Effects Earnings","Direct Effects Output")
      directEffects = rbind(list_of_datasets[[4]],directEffectsTotals)
      
      hs1 <- createStyle(halign = "CENTER", textDecoration = "Bold",border = "Bottom")
      hs2 <- createStyle(textDecoration = "Bold")
      writeData(wb,"Detailed Industry",(list_of_datasets[[1]]),borders="columns",headerStyle=hs1)
      writeData(wb,"Super Sectors",list_of_datasets[[2]],borders="columns",headerStyle=hs1)
      writeData(wb,"3-Digit Industry",(list_of_datasets[[3]]),borders="columns",headerStyle=hs1,)
      writeData(wb,"Direct Effects",directEffects,borders="columns",headerStyle=hs1,)
    #  writeData(wb,"I-pA",(list_of_datasets[[5]]))
    #  writeData(wb,"A",(list_of_datasets[[6]]))
    #  writeData(wb,"pA",(list_of_datasets[[7]]))
     # writeData(wb,"L",(list_of_datasets[[8]]))
      
      
      #Formatting ===============================
      addStyle(wb,2,hs2,rows=18,cols=1)
      setColWidths(wb, 1, cols = 1, widths = 55) #Detailed Industry
      setColWidths(wb, 2, cols = 1, widths = 55) #SS
      addStyle(wb,2,hs2,rows=2,cols=1) #Composition of GDP
      addStyle(wb,2,hs2,rows=19,cols=1) #Composition of GDP
      addStyle(wb,2,hs2,rows=26,cols=1) #Composition of GDP
      addStyle(wb,2,hs2,rows=35,cols=1) #Tax Accounts
      addStyle(wb,2,hs2,rows=42,cols=1) #EPMIE
      addStyle(wb,2,hs2,rows=49,cols=1) 
      options(openxlsx.numFmt = NULL)
      hs3<-createStyle(numFmt = "0")
      addStyle(wb, 2, hs3, rows = 7:10, cols = 3)
      addStyle(wb,3,hs2,rows=2,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=5,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=9,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=10,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=11,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=31,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=32,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=33,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=42,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=47,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=52,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=55,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=60,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=63,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=64,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=68,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=71,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=74,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=75,cols=2:6) #Direct Effects
      addStyle(wb,3,hs2,rows=76,cols=2:6) #Direct Effects
      setColWidths(wb, 3, cols = 2, widths = 55) #3 Digit Ind.
      setColWidths(wb, 4, cols = 1, widths = 55) #Direct Effects
      setColWidths(wb, 4, cols = 2, widths = 35) #Direct Effects
      setColWidths(wb, 4, cols = 3, widths = 35) #Direct Effects
      setColWidths(wb, 4, cols = 4, widths = 35) #Direct Effects
      
      
      saveWorkbook(wb, file=file,overwrite=TRUE)
    }
  )
  
  output$dropdown<- renderUI({
    req(USER$login)
    selectInput("variable", NULL,
                c("Main Street" = "cyl",
                  "Historic Building Rehabilitation" = "am",
                  "Heritage Tourism" = "gear",
                  "Historic Museum and Sites"="ah"
                )
    )
  })
  
  output$states<- renderUI({
    req(USER$login)
    selectInput("states", NULL,
                states,selected = NULL, multiple = FALSE, selectize = TRUE, width = '180%', size = NULL
    )
  })
  output$regions<- renderUI({
    req(USER$login)
    selectInput("variable3", NULL,
                c("Single Region" = "cyl2",
                  "Multiple Regions" = "am2"
                ),selected = NULL, multiple = FALSE, selectize = TRUE, width = '100%', size = NULL
    )
  })
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      shinyjs::show(id = "sidebarpanel")
      
      sidebarMenu(id="tabs",
                  menuItem("The Main Menu", tabName = "first", icon = icon("list-alt")),
                  menuItem("Main Street", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Historic Building Rehabilitation", tabName = "second", icon = icon("clock-o")),
                  menuItem("Heritage Tourism", tabName = "third", icon = icon("th")),
                  menuItem("Historic Museum and Sites", tabName = "fourth", icon = icon("calendar"))
                  
                  
      )
    }
  })
  
  
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
        tabItems(
        # Main Street
        tabItem(tabName ="first", class = "active",
                column(12,offset=2,
                       fluidRow(
                         tags$style(HTML("
                .tooltip > .tooltip-inner {
                width: 400px;
                color: white;
                font-size:20px;
                background-color: black;
                }
                ")),
                         actionButton(inputId = "main_street", label = NULL, style = "width: 450px; height: 250px;
                      background: url('street2.jpg');  background-size: cover; background-position: center;"),  
                         bsTooltip("main_street", "Main Street",placement = "bottom", trigger = "hover",options = NULL),                
                         
                         actionButton(inputId = "rehab_button", label = NULL, style = "width: 450px; height: 250px;
                      background: url('rehab1.jpg');  background-size: cover; background-position: center;"),
                         bsTooltip("rehab_button", "Historic Building Rehabilitation",placement = "bottom", trigger = "hover",options = NULL)
                         
                       ), 
                    fluidRow(
                         actionButton(inputId = "heritage_button", label = NULL, style = "width: 450px; height: 250px;
                      background: url('museum2.jpg');  background-size: cover; background-position: center;") , 
                         bsTooltip("heritage_button", "Heritage Tourism",placement = "bottom", trigger = "hover",options = NULL)
                         ,
                         
                         #, align = "right") #heritage tourism
                         actionButton(inputId = "historic_button", label = NULL, style = "width: 450px; height: 250px;
                      background: url('tourism2.jpg');  background-size: cover; background-position: center;"),
                         bsTooltip("historic_button", "Historic Museums and Sites",placement = "bottom", trigger = "hover",options = NULL)
                       )#, align = "right")#museum and sites
                )
        ),
        
        tabItem(tabName ="dashboard", #class = "active",
                fluidRow(
                  column(10, offset=0, box(title="Main Street", status="primary",collapsible=T, width = "auto", height = "auto",solidHeader=T, 
                                           column(12,
                                                  renderText("Please enter outputs as though they were in 1000s.\n"),
                                                  br(),
                                                  rHandsontableOutput('mainStreetOne'),
                                                  rHandsontableOutput('mainStreetTwo'),
                                                  rHandsontableOutput('mainStreetThree'),
                                                  br(),
                                                  actionButton("zeroButtonMain", "Zero Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                           )) )
                  
                )),
        
        tabItem(tabName = "second",
                fluidRow(
                  
                  
                  
                  
                  
                  column(12,offset=0, box(title="Historic Building Rehabilitation", status="primary",collapsible=T, width = 8,solidHeader=T, tabsetPanel(type = "tabs",
                                                                                                                                                         
                                                                                                                                                         tabPanel("Single Family Home", 
                                                                                                                                                                  fluidRow(column(6,dataTableOutput("TableWithRadio")),column(6,linebreaks(1),dataTableOutput("TableWithRadio2"))),linebreaks(1),
                                                                                                                                                                  fluidRow(column(5, offset=1, rHandsontableOutput('table6'),linebreaks(1))), 
                                                                                                                                                                        #   column(6,rHandsontableOutput('table8'))),
                                                                                                                                                                  linebreaks(2),
                                                                                                                                                                 fluidRow(column(5,offset=1,actionButton("button", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                                                                                                                                                                 #,column(6,rHandsontableOutput('table9'),linebreaks(1),actionButton("button2", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                                                                                                                                                                  linebreaks(1),column(6,offset=1,textInput("single_amount", "Total Amount",value="0"))
                                                                                                                                                         ),
                                                                                                                                                         
                                                                                                                                                         tabPanel("Multiple Family Home",
                                                                                                                                                                  fluidRow(column(6,dataTableOutput("TableWithRadio_mf")),column(6,linebreaks(1),dataTableOutput("TableWithRadio2_mf"))),linebreaks(1),
                                                                                                                                                                  fluidRow(column(5, offset=1, rHandsontableOutput('table6_mf'),linebreaks(1))), 
                                                                                                                                                                          # column(6,rHandsontableOutput('table8_mf'))),
                                                                                                                                                                  linebreaks(2)
                                                                                                                                                                  ,fluidRow(column(5,offset=1, actionButton("button_mf", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                                                                                                                                                          #  ,column(6,#rHandsontableOutput('table9_mf'),
                                                                                                                                                                            # linebreaks(1),actionButton("button2_mf", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                                                                                                                                                  linebreaks(1),column(6,offset=1,textInput("multi_amount", "Total Amount",value="0"))
                                                                                                                                                         ),
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         tabPanel("Commercial",
                                                                                                                                                                  fluidRow(column(6,dataTableOutput("TableWithRadio_com")),column(6,linebreaks(1), dataTableOutput("TableWithRadio2_com"))),linebreaks(1),
                                                                                                                                                                  fluidRow(column(5, offset=1, rHandsontableOutput('table6_com'),linebreaks(1))), 
                                                                                                                                                                         #  column(6,rHandsontableOutput('table8_com')))
                                                                                                                                                                  #,
                                                                                                                                                                  linebreaks(2)
                                                                                                                                                                  ,fluidRow(column(5,offset=1,linebreaks(1),actionButton("button_com", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                                                                                                                                                      #      ,column(6,#rHandsontableOutput('table9_com'),
                                                                                                                                                                      #              linebreaks(1),actionButton("button2_com", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                                                                                                                                                  ,linebreaks(1),column(6,offset=1,textInput("comm_amount", "Total Amount",value="0"))    
                                                                                                                                                         ),
                                                                                                                                                         tabPanel("Civil",
                                                                                                                                                                  fluidRow(column(6,dataTableOutput("TableWithRadio_civ")),column(6,linebreaks(1), dataTableOutput("TableWithRadio2_civ"))),linebreaks(1),
                                                                                                                                                                  fluidRow(column(5, offset=1, rHandsontableOutput('table6_civ'),linebreaks(1))), 
                                                                                                                                                                        #   column(6,rHandsontableOutput('table8_civ')))
                                                                                                                                                                  #,
                                                                                                                                                                  linebreaks(2)
                                                                                                                                                                  ,fluidRow(column(5,offset=1,linebreaks(1),actionButton("button_civ", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                                                                                                                                                  
                                                                                                                                                                  #          ,column(6,#rHandsontableOutput('table9_civ'),
                                                                                                                                                                  #                  linebreaks(1),actionButton("button2_civ", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                                                                                                                                                  ,linebreaks(1),column(6,offset=1,textInput("civil_amount", "Total Amount",value="0"))
                                                                                                                                                         )
                                                                                                                                                         
                  )
                  )
                  
                  )))
        
        ,
        
        tabItem(tabName = "third",
                fluidRow(
                  column(10, offset=0, box(title="Heritage Tourism", status="primary",collapsible=T, width = "auto", height = "auto",solidHeader=T, 
                                           column(12,#="center",
                                                  checkboxInput("checkbox2",label="Day Trips", value=FALSE),rHandsontableOutput('table4b'),checkboxInput("checkbox1", label = "Overnight Trips", value = FALSE),rHandsontableOutput('table4a'),
                                                  linebreaks(1)
                                                  ,rHandsontableOutput('table4c'), linebreaks(1),actionButton("buttondefault", "Set Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                  actionButton("button_zero_heritage", "Zero Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                  
                                           )) )
                  
                )),
        
        tabItem(tabName = "fourth",
                fluidRow(
                  column(10, offset=0, box(title="Historic Museums and Sites", status="primary",collapsible=T,solidHeader=T,width = "auto", height = "auto",
                                                                                                              column(12,                                                  
                                                                                                              renderText("Please enter the values as though they were in 1000s.\n"),
                                                                                                              rHandsontableOutput('historicRehabOne'),linebreaks(1)
                                                                    )))
                  
                ))
      )
    }
    else {
      print("D")
      loginpage
    }
  })
  observeEvent(input$main_street, {
    updateTabItems(session = session, inputId = "tabs", selected = "dashboard")
  })
  observeEvent(input$rehab_button, {
    print("Main button was pressed")
    updateTabItems(session = session, inputId = "tabs", selected = "second")
  })
  observeEvent(input$heritage_button, {
    print("Main button was pressed")
    updateTabItems(session = session, inputId = "tabs", selected = "third")
  })
  observeEvent(input$historic_button, {
    print("Main button was pressed")
    updateTabItems(session = session, inputId = "tabs", selected = "fourth")
  })
  output$mainStreetOne <- renderRHandsontable({
    
    print("table1")
    manipulator<-1000
    construction_bool<-TRUE
    jobs_bool<- TRUE
    df_constructionA<-df_constructionA() #4 Rows
    state=reactiveVal(state_variable())
    colnames(df_constructionA)<-c("Construction")
    rownames(df_constructionA)<-c("Rehabilitation:","New Construction:","Joint Ventures:","Construction Subtotal*:")
    rhandsontable(df_constructionA,stretchH = "all",rowHeaderWidth=300)%>%
      hot_col("Construction", format = "$0,0.00") 
  }) 
  output$mainStreetTwo <- renderRHandsontable({
    df_jobsA<-df_jobsA() #4 Rows
    colnames(df_jobsA)<-"Jobs"
    rownames(df_jobsA)<-"Number of Continuous Jobs (FTE): "
    rhandsontable(df_jobsA,stretchH = "all",rowHeaderWidth=300)
    
  }) 
  output$mainStreetThree <- renderRHandsontable({
    df_jobsB<-as.matrix(df_jobsB(), nrow=1,ncol=1) #4 Rows
    colnames(df_jobsB)<-" "
    rownames(df_jobsB)<-"Total Output (Approximate) "
    
    rhandsontable(df_jobsB,stretchH = "all",rowHeaderWidth=300)%>%
      hot_col(" ", readOnly = TRUE) 
  }) 
  
  output$table2<- renderRHandsontable({
    df_jobs=df_jobs()
    colnames(df_jobs)<-"Jobs"
    rownames(df_jobs)<-c("Net Gain in Jobs Created","Total Output (approximate)")
    # print("YEAH")
    if(is.null(df_jobs())){}
    else{
      rhandsontable(df_jobs, rowHeaderWidth=200,width=400, height=300,stretchH = "all")}
    
  })
  output$historicRehabOne <- renderRHandsontable({
    print("historicRehabOne")
    df_spending=df_spending()
    colnames(df_spending)<-" "
    
    rownames(df_spending)<-c(
      "Annual Operation Spending","Annual Capital Spending","Visitor's Revenues Generated"
    )
    rhandsontable(df_spending, rowHeaderWidth=200,stretchH = "all") %>%
      hot_col(" ", format = "$0,0.00") })
  
  output$table4a <- renderRHandsontable({
    df_heritagea<-df_heritagea()
    colnames(df_heritagea)<-c("Percent","$/ Person Nights")
    rownames(df_heritagea)<-c("Camping", "Commercial Lodging","Friends/ Family")
    rhandsontable(df_heritagea, rowHeaderWidth=200, stretchH = "all")%>%
      hot_col("$/ Person Nights", format = "$0,0.00") %>%
      hot_col("Percent", format = "0.0%") 
    
  }) 
  
  output$table4b <- renderRHandsontable({
    df_heritageb<-df_heritageb()
    colnames(df_heritageb)<-c("Person Days","$/Person Days")
    rownames(df_heritageb)<-c(" ")
    rhandsontable(df_heritageb, rowHeaderWidth=200,stretchH = "all")%>%
      hot_col("$/Person Days", format = "$0,0.00")
  }) 
  
  output$table4c <- renderRHandsontable({
    df_heritagec<-df_heritagec()
    colnames(df_heritagec)<-c(" ")
    rownames(df_heritagec)<-c("Total Person Nights ")
    rhandsontable(df_heritagec, rowHeaderWidth=200,stretchH = "all")
  }) 
  
  #width=800,
  output$table5 <- renderRHandsontable({
    print("table5")
    
    # print("we're in table5")
    DF = data.frame(c("Types of System Repair:","Amount of Interior Work Needed:","Amount of Site Work Needed:"), bool = TRUE, stringsAsFactors = FALSE)
    #print('creating table5')
    rhandsontable(DF,colHeaders=c("",""), rowHeaders = NULL, width = 550, height = 300, stretchH="all") 
    
  })
  output$table6 <- renderRHandsontable({
    print("table6")
    
    table6<-df_table6()
    colnames(table6)<-" "
    rownames(table6)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
    rhandsontable(table6, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })

  output$table8 <- renderRHandsontable({
    print("table8")
    table8<-df_table8()
    colnames(table8)<- "% Labor"
    rownames(table8)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
    rhandsontable(table8, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0.00%")
    
  })

  output$table6_mf <- renderRHandsontable({
    table6_mf<-df_table6_mf()
    colnames(table6_mf)<-" "
    rownames(table6_mf)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
    rhandsontable(table6_mf, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })

  output$table8_mf <- renderRHandsontable({
    table8_mf<-df_table8_mf()
    colnames(table8_mf)<-"% Labor"
    rownames(table8_mf)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
    rhandsontable(table8_mf, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0%")
    
  })

  
  output$table6_com <- renderRHandsontable({
    table6_com<-df_table6_com()
    colnames(table6_com)<-" "
    rownames(table6_com)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
    rhandsontable(table6_com, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })

  output$table8_com <- renderRHandsontable({
    table8_com<-df_table8_com()
    colnames(table8_com)<-"% Labor"
    rownames(table8_com)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
    rhandsontable(table8_com, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0.00%")
    
  })

  
  output$table6_civ <- renderRHandsontable({
    table6_civ<-df_table6_civ()
    colnames(table6_civ)<-" "
    rownames(table6_civ)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
    rhandsontable(table6_civ, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })

  output$table8_civ <- renderRHandsontable({
    table8_civ<-df_table8_civ()
    colnames(table8_civ)<-"% Labor"
    rownames(table8_civ)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
    rhandsontable(table8_civ, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0.00%")
  })

  observeEvent(input$mainStreetOne$changes$changes,{
    print("input$table1$changes$changes")
    user_input<-hot_to_r(input$mainStreetOne)
    jobs_output<-as.matrix(read_excel(file.path(getwd(),state_variable(),paste("AL","_vectors.xlsx",sep="")),sheet=1)[291,28])
    jobsOutput<-as.numeric(jobs_output[1,1])
    df_jobsA<-df_jobsA()
    jobsAValue<-as.numeric(df_jobsA[1,1])
    print("Jobs Output")
    print(jobsOutput)
    intermediary_value<-jobsAValue/jobsOutput
    user_input[4,1]<-user_input[1,1]+user_input[2,1]+user_input[3,1]
    df_constructionA(user_input)
  #  
    df_jobsB(user_input[4,1]+intermediary_value)
    df_constructionA<-reactiveVal(user_input)
    
  })
  observeEvent(input$mainStreetTwo$changes$changes,{
    print("input$table1$changes$changes")
    user_input<-hot_to_r(input$mainStreetTwo)
    df_jobsA(user_input)
    jobs_output<-as.matrix(read_excel(file.path(getwd(),state_variable(),paste("AL","_vectors.xlsx",sep="")),sheet=1)[291,28])
    jobsOutput<-as.numeric(jobs_output[1,1])
    # df_jobsA<- reactiveVal(user_input)
    df_constructionA<-df_constructionA()
    df_jobsB(df_constructionA[4,1]+user_input[1,1]/jobs_output)
    
  })
  
  observeEvent(input$table2$changes$changes,{
    print("input$table2$changes$changes")
    
    df_jobs_input = hot_to_r(input$table2)
    df_jobs_input =total_output(hot_to_r(input$table1),df_jobs_input )

    
    df_jobs(df_jobs_input)
    print("observe vent 4 JOBS")
    
  })
  observeEvent(input$historicRehabOne$changes$changes,{
    print("input$historicRehabOne$changes$changes")
    df_spending_input = hot_to_r(input$historicRehabOne)
    df_spending(df_spending_input)
  })
  observeEvent(input$table4a$changes$changes,{
    df_heritage_input = hot_to_r(input$table4a)
    df_heritagea(df_heritage_input)
  })
  observeEvent(input$table4b$changes$changes,{
    df_heritage_input = hot_to_r(input$table4b)
    df_heritageb(df_heritage_input)
  })
  observeEvent(input$table4c$changes$changes,{
    print("Table Changed")
    df_heritage_input = hot_to_r(input$table4c)
    df_heritagec(df_heritage_input)
    print(df_heritagec())
  })
  
  
  
  #====Part1====
  observeEvent(input$table6$changes$changes,{
    print("In observe event table 6")
    df_table6_input = hot_to_r(input$table6)
    df_table6(df_table6_input)
  })
  


  observeEvent(input$table8$changes$changes,{
    print("In observe event table 8")
    df_table8_input = hot_to_r(input$table8)
    
    df_table8(df_table8_input)
    
  })

  
  #====Part2====
  
  
  observeEvent(input$table6_mf$changes$changes,{
    print("In observe event table 6_mf")
    df_table6_input = hot_to_r(input$table6_mf)
    df_table6_mf(df_table6_input)
  })
  
 
  observeEvent(input$table8_mf$changes$changes,{
    print("In observe event table 8_mf")
    df_table8_input = hot_to_r(input$table8_mf)
    
    df_table8_mf(df_table8_input)
    
  })

  
  observeEvent(input$table6_com$changes$changes,{
    print("In observe event table 6_com")
    df_table6_input = hot_to_r(input$table6_com)
    df_table6_com(df_table6_input)
  })
  
  observeEvent(input$table7_com$changes$changes,{
    print("In observe event table 7_com")
    df_table7_input = hot_to_r(input$table7_com)
    print("In observe event table 7 part 2")
    
    df_table7(df_table7_input)
  })
  
  observeEvent(input$table8_com$changes$changes,{
    print("In observe event table 8_com")
    df_table8_input = hot_to_r(input$table8_com)
    
    df_table8_com(df_table8_input)
    
  })

  
  observeEvent(input$table6_civ$changes$changes,{
    print("In observe event table 6_civ")
    df_table6_input = hot_to_r(input$table6_civ)
    df_table6_civ(df_table6_input)
  })
  
  observeEvent(input$table7_civ$changes$changes,{
    print("In observe event table 7_civ")
    df_table7_input = hot_to_r(input$table7_civ)
    print("In observe event table 7 part 2")
    
    df_table7_civ(df_table7_input)
  })
  
  observeEvent(input$table8_civ$changes$changes,{
    print("In observe event table 8 _civ")
    df_table8_input = hot_to_r(input$table8_civ)
    
    df_table8_civ(df_table8_input)
    
  })
  observeEvent(input$table9_civ$changes$changes,{
    print("In observe event table 9 _civ")
    df_table9_input = hot_to_r(input$table9_civ)
    
    df_table9_civ(df_table9_input)
    
  })
  
  
  observeEvent(input$buttondefault, {
    #Assigns the default values. df_heritageb is the first table, df_heritagea is the second table. df_heritage c is the total person nights. 
    df_heritageb<-df_heritageb()
    df_heritageb[1,2]<-62.45
    #df_heritageb(df_heritageb)
    
    df_heritagea<-df_heritagea()
    df_heritagea[1,1]<-.04
    df_heritagea[1,2]<-44.44
    
    df_heritagea[2,1]<-round(.58, 2)
    print("This is the value: ")
    print(round(.58,2))
    df_heritagea[2,2]<-83.57
    df_heritagea[3,1]<-.38
    df_heritagea[3,2]<-.5344
    #df_heritagea(df_heritagea)
    
    df_heritagec<-df_heritagec()
    df_heritagec[1,1]<-0
    print("DF_HERITAGE C")
    print(df_heritagec)
    x<<-df_heritagec
    df_heritagec(df_heritagec)
    df_heritageb(df_heritageb)
    df_heritagea(df_heritagea)
    
  })
  
  observeEvent(input$zeroButtonMain,
  {
    print("We in")
    df_constructionA<-df_constructionA() #4 Rows
    df_constructionA[1,1]=0
    df_constructionA[2,1]=0
    df_constructionA[3,1]=0
    df_constructionA[4,1]=0
    df_constructionA(df_constructionA)
    df_jobsA=df_jobsA()
    df_jobsA[1,1]=0
    df_jobsA(df_jobsA)
  })
  observeEvent(input$button_zero_heritage,
               {
                 #Assigns the default values. df_heritageb is the first table, df_heritagea is the second table. df_heritage c is the total person nights. 
                 df_heritageb<-df_heritageb()
                 df_heritageb[1,2]<-0
                 df_heritagea<-df_heritagea()
                 df_heritagea[1,1]<-0
                 df_heritagea[1,2]<-0
                 df_heritagea[2,1]<-0
                 df_heritagea[2,2]<-0
                 df_heritagea[3,1]<-0
                 df_heritagea[3,2]<-0
                 df_heritagec<-df_heritagec()
                 df_heritagec[1,1]<-0
                 print("DF_HERITAGE C")
                 print(df_heritagec)
                 x<<-df_heritagec
                 df_heritagec(df_heritagec)
                 df_heritageb(df_heritageb)
                 df_heritagea(df_heritagea)
               })
  observeEvent(input$button,{
    print("Button, Part 1")
    table6<-df_table6()
    print("Button, Part 1b")
    
    table1<-get_value_rehab(dtWithRadioButton,1)
    print("Button, Part 1c")
    
    table2<-get_value_rehab(dtWithRadioButton,2)
    print("Button, Part 2")
    
    data<-as.matrix(getPercentages(table1,table2, 1))
    architecture<-as.numeric(8.52/100)
    print("Button, Part 3")
    
    #Soft Costs is yes
    if(table2[3,1]==11){
      data=data*.8
      architecture<-architecture*.9
      data[17]<-.1 #We're adding one more here. 
      table6[1,1] = .1
    }
    else{
      data[17]<-0
      table6[1,1]<-0
    }
    
    print("Button, Part 4")
    
    table6[2,1]<-data[1]
    table6[3,1]<-data[2]#as.numeric(2.03/100)
    table6[4,1]<-data[3]#as.numeric(8.68/100)
    table6[5,1]<-data[4]#as.numeric(.55/100)
    table6[6,1]<-data[5]#as.numeric(15.81/100)
    table6[7,1]<-data[6]#as.numeric(4.36/100)
    table6[8,1]<-data[7]#as.numeric(10.7/100)
    table6[9,1]<-data[8]#as.numeric(0)
    table6[10,1]<-data[9]#as.numeric(.21/100)
    table6[11,1]<-data[10]#as.numeric(2.26/100)
    table6[12,1]<-data[11]#as.numeric(0)
    table6[13,1]<-data[12]#as.numeric(0)
    table6[14,1]<-data[13]#as.numeric(0)
    table6[15,1]<-data[14]#as.numeric(6.06/100)
    table6[16,1]<-data[15]#as.numeric(2.32/100)
    table6[17,1]<-data[16]#as.numeric(2.61/100)
    table6[18,1]<-data[17]
    df_table6(table6)
    # dt <- dtWithRadioButton$dt # accessing the reactive value
    # do some processing based on the radio button selection
    
    #values<-get_value1_rehab(dt)
    #print("Values")
    #print(values)
    
  })
  observeEvent(input$button2,{
    table8<-df_table8()
    table8[1,1]<-as.numeric(60/100)
    table8[2,1]<-as.numeric(60/100)
    table8[3,1]<-as.numeric(60/100)
    table8[4,1]<-as.numeric(40/100)
    table8[5,1]<-as.numeric(35/100)
    table8[6,1]<-as.numeric(45/100)
    table8[7,1]<-as.numeric(35/100)
    table8[8,1]<-as.numeric(35/100)
    table8[9,1]<-as.numeric(30/100)
    table8[10,1]<-as.numeric(0)
    table8[11,1]<-as.numeric(15/100)
    table8[12,1]<-as.numeric(15/100)
    table8[13,1]<-as.numeric(15/100)
    table8[14,1]<-as.numeric(60/100)
    table8[15,1]<-as.numeric(60/100)
    df_table8(table8)
    
  })
  
  observeEvent(input$button_mf,{
    table6_mf<-df_table6_mf()
    
    print("We're in the dispersal section:")
    table1<-get_value_rehab(dtWithRadioButton_mf,1)
    table2<-get_value_rehab(dtWithRadioButton_mf,2)
    data<-as.matrix(getPercentages(table1,table2, 2))
    
    print("The data:")
    print(data)
    if(table2[3,1]==11){
      data=data*.8
      data[17]<-.1 #We're adding one more here. 
      table6_mf[1,1]<-.1 #We're adding one more here. 
      
    }
    else{
      data[17]<-0
      table6_mf[1,1]<-0}
    
    table6_mf[2,1]<-data[1]#as.numeric(12.87/100)
    table6_mf[3,1]<-data[2]#as.numeric(1.99/100)
    table6_mf[4,1]<-data[3]#as.numeric(8.49/100)
    table6_mf[5,1]<-data[4]#as.numeric(.54/100)
    table6_mf[6,1]<-data[5]#as.numeric(15.46/100)
    table6_mf[7,1]<-data[6]#as.numeric(4.27/100)
    table6_mf[8,1]<-data[7]#as.numeric(10.46/100)
    #table7_mf<-df_table7_mf()
    table6_mf[9,1]<-data[8]#as.numeric(14.24/100)
    table6_mf[10,1]<-data[9]#as.numeric(.21/100)
    table6_mf[11,1]<-data[10]#as.numeric(2.21/100)
    table6_mf[12,1]<-data[11]#as.numeric(0)
    table6_mf[13,1]<-data[12]#as.numeric(0)
    table6_mf[14,1]<-data[13]#as.numeric(0)
    table6_mf[15,1]<-data[14]#as.numeric(5.93/100)
    table6_mf[16,1]<-data[15]#as.numeric(2.27/100)
    table6_mf[17,1]<-data[16]#as.numeric(2.55/100)
    table6_mf[18,1]<-data[17]#as.numeric(2.55/100)
    df_table6_mf(table6_mf)
    
  })
  observeEvent(input$button2_mf,{
    table8_mf<-df_table8_mf()
    table8_mf[1,1]<-as.numeric(60/100)
    table8_mf[2,1]<-as.numeric(60/100)
    table8_mf[3,1]<-as.numeric(60/100)
    table8_mf[4,1]<-as.numeric(40/100)
    table8_mf[5,1]<-as.numeric(35/100)
    table8_mf[6,1]<-as.numeric(45/100)
    table8_mf[7,1]<-as.numeric(35/100)
    table8_mf[8,1]<-as.numeric(35/100)
    table8_mf[9,1]<-as.numeric(30/100)
    table8_mf[10,1]<-as.numeric(15/100)
    table8_mf[11,1]<-as.numeric(15/100)
    table8_mf[12,1]<-as.numeric(15/100)
    table8_mf[13,1]<-as.numeric(15/100)
    table8_mf[14,1]<-as.numeric(60/100)
    table8_mf[15,1]<-as.numeric(60/100)
    df_table8_mf(table8_mf)
    
  })
  
  
  observeEvent(input$button_com,{
    table6_com<-df_table6_com()
    print("We're in the dispersal section:")
    table1<-get_value_rehab(dtWithRadioButton_com,1)
    table2<-get_value_rehab(dtWithRadioButton_com,2)
    data<-as.matrix(getPercentages(table1,table2, 3))
    if(table2[3,1]==11){
      data=data*.8
      data[17]<-.1 #We're adding one more here. 
      table6_com[1,1]<-.1
      
    }
    else{
      data[17]<-0
      table6_com[1,1]<-0

      }
    table6_com[2,1]<-data[1]#as.numeric(12.87/100)
    table6_com[3,1]<-data[2]#as.numeric(1.99/100)
    table6_com[4,1]<-data[3]#as.numeric(8.49/100)
    table6_com[5,1]<-data[4]#as.numeric(.54/100)
    table6_com[6,1]<-data[5]#as.numeric(15.46/100)
    table6_com[7,1]<-data[6]#as.numeric(4.27/100)
    table6_com[8,1]<-data[7]#as.numeric(10.46/100)
    #df_table6_com(table6_com)
    #table7_com<-df_table7_com()
    table6_com[9,1]<-data[8]#as.numeric(14.24/100)
    table6_com[10,1]<-data[9]#as.numeric(.21/100)
    table6_com[11,1]<-data[10]#as.numeric(2.21/100)
    table6_com[12,1]<-data[11]#as.numeric(0)
    table6_com[13,1]<-data[12]#as.numeric(0)
    table6_com[14,1]<-data[13]#as.numeric(0)
    table6_com[15,1]<-data[14]#as.numeric(5.93/100)
    table6_com[16,1]<-data[15]#as.numeric(2.27/100)
    table6_com[17,1]<-data[16]#as.numeric(2.55/100)
    table6_com[18,1]<-data[17]#as.numeric(2.55/100)
    
    df_table6_com(table6_com)
  })
  observeEvent(input$button2_com,{
    table8_com<-df_table8_com()
    table8_com[1,1]<-as.numeric(60/100)
    table8_com[2,1]<-as.numeric(60/100)
    table8_com[3,1]<-as.numeric(60/100)
    table8_com[4,1]<-as.numeric(40/100)
    table8_com[5,1]<-as.numeric(35/100)
    table8_com[6,1]<-as.numeric(45/100)
    table8_com[7,1]<-as.numeric(35/100)
    table8_com[8,1]<-as.numeric(35/100)
    table8_com[9,1]<-as.numeric(30/100)
    table8_com[10,1]<-as.numeric(15/100)
    table8_com[11,1]<-as.numeric(15/100)
    table8_com[12,1]<-as.numeric(15/100)
    table8_com[13,1]<-as.numeric(15/100)
    table8_com[14,1]<-as.numeric(60/100)
    table8_com[15,1]<-as.numeric(60/100)
    df_table8_com(table8_com)
    
  })
  
  observeEvent(input$button_civ,{
    table6_civ<-df_table6_civ()
    print("We're in the dispersal section:")
    table1<-get_value_rehab(dtWithRadioButton,1)
    table2<-get_value_rehab(dtWithRadioButton,2)
    data<-as.matrix(getPercentages(table1,table2, 4))
    if(table2[3,1]==11){
      data=data*.8
      data[17]<-.1 #We're adding one more here. 
      table6_civ[1,1]=.1
    }
    else{
      data[17]<-0
      table6_civ[1,1]=0
      }
    table6_civ[2,1]<-data[1]#as.numeric(12.87/100)
    table6_civ[3,1]<-data[2]#as.numeric(1.99/100)
    table6_civ[4,1]<-data[3]#as.numeric(8.39/100)
    table6_civ[5,1]<-data[4]#as.numeric(.54/100)
    table6_civ[6,1]<-data[5]#as.numeric(18.55/100)
    table6_civ[7,1]<-data[6]#as.numeric(4.27/100)
    table6_civ[8,1]<-data[7]#as.numeric(10.46/100)
    #table7_civ<-df_table7_civ()
    table6_civ[9,1]<-data[8]#as.numeric(14.24/100)
    table6_civ[10,1]<-data[9]#as.numeric(.21/100)
    table6_civ[11,1]<-data[10]#as.numeric(2.21/100)
    table6_civ[12,1]<-data[11]#as.numeric(0)
    table6_civ[13,1]<-data[12]#as.numeric(0)
    table6_civ[14,1]<-data[13]#as.numeric(0)
    table6_civ[15,1]<-data[14]#as.numeric(5.93/100)
    table6_civ[16,1]<-data[15]#as.numeric(2.27/100)
    table6_civ[17,1]<-data[16]#as.numeric(2.55/100)
    table6_civ[18,1]<-data[17]#as.numeric(2.55/100)
    
    df_table6_civ(table6_civ)
    
  })
  observeEvent(input$button2_civ,{
    table8_civ<-df_table8_civ()
    table8_civ[1,1]<-as.numeric(60/100)
    table8_civ[2,1]<-as.numeric(60/100)
    table8_civ[3,1]<-as.numeric(60/100)
    table8_civ[4,1]<-as.numeric(40/100)
    table8_civ[5,1]<-as.numeric(35/100)
    table8_civ[6,1]<-as.numeric(45/100)
    table8_civ[7,1]<-as.numeric(35/100)
    table8_civ[8,1]<-as.numeric(35/100)
    table8_civ[9,1]<-as.numeric(30/100)
    table8_civ[10,1]<-as.numeric(15/100)
    table8_civ[11,1]<-as.numeric(15/100)
    table8_civ[12,1]<-as.numeric(15/100)
    table8_civ[13,1]<-as.numeric(15/100)
    table8_civ[14,1]<-as.numeric(60/100)
    table8_civ[15,1]<-as.numeric(60/100)
    df_table8_civ(table8_civ)
    
  })
  
  
  observeEvent(input$historicRehabOne$changes$changes,{
    print("In observe event 1")
    df_spending_input = hot_to_r(input$historicRehabOne)
    print("In observe event 2")
    print(df_spending_input[1,1])
    
    df_spending_input[1,1]=df_spending_input[1,1]+manipulator
    print(df_spending_input[1,])
    print("In observe event 3")
    
    df_spending(df_spending_input)
    print("observe vent 4")
    
  })
  
  get_value_rehab<-function(dtWithRadioButton,tableNumber){
    print("Get_Value_Rehab Part A.5")
    x=0
    radioTable=0
    values=0
    if(tableNumber==1){  values<-c("Types of System Repair","Amount of Interior Work Needed", "Amount of Site Work Needed")
    } else {  values<-c("Kind of Material Used","Amount of Exterior Work Needed", "Include Soft Costs")}
    #This is not in "rehab_radio_tables.R" because it does not work in any other file. 
    #This function translates the radio button values into a table of numbers to identify selection. 
    #tableNumber is to decide if this is for the first or the second radioTable. 
    print("Get_Value_Rehab Part A")
    m<-dtWithRadioButton$dt
    print("Get_Value_Rehab Part B")
    #Setting the radiotable values to m. 
    for (i in seq_len(nrow(m))) {
      x[i]=paste0(values[i], ":", input[[values[i]]])
      #Extracting the value of each radio button as well as the category to x. 
      radioTable[i]<-strsplit(x[i],":")
      #Splits x into the category label and the actual value (or number ID of the radio button, if you will)
    }
    print("Get_Value_Rehab Part C")
    table=splitTable(radioTable, values)
    print("Get_Value_Rehab Part D")
    return (table)
  }
  # Fill in the spot we created for a plot
  
  
  getInputs <- function(pattern){
    reactives <- names(reactiveValuesToList(input))
    reactives[grep(pattern,reactives)]
  }
  
  
  
  
  
output$TableWithRadio<-renderDT(
    datatable(dtWithRadioButton$dt, selection = "none", escape = FALSE, 
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE
              ), 
              callback = JS(
                "table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                Shiny.bindAll(table.table().node());"),
              rownames = TRUE), server = FALSE)
  output$TableWithRadio2<-  renderDT(
 #   output$TableWithRadio2$dt$Extensive <- paste0("<b>", output$TableWithRadio2$dt$Extensive, "</b>")>
    datatable(dtWithRadioButton2$dt, selection = "none", escape = FALSE, 
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE
              ), 
              callback = JS(
                "table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                Shiny.bindAll(table.table().node());"),
              rownames = TRUE), server = FALSE)
  
  linebreaks <- function(n){HTML(strrep(br(), n))}
  
  output$TableWithRadio_mf<-  renderDT(
    datatable(dtWithRadioButton_mf$dt, selection = "none", escape = FALSE, 
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE
              ), 
              callback = JS(
                "table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                Shiny.bindAll(table.table().node());"),
              rownames = TRUE), server = FALSE)
  
  output$TableWithRadio2_mf<-  renderDT(
    datatable(dtWithRadioButton2_mf$dt, selection = "none", escape = FALSE, 
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE
              ), 
              callback = JS(
                "table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                Shiny.bindAll(table.table().node());"),
              rownames = TRUE), server = FALSE)
  
  output$TableWithRadio_com<-  renderDT(
    datatable(dtWithRadioButton_com$dt, selection = "none", escape = FALSE, 
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE
              ), 
              callback = JS(
                "table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                Shiny.bindAll(table.table().node());"),
              rownames = TRUE), server = FALSE)
  output$TableWithRadio2_com<-  renderDT(
    datatable(dtWithRadioButton2_com$dt, selection = "none", escape = FALSE, 
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE
              ), 
              callback = JS(
                "table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                Shiny.bindAll(table.table().node());"),
              rownames = TRUE), server = FALSE)
  
  
  output$TableWithRadio_civ<-  renderDT(
    datatable(dtWithRadioButton_com$dt, selection = "none", escape = FALSE, 
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE
              ), 
              callback = JS(
                "table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                Shiny.bindAll(table.table().node());"),
              rownames = TRUE), server = FALSE)
  
  
  output$TableWithRadio2_civ<-  renderDT(
    datatable(dtWithRadioButton2_com$dt, selection = "none", escape = FALSE, 
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE
              ), 
              callback = JS(
                "table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-radiogroup');
                });
                Shiny.unbindAll(table.table().node());
                Shiny.bindAll(table.table().node());"),
              rownames = TRUE), server = FALSE)
  
  
  
  
  
  addTooltip(session, id = "btn", title = "This is an input.",placement = "left", trigger = "hover")  
  
  tags$style(HTML("
    mainStreetOne {
      overflow: hidden !important;
    }
  "))
  tags$style(HTML("
    mainStreetTwo {
      overflow: hidden !important;
    }
  "))
  tags$style(HTML("
    mainStreetThree {
      overflow: hidden !important;
    }
  "))
  
  state_variable<-  reactive({
    return (input$states)
  })
  
}