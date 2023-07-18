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


source("main_street.R")
source("getStates.R")
source("calculations.R")
source("rehab.R")
source("calculations.R")
source("rehab_radio_tables.R")

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
  print("A")
  wb2<<-0
  #De-vlobalize after
  heritageValues<<-0
  rehab_value<<-0
  
  #Arbtirary Default Values
  rehab_output<<-2
  total_output<<-0
  

  detailed_industry<<-as.data.frame(matrix(0, nrow = 404, ncol = 3))
  df_heritageb<<- reactiveVal(as.data.frame(matrix(0, nrow = 1, ncol = 2)))
  df_heritagea<<- reactiveVal(as.data.frame(matrix(0, nrow = 3, ncol = 2)))
  df_heritagec<<- reactiveVal(as.data.frame(matrix(0, nrow = 1, ncol = 1)))
  #df_construction<<-reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  df_constructionA<<-reactiveVal(data.frame(cur=c(0,0,0,0)))
  
  #df_construction<<-reactiveVal(as.data.frame(matrix(0, nrow = 4, ncol = 1)))
  df_jobsA<<- reactiveVal(as.data.frame(matrix(0, nrow=1,ncol=1)))
  df_jobsB<<-reactiveVal(as.data.frame(matrix(0, nrow=1,ncol=1)))
  df_spending<<-reactiveVal(as.data.frame(matrix(0, nrow = 3, ncol = 1)))
  
  #Table 6 / historic building rehabilitation Single Fam
  df_table6<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  df_table7<<- reactiveVal(as.data.frame(matrix(0, nrow = 10, ncol = 1)))
  df_table8<<- reactiveVal(as.data.frame(matrix(0, nrow = 7, ncol = 1)))
  df_table9<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  
  #Multi Family
  df_table6_mf<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  df_table7_mf<<- reactiveVal(as.data.frame(matrix(0, nrow = 10, ncol = 1)))
  df_table8_mf<<- reactiveVal(as.data.frame(matrix(0, nrow = 7, ncol = 1)))
  df_table9_mf<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  #Commercial
  df_table6_com<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  df_table7_com<<- reactiveVal(as.data.frame(matrix(0, nrow = 10, ncol = 1)))
  df_table8_com<<- reactiveVal(as.data.frame(matrix(0, nrow = 7, ncol = 1)))
  df_table9_com<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  #Civic
  df_table6_civ<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  df_table7_civ<<- reactiveVal(as.data.frame(matrix(0, nrow = 10, ncol = 1)))
  df_table8_civ<<- reactiveVal(as.data.frame(matrix(0, nrow = 7, ncol = 1)))
  df_table9_civ<<- reactiveVal(as.data.frame(matrix(0, nrow = 8, ncol = 1)))
  
  
  
  #===========For downloadables============================================
  state_variable<<-  reactive({
    print("in state_variable")
    #State variable is a reactive function meant to store whatever state the user chooses. Reactive means that this part of the code will be re-executed every time
    #the code detects a change in this value.
    return (input$states)
    #We return the states variable here for whenever I call it later. 
  })
  
  df_new2 <<- as.data.frame(matrix(0, nrow = 404, ncol = 4)) 
  A_matrix_reac<- reactiveVal(0)
  # 
  # #Downloadable Stuff
  observeEvent(input$states,{
    
  })
  
  #observeEvent(output$downloadbtn,{
  #  print("ENTER DOWNLOAD BTN")
  
  
  #=============================================
  
  initial_detailed_industry <- function(state_variable="AL", CDF=0) {
    #Input$Start is the ID of the "Generate" button.  
    wb2<<-read_excel("recon_info.xlsx")#,rowIndex=3:408, colIndex=1:2)
    wb2<<-wb2[-c(1,2,3),]
    wb2<<-wb2[1:404,]
    rehab_output<<-cbind(sapply(wb2[,1],as.numeric),wb2[,2],0)
    colnames(rehab_output)<<-c("REcon","Industries","Output")
    print(dim(rehab_output))
    print(rehab_output)
    return()
  }
  heritageCalculator<-function(df_heritagea,df_heritageb,df_heritagec)
  {
    #df_heritageb(df_heritageb)
    #df_heritagea(df_heritagea)
    #df_heritagec(df_heritagec)
    
    value1<-0
    value2<-0
    value3<-0
    heritage_total<-0
    #Does all the heritage calculations. 
    if(input$checkbox2&&input$checkbox1==1){
      df_heritagec<-as.data.frame(t(as.matrix(c(df_heritagec[1,1],0))))
      df_heritageb<-as.matrix(df_heritageb)
      df_heritagea<-as.matrix(df_heritagea)
      print(df_heritagec)
      
      #I just added an empty value to the third heritage table for total person night
      heritage_total<-rbind(df_heritageb, df_heritagea, df_heritagec)
      value1<- heritage_total[3,1]*heritage_total[3,2]*df_heritagec[1]
      value2<- heritage_total[2,1]*heritage_total[4,2]*df_heritagec[1]
      value3<- heritage_total[4,1]*heritage_total[4,2]*df_heritagec[1]
      #Add day activities
      
    }
    else if (input$checkbox2==1){
      #Zeroing out the tables that are not in the appropraite checkbox. 
      print("Day Stay")
      df_heritagea[1,1]<-0
      df_heritagea[1,2]<-0
      df_heritagea[2,1]<-0
      df_heritagea[2,2]<-0
      df_heritagea[3,1]<-0
      df_heritagea[3,2]<-0
      df_heritagec[1,1]<-0
      #Add calcs
      #Updating the table that is selected with the default values. 
    #  df_heritageb(df_heritageb)
    #  df_heritagec(df_heritagec[1,1])
      
      
    }
    
    else if (input$checkbox1==1){
      print("Night Stays")
      df_heritageb[1,1]<-0
      df_heritageb[1,2]<-0
      df_heritagec<-as.data.frame(t(as.matrix(c(df_heritagec[1,1],0))))
      df_heritageb<-as.matrix(df_heritageb)
      df_heritagea<-as.matrix(df_heritagea)
      
      print("Night Stays Part 1")
      #I just added an empty value to the third heritage table for total person night
      names<-colnames(df_heritageb)
      colnames(df_heritagea)<-names
      colnames(df_heritagec)<-names
      print(rbind(df_heritageb, df_heritagea, df_heritagec))
      heritage_total<-rbind(df_heritageb, df_heritagea, df_heritagec)
      print("Night Stays Part 1B")
      value1<- heritage_total[2,1]*heritage_total[4,2]*df_heritagec[1,1] 
      value2<- heritage_total[3,1]*heritage_total[3,2]*df_heritagec[1,1]
      value3<- heritage_total[4,1]*heritage_total[4,2]*df_heritagec[1,1]
      df_heritageb(df_heritageb)
      print("Night Stays Part 3")
      
     ## df_heritagea(df_heritagea)
    #  df_heritagec(df_heritagec)
      
      
    }
    
    else{print("WOOO")}
    return(c(value1,value2,value3))
    
  }
  
  manipulator<<-0
  user_input<<-0
  
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
      paste("data-", Sys.Date(), ".xlsx", sep="")},
    #The name just shows the date. I should change the name in the near future. 
    content = function(file) {
      
      #Single Family
      print("Single Family A")
      
      table6<-df_table6()
      colnames(table6)<-"A"
      rownames(table6)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture")
      print("Single Family B")
      table7<-df_table7()
      colnames(table7)<-"A"
      rownames(table7)<-c("Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
      combined_table6<-rbind(table6,table7)
      print("Single Family C")
      
      table8<-df_table8()
      colnames(table8)<-"A"
      rownames(table8)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows")
      
      print("Single Family D")
      table9<-df_table9()
      colnames(table9)<-"A"
      rownames(table9)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      combined_table8<-rbind(table8,table9)
      
      table6_mf<-df_table6_mf()
      table7_mf<-df_table7_mf()
      table8_mf<-df_table8_mf()
      rownames(table8_mf)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows")
      table9_mf<-df_table9_mf()
      rownames(table9_mf)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      
      
      combined_table6_mf<-rbind(table6_mf,table7_mf)
      combined_table8_mf<-rbind(table8_mf,table9_mf)
      table6_com<-df_table6_com()
      table7_com<-df_table7_com()
      table8_com<-df_table8_com()
      rownames(table8_com)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows")
      table9_com<-df_table9_com()
      print("Table 9 _com")
      rownames(table9_com)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      #   # 
      combined_table6_com<-rbind(table6_com,table7_com)
      combined_table8_com<-rbind(table8_com,table9_com)
      table6_civ<-df_table6_civ()
      table7_civ<-df_table7_civ()
      table8_civ<-df_table8_civ()
      rownames(table8_civ)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows")
      table9_civ<-df_table9_civ()
      rownames(table9_civ)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
      combined_table6_civ<-rbind(table6_civ,table7_civ)
      combined_table8_civ<-rbind(table8_civ,table9_civ)
      #     print("Table 88 _civ")
      #     
      state=state_variable()
      initial_detailed_industry(state_variable=state)
      
      withProgress(message = 'Calculating...', value = 0, {
        # Number of times we'll go through the loop
        j<-7
        n <- 1
        
        # for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        # Increment the progress bar, and update the detail text.
        incProgress(1/j, detail = paste("Doing part", n))
        
        # Pause for 0.1 seconds to simulate a long computation.
        # }
        #})
        #test<<-combined_table6
        #
        
        rehab_output_template<-rehab_output
        #This is to give each output df a template with industry names. WIthout this template, glitches appeared. 
        rehab_output<<-rehab_calculations(rehab_output_template,combined_table6, j=11 )#rehab_calculations(rehab_output,combined_table6,11)
        #11 refers to the 17 value table on first table
        rehab_value<<-strtoi(input$single_amount)
        n <- 2
        incProgress(1/j, detail = paste("Doing part", n))
        rehab_output[,3]<<-rehab_output[,3]*rehab_value
        rehab_output_mf<<-rehab_calculations(rehab_output_template,combined_table6_mf, j=21)[,3]*strtoi(input$multi_amount)
        n <- 3
        incProgress(1/j, detail = paste("Doing part", n))
        rehab_output_com<-rehab_calculations(rehab_output_template,combined_table6_com,j=31)[,3]*strtoi(input$comm_amount)
        rehab_output_civil<-rehab_calculations(rehab_output,combined_table8_com, j=31)[,3]*strtoi(input$civil_amount)#Labor percents will be ignored.
        rehab_output[,3]<<-rehab_output[,3]+rehab_output_mf+rehab_output_com+rehab_output_civil
        n <- 4
        incProgress(1/j, detail = paste("Doing part", n))
        #Transferring rehab_output into total_output
        
        #total_output[,3]<<- as.numeric(rehab_output[,3])
        # #   #Part 1 Main Street
        df_main_street_one<-df_constructionA()
        
        n <- 5
        incProgress(1/j, detail = paste("Doing part", n))
        
          rehab_output[29,3]<<-as.numeric(df_main_street_one[1,1])/1000 #Rehab   #Office and commercial structures
          rehab_output[32,3]<<- as.numeric(df_main_street_one[2,1])/1000 #New Construction
          rehab_output[31,3]<<- .5*as.numeric(df_main_street_one[3,1])/1000 #Rehab
          rehab_output[33,3]<<- .5*as.numeric(df_main_street_one[3,1])/1000 #Rehab
        
        n <- 6
        incProgress(1/j, detail = paste("Doing part", n))
        #   #Part 3 Tourism CHECK ALL OF THIS ALLOCATION
        
        heritageValues<<-heritageCalculator(df_heritagea=df_heritagea(),df_heritageb=df_heritageb(),df_heritagec=df_heritagec())
        rehab_output[365,3]<<-heritageValues[1] ## df_heritage<<- reactiveVal(as.data.frame(matrix(0, nrow = 5, ncol = 2)))
        rehab_output[364,3]<<-heritageValues[2]
        rehab_output[394,3]<<-heritageValues[3]#I attempted to pick the Household Sector
        
        n <- 7
        incProgress(1/j, detail = paste("Doing part", n))
        #   
        #   #Part 4 Historic Museums DOUBLE CHECK
        rehab_output[361,3]<<-df_spending()[1,1]
        jobs_retail<-df_jobsA()[1,1] #This variable is the only Jobs variable reported. So it will go into the Jobs column. 
        fund_calc(state,rehab_output,jobs_retail)
        
      })
      #   
      list_of_datasets <- list("Detailed Industry" = df_gen1a, "Super Sectors" = df_gen3a,"3-digit Industry" =df_gen4a,"Direct Effects"=directEffects)#,#"I-pA"=I_pAa,"M"=Ma,"Taxes" = df_gen2a,"totalEffects"=totalEffectsa,"detailedIndustry"=detailedIndustrya,"Super Sector woHH"=super_sector_woHHa, "3 digit industry woHH"=three_digit_industry_woHHa)##,  "Etc." = ditto) # , "3 digit industry woHH"=three_digit_industry_woHH,
      #   #compiling all the tables together into a list. 
      write_xlsx(list_of_datasets, file)

      #writing all the tables into a file. 
      
      
      
      
      
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
                  
                  column(10, offset=0, box(title="Main Street", status="primary",collapsible=T, width = 8,solidHeader=T, 
                                           column(12,#="center",
                                                  rHandsontableOutput('table1a'),
                                                  rHandsontableOutput('table1b'),
                                                  rHandsontableOutput('table1c')
                                           )) )
                  
                )),
        
        tabItem(tabName = "second",
                fluidRow(
                  
                  
                  
                  
                  
                  column(12,offset=0, box(title="Historic Building Rehabilitation", status="primary",collapsible=T, width = 8,solidHeader=T, tabsetPanel(type = "tabs",
                                                                                                                                                         
                                                                                                                                                         tabPanel("Single Family Home", 
                                                                                                                                                                  fluidRow(column(6,dataTableOutput("TableWithRadio")),column(6,linebreaks(1),dataTableOutput("TableWithRadio2"))),linebreaks(1),
                                                                                                                                                                  fluidRow(column(5, offset=1, rHandsontableOutput('table6'),linebreaks(1)), 
                                                                                                                                                                           column(6,rHandsontableOutput('table8'))),
                                                                                                                                                                  #linebreaks(2),
                                                                                                                                                                  fluidRow(column(5,offset=1, rHandsontableOutput('table7'),linebreaks(1),actionButton("button", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                                                                                                                                                            ,column(6,rHandsontableOutput('table9'),linebreaks(1),actionButton("button2", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                                                                                                                                                  ,linebreaks(1),column(6,offset=1,textInput("single_amount", "Total Amount",value="1"))
                                                                                                                                                         ),
                                                                                                                                                         
                                                                                                                                                         tabPanel("Multiple Family Home",
                                                                                                                                                                  fluidRow(column(6,dataTableOutput("TableWithRadio_mf")),column(6,dataTableOutput("TableWithRadio2_mf"))),linebreaks(1),
                                                                                                                                                                  fluidRow(column(5, offset=1, rHandsontableOutput('table6_mf'),linebreaks(1)), 
                                                                                                                                                                           column(6,rHandsontableOutput('table8_mf'))),
                                                                                                                                                                  linebreaks(2)
                                                                                                                                                                  ,fluidRow(column(5,offset=1, rHandsontableOutput('table7_mf'),linebreaks(1),actionButton("button_mf", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                                                                                                                                                            ,column(6,rHandsontableOutput('table9_mf'),linebreaks(1),actionButton("button2_mf", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                                                                                                                                                  ,linebreaks(1),column(6,offset=1,textInput("multi_amount", "Total Amount",value="1"))
                                                                                                                                                         ),
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         tabPanel("Commercial",
                                                                                                                                                                  fluidRow(column(6,dataTableOutput("TableWithRadio_com")),column(6,dataTableOutput("TableWithRadio2_com"))),linebreaks(1),
                                                                                                                                                                  fluidRow(column(5, offset=1, rHandsontableOutput('table6_com'),linebreaks(1)), 
                                                                                                                                                                           column(6,rHandsontableOutput('table8_com')))
                                                                                                                                                                  ,
                                                                                                                                                                  linebreaks(2)
                                                                                                                                                                  ,fluidRow(column(5,offset=1, rHandsontableOutput('table7_com'),linebreaks(1),actionButton("button_com", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                                                                                                                                                            ,column(6,rHandsontableOutput('table9_com'),linebreaks(1),actionButton("button2_com", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                                                                                                                                                  ,linebreaks(1),column(6,offset=1,textInput("comm_amount", "Total Amount",value="1"))    
                                                                                                                                                         ),
                                                                                                                                                         tabPanel("Civil",
                                                                                                                                                                  fluidRow(column(6,dataTableOutput("TableWithRadio_civ")),column(6,dataTableOutput("TableWithRadio2_civ"))),linebreaks(1),
                                                                                                                                                                  fluidRow(column(5, offset=1, rHandsontableOutput('table6_civ'),linebreaks(1)), 
                                                                                                                                                                           column(6,rHandsontableOutput('table8_civ')))
                                                                                                                                                                  ,
                                                                                                                                                                  linebreaks(2)
                                                                                                                                                                  ,fluidRow(column(5,offset=1, rHandsontableOutput('table7_civ'),linebreaks(1),actionButton("button_civ", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                                                                                                                                                            ,column(6,rHandsontableOutput('table9_civ'),linebreaks(1),actionButton("button2_civ", "Get Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                                                                                                                                                  ,linebreaks(1),column(6,offset=1,textInput("civil_amount", "Total Amount",value="1"))
                                                                                                                                                         )
                                                                                                                                                         
                  )
                  )
                  
                  )))
        
        ,
        
        tabItem(tabName = "third",
                fluidRow(
                  column(10, offset=0, box(title="Heritage Tourism", status="primary",collapsible=T, width = 8,solidHeader=T, 
                                           column(12,#="center",
                                                  checkboxInput("checkbox2",label="Day Trips", value=FALSE),rHandsontableOutput('table4b'),checkboxInput("checkbox1", label = "Night Stays", value = FALSE),rHandsontableOutput('table4a'),
                                                  linebreaks(1)
                                                  ,rHandsontableOutput('table4c'), linebreaks(1),actionButton("buttondefault", "Set Default Values",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                  
                                           )) )
                  
                )),
        
        tabItem(tabName = "fourth",
                fluidRow(
                  box(title="Historic Museums and Sites", status="primary",collapsible=T,solidHeader=T,column(12,rHandsontableOutput('table3'),linebreaks(1)))
                  
                ))
      )
    }
    else {
      print("D")
      loginpage
    }
  })
  #NAVIGATING THE MAIN MENU TO SIDE BAR
  observeEvent(input$main_street, {
    print("Main button was pressed")
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
  #================ Start
  
  output$table1a <- renderRHandsontable({
    print("table1")
    manipulator<<-1000
    construction_bool<<-TRUE
    jobs_bool<<- TRUE
    
    df_constructionA<-df_constructionA() #4 Rows
    #print("pst df_construction")
    #print("pst B assignment")
    
    #df_construction<-as.data.frame(matrix(0, nrow = 4, ncol = 1))
    colnames(df_constructionA)<-c("Construction")
    rownames(df_constructionA)<-c("Rehabilitation:","New Construction:","Joint Ventures:","Construction Subtotal*:")
    rhandsontable(df_constructionA,stretchH = "all",rowHeaderWidth=300)%>%
      hot_col("Construction", format = "$0,0.00") 
  }) 
  output$table1b <- renderRHandsontable({
    df_jobsA<-df_jobsA() #4 Rows
    
    colnames(df_jobsA)<-"Jobs"
    rownames(df_jobsA)<-"Number of Continuous Jobs (FTE): "
    rhandsontable(df_jobsA,stretchH = "all",rowHeaderWidth=300)
    
  }) 
  output$table1c <- renderRHandsontable({
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
  output$table3 <- renderRHandsontable({
    print("table3")
    df_spending=df_spending()
    colnames(df_spending)<-" "
    
    rownames(df_spending)<-c(
      
      "Total Annual Spending","Total Capital Spending","Visitor's Revenues Generated"
      
      
    )
    rhandsontable(df_spending, rowHeaderWidth=200,stretchH = "all") %>%
      hot_col(" ", format = "$0,0.00") })
  
  output$table4a <- renderRHandsontable({
    df_heritagea<-df_heritagea()
    colnames(df_heritagea)<-c("Percent","$/ Person-Night")
    rownames(df_heritagea)<-c("Camping", "Commercial Lodging","Friends/ Family")
    rhandsontable(df_heritagea, rowHeaderWidth=200, stretchH = "all")%>%
      hot_col("$/ Person-Night", format = "$0,0.00") %>%
      hot_col("Percent", format = "0.0%") 
    
  }) 
  
  output$table4b <- renderRHandsontable({
    df_heritageb<-df_heritageb()
    colnames(df_heritageb)<-c("Person_Days","$/Person_Days")
    rownames(df_heritageb)<-c(" ")
    rhandsontable(df_heritageb, rowHeaderWidth=200,stretchH = "all")%>%
      hot_col("$/Person_Days", format = "$0,0.00")
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
    rownames(table6)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture")
    rhandsontable(table6, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })
  output$table7 <- renderRHandsontable({
    print("table7")
    
    table7<-df_table7()
    colnames(table7)<-" "
    rownames(table7)<-c("Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
    rhandsontable(table7, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })
  output$table8 <- renderRHandsontable({
    print("table8")
    table8<-df_table8()
    colnames(table8)<- "% Labor"
    rownames(table8)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows")
    rhandsontable(table8, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0.00%")
    
  })
  output$table9 <- renderRHandsontable({
    print("table9")
    table9<-df_table9()
    colnames(table9)<-"% Labor"
    rownames(table9)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
    rhandsontable(table9, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0.00%")
    
  })
  output$table6_mf <- renderRHandsontable({
    table6_mf<-df_table6_mf()
    colnames(table6_mf)<-" "
    rownames(table6_mf)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture")
    rhandsontable(table6_mf, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })
  output$table7_mf <- renderRHandsontable({
    table7_mf<-df_table7_mf()
    colnames(table7_mf)<-" "
    rownames(table7_mf)<-c("Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
    rhandsontable(table7_mf,rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })
  output$table8_mf <- renderRHandsontable({
    table8_mf<-df_table8_mf()
    colnames(table8_mf)<-"% Labor"
    rownames(table8_mf)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows")
    rhandsontable(table8_mf, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0%")
    
  })
  output$table9_mf <- renderRHandsontable({
    table9_mf<-df_table9_mf()
    colnames(table9_mf)<-"% Labor"
    rownames(table9_mf)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
    rhandsontable(table9_mf, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0%")
  })
  
  output$table6_com <- renderRHandsontable({
    table6_com<-df_table6_com()
    colnames(table6_com)<-" "
    rownames(table6_com)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture")
    rhandsontable(table6_com, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })
  output$table7_com <- renderRHandsontable({
    table7_com<-df_table7_com()
    colnames(table7_com)<-" "
    rownames(table7_com)<-c("Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
    rhandsontable(table7_com, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })
  output$table8_com <- renderRHandsontable({
    table8_com<-df_table8_com()
    colnames(table8_com)<-"% Labor"
    rownames(table8_com)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows")
    rhandsontable(table8_com, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0.00%")
    
  })
  output$table9_com <- renderRHandsontable({
    table9_com<-df_table9_com()
    colnames(table9_com)<-"% Labor"
    rownames(table9_com)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
    rhandsontable(table9_com, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0.00%")
  })
  
  output$table6_civ <- renderRHandsontable({
    table6_civ<-df_table6_civ()
    colnames(table6_civ)<-" "
    rownames(table6_civ)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture")
    rhandsontable(table6_civ, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })
  output$table7_civ <- renderRHandsontable({
    table7_civ<-df_table7_civ()
    colnames(table7_civ)<-" "
    rownames(table7_civ)<-c("Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
    rhandsontable(table7_civ, rowHeaderWidth=200,width=325,stretchH = "all")%>%
      hot_col(" ", format = "0.00%")
    
  })
  output$table8_civ <- renderRHandsontable({
    table8_civ<-df_table8_civ()
    colnames(table8_civ)<-"% Labor"
    rownames(table8_civ)<-c("Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows")
    rhandsontable(table8_civ, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0.00%")
  })
  output$table9_civ <- renderRHandsontable({
    table9_civ<-df_table9_civ()
    colnames(table9_civ)<-"% Labor"
    rownames(table9_civ)<-c("Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical")
    rhandsontable(table9_civ, rowHeaderWidth=150,width=325,stretchH = "all")%>%
      hot_col("% Labor", format = "0.00%")
  })
  observeEvent(input$table1a$changes$changes,{
    print("input$table1$changes$changes")
    user_input<-hot_to_r(input$table1a)
    user_input[4,1]<-user_input[1,1]+user_input[2,1]+user_input[3,1]
    df_constructionA(user_input)
    df_jobsA<-df_jobsA()
    df_jobsB(user_input[4,1]+df_jobsA[1,1]*1010)
    #df_constructionA<<-reactiveVal(user_input)
    
  })
  observeEvent(input$table1b$changes$changes,{
    print("input$table1$changes$changes")
    user_input<-hot_to_r(input$table1b)
    df_jobsA<<- reactiveVal(user_input)
    df_constructionA<-df_constructionA()
    df_jobsB(df_constructionA[4,1]+user_input[1,1]*1001)
    
  })
  
  observeEvent(input$table2$changes$changes,{
    print("input$table2$changes$changes")
    
    df_jobs_input = hot_to_r(input$table2)
    df_jobs_input =total_output(hot_to_r(input$table1),df_jobs_input )
    # print(df_jobs_input[1,1])
    
    #df_jobs_input[1,1]=df_jobs_input[1,1]+1000
    #print(df_jobs_input[1,])
    
    df_jobs(df_jobs_input)
    print("observe vent 4 JOBS")
    
  })
  observeEvent(input$table3$changes$changes,{
    print("input$table3$changes$changes")
    df_spending_input = hot_to_r(input$table3)
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
  
  observeEvent(input$table7$changes$changes,{
    print("In observe event table 7")
    df_table7_input = hot_to_r(input$table7)
    print("In observe event table 7 part 2")
    
    df_table7(df_table7_input)
  })
  
  observeEvent(input$table8$changes$changes,{
    print("In observe event table 8")
    df_table8_input = hot_to_r(input$table8)
    
    df_table8(df_table8_input)
    
  })
  observeEvent(input$table9$changes$changes,{
    print("In observe event table 9")
    df_table9_input = hot_to_r(input$table9)
    
    df_table9(df_table9_input)
    
  })
  
  #====Part2====
  
  
  observeEvent(input$table6_mf$changes$changes,{
    print("In observe event table 6_mf")
    df_table6_input = hot_to_r(input$table6_mf)
    df_table6_mf(df_table6_input)
  })
  
  observeEvent(input$table7_mf$changes$changes,{
    print("In observe event table 7_mf")
    df_table7_input = hot_to_r(input$table7_mf)
    print("In observe event table 7 part 2")
    
    df_table7_mf(df_table7_input)
  })
  
  observeEvent(input$table8_mf$changes$changes,{
    print("In observe event table 8_mf")
    df_table8_input = hot_to_r(input$table8_mf)
    
    df_table8_mf(df_table8_input)
    
  })
  observeEvent(input$table9_mf$changes$changes,{
    print("In observe event table 9_mf")
    df_table9_input = hot_to_r(input$table9_mf)
    
    df_table9_mf(df_table9_input)
    
  })
  #====Part3====
  
  
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
  observeEvent(input$table9_com$changes$changes,{
    print("In observe event table 9_com")
    df_table9_input = hot_to_r(input$table9)
    
    df_table9(df_table9_input)
    
  })
  
  
  #====Part4====
  
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
      data=data*.9
      architecture<-architecture*.9
      data[17]<-.1 #We're adding one more here. 
    }
    else
      data[17]<-0
    
    table6[1,1]<-architecture
    print("Button, Part 4")
    
    table6[2,1]<-data[1]
    table6[3,1]<-data[2]#as.numeric(2.03/100)
    table6[4,1]<-data[3]#as.numeric(8.68/100)
    table6[5,1]<-data[4]#as.numeric(.55/100)
    table6[6,1]<-data[5]#as.numeric(15.81/100)
    table6[7,1]<-data[6]#as.numeric(4.36/100)
    table6[8,1]<-data[7]#as.numeric(10.7/100)
    df_table6(table6)
    table7<-df_table7()
    table7[1,1]<-data[8]#as.numeric(0)
    table7[2,1]<-data[9]#as.numeric(.21/100)
    table7[3,1]<-data[10]#as.numeric(2.26/100)
    table7[4,1]<-data[11]#as.numeric(0)
    table7[5,1]<-data[12]#as.numeric(0)
    table7[6,1]<-data[13]#as.numeric(0)
    table7[7,1]<-data[14]#as.numeric(6.06/100)
    table7[8,1]<-data[15]#as.numeric(2.32/100)
    table7[9,1]<-data[16]#as.numeric(2.61/100)
    table7[10,1]<-data[17]
    df_table7(table7)
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
    df_table8(table8)
    table9<-df_table9()
    table9[1,1]<-as.numeric(35/100)
    table9[2,1]<-as.numeric(30/100)
    table9[3,1]<-as.numeric(0)
    table9[4,1]<-as.numeric(15/100)
    table9[5,1]<-as.numeric(15/100)
    table9[6,1]<-as.numeric(15/100)
    table9[7,1]<-as.numeric(60/100)
    table9[8,1]<-as.numeric(60/100)
    df_table9(table9)
    
  })
  
  observeEvent(input$button_mf,{
    print("We're in the dispersal section:")
    table1<-get_value_rehab(dtWithRadioButton_mf,1)
    table2<-get_value_rehab(dtWithRadioButton_mf,2)
    data<-as.matrix(getPercentages(table1,table2, 2))
    
    print("The data:")
    print(data)
    if(table2[3,1]==11){
      data=data*.9
      data[17]<-.1 #We're adding one more here. 
    }
    else
      data[17]<-0
    
    table6_mf<-df_table6_mf()
    table6_mf[1,1]<-as.numeric(8.33/100)
    table6_mf[2,1]<-data[1]#as.numeric(12.87/100)
    table6_mf[3,1]<-data[2]#as.numeric(1.99/100)
    table6_mf[4,1]<-data[3]#as.numeric(8.49/100)
    table6_mf[5,1]<-data[4]#as.numeric(.54/100)
    table6_mf[6,1]<-data[5]#as.numeric(15.46/100)
    table6_mf[7,1]<-data[6]#as.numeric(4.27/100)
    table6_mf[8,1]<-data[7]#as.numeric(10.46/100)
    df_table6_mf(table6_mf)
    table7_mf<-df_table7_mf()
    table7_mf[1,1]<-data[8]#as.numeric(14.24/100)
    table7_mf[2,1]<-data[9]#as.numeric(.21/100)
    table7_mf[3,1]<-data[10]#as.numeric(2.21/100)
    table7_mf[4,1]<-data[11]#as.numeric(0)
    table7_mf[5,1]<-data[12]#as.numeric(0)
    table7_mf[6,1]<-data[13]#as.numeric(0)
    table7_mf[7,1]<-data[14]#as.numeric(5.93/100)
    table7_mf[8,1]<-data[15]#as.numeric(2.27/100)
    table7_mf[9,1]<-data[16]#as.numeric(2.55/100)
    table7_mf[10,1]<-data[17]#as.numeric(2.55/100)
    df_table7_mf(table7_mf)
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
    df_table8_mf(table8_mf)
    table9_mf<-df_table9_mf()
    table9_mf[1,1]<-as.numeric(35/100)
    table9_mf[2,1]<-as.numeric(30/100)
    table9_mf[3,1]<-as.numeric(15/100)
    table9_mf[4,1]<-as.numeric(15/100)
    table9_mf[5,1]<-as.numeric(15/100)
    table9_mf[6,1]<-as.numeric(15/100)
    table9_mf[7,1]<-as.numeric(60/100)
    table9_mf[8,1]<-as.numeric(60/100)
    df_table9_mf(table9_mf)
    
  })
  
  
  observeEvent(input$button_com,{
    table6_com<-df_table6_com()
    print("We're in the dispersal section:")
    table1<-get_value_rehab(dtWithRadioButton_com,1)
    table2<-get_value_rehab(dtWithRadioButton_com,2)
    data<-as.matrix(getPercentages(table1,table2, 3))
    if(table2[3,1]==11){
      data=data*.9
      data[17]<-.1 #We're adding one more here. 
    }
    else
      data[17]<-0
    table6_com[1,1]<-as.numeric(8.33/100)
    table6_com[2,1]<-data[1]#as.numeric(12.87/100)
    table6_com[3,1]<-data[2]#as.numeric(1.99/100)
    table6_com[4,1]<-data[3]#as.numeric(8.49/100)
    table6_com[5,1]<-data[4]#as.numeric(.54/100)
    table6_com[6,1]<-data[5]#as.numeric(15.46/100)
    table6_com[7,1]<-data[6]#as.numeric(4.27/100)
    table6_com[8,1]<-data[7]#as.numeric(10.46/100)
    df_table6_com(table6_com)
    table7_com<-df_table7_com()
    table7_com[1,1]<-data[8]#as.numeric(14.24/100)
    table7_com[2,1]<-data[9]#as.numeric(.21/100)
    table7_com[3,1]<-data[10]#as.numeric(2.21/100)
    table7_com[4,1]<-data[11]#as.numeric(0)
    table7_com[5,1]<-data[12]#as.numeric(0)
    table7_com[6,1]<-data[13]#as.numeric(0)
    table7_com[7,1]<-data[14]#as.numeric(5.93/100)
    table7_com[8,1]<-data[15]#as.numeric(2.27/100)
    table7_com[9,1]<-data[16]#as.numeric(2.55/100)
    table7_com[10,1]<-data[17]#as.numeric(2.55/100)
    
    df_table7_com(table7_com)
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
    df_table8_com(table8_com)
    table9_com<-df_table9_com()
    table9_com[1,1]<-as.numeric(35/100)
    table9_com[2,1]<-as.numeric(30/100)
    table9_com[3,1]<-as.numeric(15/100)
    table9_com[4,1]<-as.numeric(15/100)
    table9_com[5,1]<-as.numeric(15/100)
    table9_com[6,1]<-as.numeric(15/100)
    table9_com[7,1]<-as.numeric(60/100)
    table9_com[8,1]<-as.numeric(60/100)
    df_table9_com(table9_com)
    
  })
  
  observeEvent(input$button_civ,{
    table6_civ<-df_table6_civ()
    print("We're in the dispersal section:")
    table1<-get_value_rehab(dtWithRadioButton,1)
    table2<-get_value_rehab(dtWithRadioButton,2)
    data<-as.matrix(getPercentages(table1,table2, 4))
    if(table2[3,1]==11){
      data=data*.9
      data[17]<-.1 #We're adding one more here. 
    }
    else
      data[17]<-0
    table6_civ[1,1]<-as.numeric(8.33/100)
    table6_civ[2,1]<-data[1]#as.numeric(12.87/100)
    table6_civ[3,1]<-data[2]#as.numeric(1.99/100)
    table6_civ[4,1]<-data[3]#as.numeric(8.39/100)
    table6_civ[5,1]<-data[4]#as.numeric(.54/100)
    table6_civ[6,1]<-data[5]#as.numeric(18.55/100)
    table6_civ[7,1]<-data[6]#as.numeric(4.27/100)
    table6_civ[8,1]<-data[7]#as.numeric(10.46/100)
    df_table6_civ(table6_civ)
    table7_civ<-df_table7_civ()
    table7_civ[1,1]<-data[8]#as.numeric(14.24/100)
    table7_civ[2,1]<-data[9]#as.numeric(.21/100)
    table7_civ[3,1]<-data[10]#as.numeric(2.21/100)
    table7_civ[4,1]<-data[11]#as.numeric(0)
    table7_civ[5,1]<-data[12]#as.numeric(0)
    table7_civ[6,1]<-data[13]#as.numeric(0)
    table7_civ[7,1]<-data[14]#as.numeric(5.93/100)
    table7_civ[8,1]<-data[15]#as.numeric(2.27/100)
    table7_civ[9,1]<-data[16]#as.numeric(2.55/100)
    table7_civ[10,1]<-data[17]#as.numeric(2.55/100)
    
    df_table7_civ(table7_civ)
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
    df_table8_civ(table8_civ)
    table9_civ<-df_table9_civ()
    table9_civ[1,1]<-as.numeric(35/100)
    table9_civ[2,1]<-as.numeric(30/100)
    table9_civ[3,1]<-as.numeric(15/100)
    table9_civ[4,1]<-as.numeric(15/100)
    table9_civ[5,1]<-as.numeric(15/100)
    table9_civ[6,1]<-as.numeric(15/100)
    table9_civ[7,1]<-as.numeric(60/100)
    table9_civ[8,1]<-as.numeric(60/100)
    df_table9_civ(table9_civ)
    
  })
  
  
  observeEvent(input$table3$changes$changes,{
    print("In observe event 1")
    df_spending_input = hot_to_r(input$table3)
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
  
  
}