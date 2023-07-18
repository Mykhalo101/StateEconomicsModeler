library(Rcpp)
library(openxlsx)
library(readxl)
library(xlsx)

CDF<<-0
state_variable<<-"AL"
wb_1<<-0
wb2<<-0
three_digit_categories<<-0
category<<-0
detailed_industry<<-0
df_construction<<-as.data.frame(matrix(1, nrow = 8, ncol = 1))


df_heritage<<- as.data.frame(matrix(0, nrow = 5, ncol = 2))

df_spending<<-as.data.frame(matrix(0, nrow = 3, ncol = 1))




initial_detailed_industry <- function(state_variable) {
  print("Initial Detailed Industry")
  detailed_industry<<-as.data.frame(matrix(0, nrow = 404, ncol = 3))
  
  #Input$Start is the ID of the "Generate" button.  
  temp=paste(as.vector(state_variable), "_vectors",".xlsx", sep="") 
 # print(c("Temp ",temp))
  #temp is the variable that holds the vectors file for whatever state that is selected by the user. 
  temp2<-paste("A_",tolower(as.vector(state_variable)),".xlsx", sep="")
  #print(c("Temp2 ",temp2))
  category<<- read.xlsx("AL_vectors.xlsx", sheetIndex=1, colIndex=5)#wb2[,5]#read.xlsx(wb,startRow=1,cols=5) #Good
  print(category)
  #print("category")
#  print(category[,1])
  rownames(detailed_industry)<<-category[,1]
  #print("Mission complete")
  #print(detailed_industry)
  #print("Pre CDF")
  CDF<<-read.xlsx("AL_vectors.xlsx", sheetIndex=1,startRow=1,colIndex=c(29,16,31))#colIndex=c(29,29,16,31))#wb2[,5]#read.xlsx(wb,startRow=1,cols=5) #Good
  #print("Past CDF")
  #CDF <<-read.xlsx(wb,startRow=1,cols=c(28,29,16,31))
  #print(CDF)
  return()
}

main_street <- function(detailed_industry,main_street) {
  print("Main Street")
  
  print("Enter main street sub")
  detailed_industry[25,1]<<-df_construction[2,1]
  detailed_industry[32,1]<<-df_construction[3,1]
  
  detailed_industry[31,1]<<-.5*df_construction[4,1]
  detailed_industry[33,1]<<-.5*df_construction[2,1]
  
  detailed_industry[279,1]<<-df_construction[7,1]
    
    
    
      

  return()
}

historic_museums <- function() {
  print("Historic Museums")
  
  colnames(df_spending)<<-"Historic Museum and Sites"
  rownames(df_spending)<<-c("Total Annual Spending","Total Capital Spending","Visitor's Revenues Generated")
  
  total_capital_spending<- df_spending[2,1]
  detailed_industry[370,1]<<-df_construction[7,1]
  
  total_annual_spending<-df_construction[1,1]  
  detailed_industry[261,1]<<-df_construction[7,1]
  print("Done")
  
  
}


heritage_tourism <- function() {
  print("Heritage Tourism")
  
  colnames(df_heritage)<<-c("Person Days","$/ Person-Day")
  rownames(df_heritage)<<-c("Initial", "Camping(%) ","Commercial Lodging(%)","Friends/Family (%)","Total Person Nights (%)")
  
  dollar_per_night_per_person<-1
  comm_lodging<- df_heritage[5,1]*dollar_per_night_per_person*df_heritage[3,1]
  #total person nights * dollar per night per person * commercial lodigng percentage
  detailed_industry[365,1]<<-df_construction[7,1] #Comm Lodging 
  
  camping<- df_heritage[5,1]*dollar_per_night_per_person*df_heritage[2,1]
  
  detailed_industry[364,1]<<-df_construction[7,1] #Camping
  
  other_retail<- df_heritage[1,1]*df_heritage[1,2]
  detailed_industry[291,1]<<-df_construction[7,1]   #Other Retial
   print("Done")
  
}

#Test
print("WOW")
#print(main_street)
#p#rint("WOW2")
#initial_detailed_industry (state_variable)
#main_street(detailed_industry,df_construction) 
#heritage_tourism()
#historic_museums()


