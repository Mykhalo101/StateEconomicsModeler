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
library(readxl)


wb2<-read_excel("C:/Users/Mykhalo Petrovskyy/Downloads/Lahr/PEIM3_Myk_2.0/recon_info.xlsx")#,rowIndex=3:408, colIndex=1:2)
wb2<-wb2[-c(1,2,3),]
wb2<-wb2[1:404,]
col1= sapply(wb2[,1],as.numeric)
rehab_output<-cbind(col1,wb2[,2],0)
combined_table<-as.data.frame(matrix(0, nrow = 18, ncol = 1))
colnames(combined_table)<-"A"
rownames(combined_table)<-c("Architecture and Engineering","General Requirements","Site Work","Concrete","Masonry","Metals","Wood and Plastic","Thermal and Moisture","Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
#rehab_table<<-get_tables()
answer<<-rehab_calculations(rehab_output, combined_table, 11,1)




rehab_calculations <- function(A,combined_table, j, sheetIndex) {
 
  print("We're in rehab")
  rehab_output<-A
  j<-100*j
  print(c("This is ", j))
  rehab_table<<-get_tables(sheetIndex)
  #print("Rehab Table ")
  print(rehab_table)
  counter<-0
  for (i in 1:dim(rehab_table)[1])
  {
    # print(c("First half ",j/100))
    # print(c("Second half ",floor(rehab_table[i,1]/100)))
    print("Dim number: ")
    print(dim(rehab_table)[1])
    if(j/100<floor(rehab_table[i,1]/100)){
      # print(c("We did this calc: ", counter, " times"))
      
      return(rehab_output)
    }
    print("rehab_table[i,1]")
    print(rehab_table[i,1])
    if(j==rehab_table[i,1])
    {
      print("We're in J loop")
      counter<<-counter+1
      bin=unlist(strsplit(as.character(rehab_table[i,4]),","))
      rh_key_last<-as.numeric(substr(rehab_table[i,1], 4, 4)) #Gets very last number of RH Key
      #value_to_be_assigned=combined_table[rh_key_last+1,]*rehab_table[i,3]/length(bin) #input value*rh_key value/ how many recon slots there are for it
      value_to_be_assigned<-rehab_table[i,3]/length(bin)
      
      for(a in bin){
        print("A")
        
        intermediary<-as.numeric(rehab_output[as.numeric(a),3])
        rehab_output[as.numeric(a),3]<-intermediary+value_to_be_assigned[1,1]
      #  rehab_output[as.numeric(a),3]<-rehab_output[as.numeric(a),3]
       # print("OG Value: ")
       # print(intermediary)
       # print("Value to be added:")
        print((value_to_be_assigned))
       # print("New Value: ")
       # print(rehab_output[as.numeric(a),3])
      }
      print("done")
      
    }
    
    
    else if (floor(j/100)==floor(rehab_table[i,1]/100)) 
    {
      print("B")
      counter<<-counter+1
      j<-rehab_table[i,1]
      bin<-unlist(strsplit(as.character(rehab_table[i,4]),","))
      rh_key_last<<-as.numeric(substr(rehab_table[i,1], 4, 4)) #Gets very last number of RH Key
      value_to_be_assigned<-rehab_table[i,3]/length(bin) #input value*rh_key value/ how many recon slots there are for it
      print("value to be assigned")
      print(value_to_be_assigned)
      for(a in bin){

        intermediary<-as.numeric(rehab_output[as.numeric(a),3])
        rehab_output[as.numeric(a),3]<-intermediary+value_to_be_assigned
        #rehab_output[as.numeric(a),3]<-rehab_output[as.numeric(a),3]
      }
    }
  # else {
  #    print("In else")
  #  }
  
    }
 # print(c("We did this calc: ", counter, " times"))
  print("End")
  print(rehab_output)
  return(rehab_output)
}

get_tables <- function(sheetIndex) {
  rehab_table<-read_excel("C:/Users/Mykhalo Petrovskyy/Downloads/Lahr/PEIM3_Myk_2.0/rehab_recon.xlsx", sheet=sheetIndex)#,sheetIndex=1)#, sheetIndex=1, header=TRUE)
  rehab_table[,1]<-NULL
  return(rehab_table)
}