library("xlsx")
library("readxl")


options(java.parameters = "-Xmx8000m")
rehab_table<<-as.data.frame(matrix(0, nrow = 404, ncol = 4))
counter<<-0
#Trial Period
#========================================
#rownames(table_1)<-c("Doors and Windows","Finishes","Specialties","Equipment","Furnishings","Special Construction","Conveying Systems","Mechanical","Electrical", "Attorney/ Other")
detailed_industry<<-0
rehab_output<<-0
rh_key_last<<-0


initial_detailed_industry <- function(state_variable="AL", CDF=0) {
  #Input$Start is the ID of the "Generate" button.  
  print("We're in intital_detailed_Industry: A")
  wb2<<-read_excel("recon_info.xlsx")
  print("We're in intital_detailed_Industry: B")
  rehab_output<<-cbind(sapply(wb2[,1],as.numeric),wb2[,2],0)
  print("We're in intital_detailed_Industry: C")
  
  colnames(rehab_output)<<-c("REcon","Industries","Output")
  return()
}

rehab_calculations <- function(rehab_output2,combined_table, j, sheetIndex) {
  
  print("We're in rehab")
  rehab_output<-rehab_output2
  j<-100*j
  print(c("This is ", j))
  rehab_table<-get_tables(sheetIndex)
  #print("Rehab Table ")
  print(rehab_table)
  counter<<-0
  for (i in 1:dim(rehab_table)[1])
  {
    # print(c("First half ",j/100))
    # print(c("Second half ",floor(rehab_table[i,1]/100)))
    
    if(j/100<floor(rehab_table[i,1]/100)){
      # print(c("We did this calc: ", counter, " times"))
      
      return(rehab_output)
    }
    
    if(j==rehab_table[i,1])
    {
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
      counter<<-counter+1
      j<-rehab_table[i,1]
      bin<-unlist(strsplit(as.character(rehab_table[i,4]),","))
      rh_key_last<<-as.numeric(substr(rehab_table[i,1], 4, 4)) #Gets very last number of RH Key
      value_to_be_assigned<-combined_table[rh_key_last+1,]*rehab_table[i,3]/length(bin) #input value*rh_key value/ how many recon slots there are for it
      for(a in bin){
        print("B")
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



#=========================================

get_tables <- function(sheetIndex) {
  rehab_table<<-read_excel("rehab_recon.xlsx", sheet=sheetIndex)#,sheetIndex=1)#, sheetIndex=1, header=TRUE)
  rehab_table[,1]<<-NULL
  return(rehab_table)
}

#initial_detailed_industry()
#get_tables()
#rehab_calculations(rehab_table, rehab_output, table_1,1100)