library(shiny)
library(DT)
library(shinyWidgets)
library(readxl)

#For all the radio button tables (SF, MF, Civil, etc. )
splitTable<- function(radioTable, values){
  radioTableValues<-0
  radioTable<<-radioTable
  for (i in 1:3)
  {
    radioTableValues[i]<-strtoi(radioTable[[i]][2])
  }
  radioTableValues<- as.matrix(radioTableValues)
  rownames(radioTableValues)<-values
  return(radioTableValues)
}


get_m1_rehab<-function(){
  m <- matrix(
  as.character(1:6), nrow = 3, ncol = 2, byrow = TRUE,
  dimnames = list(c("Types of System Repair","Amount of Interior Work Needed", "Amount of Site Work Needed"), c("Standard", "Extensive"))
  )
  values<-c("Types of System Repair","Amount of Interior Work Needed", "Amount of Site Work Needed")
  m[1, 1] <- sprintf(
    '<input type="radio"  name="%s" value="%s" %s/> ',
    values[1], m[1, 1], ifelse(1==1, 'checked="checked"', ""))
  m[1, 2] <- sprintf(
    '<input type="radio"  name="%s" value="%s" %s/> ',
    values[1], m[1, 2], ifelse(2==1, 'checked="checked"', ""))
  m[2, 1] <- sprintf(
    '<input type="radio"  name="%s" value="%s" %s/> ',
    values[2], m[2, 1], ifelse(1==1, 'checked="checked"', ""))
  m[2, 2] <- sprintf(
    '<input type="radio" name="%s" value="%s" %s/> ',
    values[2], m[2, 2], ifelse(2==1, 'checked="checked"', ""))
  m[3, 1] <- sprintf(
    '<input type="radio"name="%s" value="%s" %s/> ',
    values[3], m[3, 1], ifelse(1==1, 'checked="checked"', ""))
  m[3, 2] <- sprintf(
    '<input type="radio" name="%s" value="%s" %s/> ',
    values[3], m[3, 2], ifelse(2==1, 'checked="checked"', ""))
  
  return(m)
}

get_m1_rehab2<-function(){
  m <- matrix(
    as.character(7:12), nrow = 3, ncol = 2, byrow = TRUE,
    dimnames = list(c("Kind of Material Used","Amount of Exterior Work Needed", "Include Soft Costs"), c("\t","\t" ))
  )
  values<-c("Kind of Material Used","Amount of Exterior Work Needed", "Include Soft Costs")
  m[1, 1] <- sprintf(
    '<input type="radio" id="Masonry" name="%s" value="%s" %s/> <label for="Masonry">Masonry</label>',
    values[1], m[1, 1], ifelse(1==1, 'checked="checked"', ""))
  m[1, 2] <- sprintf(
    '<input type="radio"  id="Wood" name="%s" value="%s" %s/> <label for="Wood">Wood</label>',
    values[1], m[1, 2], ifelse(2==1, 'checked="checked"', ""))
  m[2, 1] <- sprintf(
    '<input type="radio"  id="Stnd" name="%s" value="%s" %s/> <label for="Stnd">Standard</label>',
    values[2], m[2, 1], ifelse(1==1, 'checked="checked"', ""))
  m[2, 2] <- sprintf(
    '<input type="radio" id="Ext" name="%s" value="%s" %s/> <label for="Ext">Extensive</label>',
    values[2], m[2, 2], ifelse(2==1, 'checked="checked"', ""))
  m[3, 1] <- sprintf(
    '<input type="radio"id="Inc1" name="%s" value="%s" %s/> <label for="Inc1">Yes</label>',
    values[3], m[3, 1], ifelse(1==1, 'checked="checked"', ""))
  m[3, 2] <- sprintf(
    '<input type="radio" id="Inc2" name="%s" value="%s" %s/> <label for="Inc2">No</label>',
    values[3], m[3, 2], ifelse(2==1, 'checked="checked"', ""))
  return(m)
}



#===========================
getPercentages<-function(table1,table2, typeOfTable){
  #Returns default percentages based on radio buttons
  #typeOfTable as in 1= , 2= , 3= , 4= 
  return_data=0
  #What the return percentages will be in. 
  ulti_table<<-rbind(table1, table2)
  #Combining the two radio tables into one for easier analysis. 
  print(ulti_table)
  rehab_values<<-read_excel("the_rehab_values.xlsx",sheet=typeOfTable)
  return(getPercentages_Boolean(rehab_values))
   
}#end


getPercentages_Boolean<-function(rehab_values){
  #It does all the boolean checks for single, multi, civil etc. 
  if(ulti_table[1]==1)
  {
    #Standard Systems
    
    if(ulti_table[2]==3)
    {#Standard Interior
      print("STANDARD INTERIOR")
      if(ulti_table[3]==5)
      {#Standard Site Work
        if(ulti_table[4]==7&&ulti_table[5]==9)
        {#Masonry and Standard Exterior
          data<-rehab_values[,1]
        }
        else if(ulti_table[4]==7&&ulti_table[5]==10)
        {#Masonry and Extensive Exterior
          data<-rehab_values[,2]
        }
        else if (ulti_table[4]==8&&ulti_table[5]==9)
        {#Wood and Standard Exterior
          data<-rehab_values[,3]
        }
        else 
        {#Wood and Ext. Exterior
          data<-rehab_values[,4]
        }
      }
      else
      {#Extensive Site Work
        #Standard Site Work
        if(ulti_table[4]==7&&ulti_table[5]==9)
        {#Masonry and Standard Exterior
          data<-rehab_values[,5]
        }
        else if(ulti_table[4]==7&&ulti_table[5]==10)
        {#Masonry and Extensive Exterior
          data<-rehab_values[,6]
        }
        else if (ulti_table[4]==8&&ulti_table[5]==9)
        {#Wood and Standard Exterior
          data<-rehab_values[,7]
        }
        else 
        {#Wood and Ext. Exterior
          data<-rehab_values[,8]
        }
      }#Ends here
    }#end
    else 
    {#Extensive Interior 
      if(ulti_table[3]==5)
      {#Standard Site Work
        if(ulti_table[4]==7&&ulti_table[5]==9)
        {#Masonry and Standard Exterior
          data<-rehab_values[,9]
        }
        else if(ulti_table[4]==7&&ulti_table[5]==10)
        {#Masonry and Extensive Exterior
          data<-rehab_values[,10]
        }
        else if (ulti_table[4]==8&&ulti_table[5]==9)
        {#Wood and Standard Exterior
          data<-rehab_values[,11]
        }
        else 
        {#Wood and Ext. Exterior
          data<-rehab_values[,12]
        }
      }
      else
      {#Extensive Site Work
        #Standard Site Work
        if(ulti_table[4]==7&&ulti_table[5]==9)
        {#Masonry and Standard Exterior
          data<-rehab_values[,13]
        }
        else if(ulti_table[4]==7&&ulti_table[5]==10)
        {#Masonry and Extensive Exterior
          data<-rehab_values[,14]
        }
        else if (ulti_table[4]==8&&ulti_table[5]==9)
        {#Wood and Standard Exterior
          data<-rehab_values[,15]
        }
        else 
        {#Wood and Ext. Exterior
          data<-rehab_values[,16]
        }
      }#Ends here
    }#ENd End
  }
  else {
    #Extensive Systems
    
    if(ulti_table[2]==3)
    {#Standard Interior
      print("STANDARD INTERIOR")
      if(ulti_table[3]==5)
      {#Standard Site Work
        if(ulti_table[4]==7&&ulti_table[5]==9)
        {#Masonry and Standard Exterior
          data<-rehab_values[,17]
        }
        else if(ulti_table[4]==7&&ulti_table[5]==10)
        {#Masonry and Extensive Exterior
          data<-rehab_values[,18]
        }
        else if (ulti_table[4]==8&&ulti_table[5]==9)
        {#Wood and Standard Exterior
          data<-rehab_values[,19]
        }
        else 
        {#Wood and Ext. Exterior
          data<-rehab_values[,20]
        }
      }
      else
      {#Extensive Site Work
        #Standard Site Work
        if(ulti_table[4]==7&&ulti_table[5]==9)
        {#Masonry and Standard Exterior
          data<-rehab_values[,21]
        }
        else if(ulti_table[4]==7&&ulti_table[5]==10)
        {#Masonry and Extensive Exterior
          data<-rehab_values[,22]
        }
        else if (ulti_table[4]==8&&ulti_table[5]==9)
        {#Wood and Standard Exterior
          data<-rehab_values[,23]
        }
        else 
        {#Wood and Ext. Exterior
          data<-rehab_values[,24]
        }
      }#Ends here
    }#end
    else 
    {#Extensive Interior 
      if(ulti_table[3]==5)
      {#Standard Site Work
        if(ulti_table[4]==7&&ulti_table[5]==9)
        {#Masonry and Standard Exterior
          data<-rehab_values[,25]
        }
        else if(ulti_table[4]==7&&ulti_table[5]==10)
        {#Masonry and Extensive Exterior
          data<-rehab_values[,26]
        }
        else if (ulti_table[4]==8&&ulti_table[5]==9)
        {#Wood and Standard Exterior
          data<-rehab_values[,27]
        }
        else 
        {#Wood and Ext. Exterior
          data<-rehab_values[,28]
        }
      }
      else
      {#Extensive Site Work
        #Standard Site Work
        if(ulti_table[4]==7&&ulti_table[5]==9)
        {#Masonry and Standard Exterior
          data<-rehab_values[,29]
        }
        else if(ulti_table[4]==7&&ulti_table[5]==10)
        {#Masonry and Extensive Exterior
          data<-rehab_values[,30]
        }
        else if (ulti_table[4]==8&&ulti_table[5]==9)
        {#Wood and Standard Exterior
          data<-rehab_values[,31]
        }
        else 
        {#Wood and Ext. Exterior
          data<-rehab_values[,32]
        }
      }#Ends here
    }#ENd End
  }

  return(data)
}

