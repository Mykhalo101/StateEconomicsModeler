threeDigitIndustryGen <- function(GDP,totalEffectsJobs1,totalEffectsEarnings,df_3digit,GDP_woHH,totalEffectsJobs1_woHH,totalEffectsEarnings_woHH,totalEffectsOutput_woHH,three_digit_industry_woHH,totalEffectsOutput){
  #3-Digit Industries Function  
  #Import the proper column names and IO values. 
  
  aggregate2 <- function(matrixFit, col, df){
    df_superSector <- df
    #1 is Agric, Forestry, Fishing 
    df_superSector[2,col]  <- sum(matrixFit[1:10,])
    df_superSector[3,col]  <- sum(matrixFit[11:13,])
    #4 is Mining
    df_superSector[5,col]  <- sum(matrixFit[14,])
    df_superSector[6,col]  <- sum(matrixFit[15:19,])
    df_superSector[7,col]  <- sum(matrixFit[20:21,])

    df_superSector[8,col]  <- sum(matrixFit[22:24,])
    df_superSector[9,col]  <-  sum(matrixFit[25:36,])
    
    df_superSector[11,col]  <-  sum(matrixFit[192:220,])
    df_superSector[12,col]<- sum(matrixFit[221:226,])
    df_superSector[13,col]  <- sum(matrixFit[227:228,])
    df_superSector[14,col]  <- sum(matrixFit[37:40,])
    df_superSector[15,col]  <- sum(matrixFit[229:236,])
    df_superSector[16,col] <- sum(matrixFit[237:238,])
    df_superSector[17,col] <- sum(matrixFit[239:242,])
    df_superSector[18,col] <- sum(matrixFit[243:261,])
    df_superSector[19,col] <- sum(matrixFit[262:271,])
    df_superSector[20,col] <- sum(matrixFit[41:52,])
    df_superSector[21,col] <- sum(matrixFit[53:62,])
    df_superSector[22,col] <- sum(matrixFit[63:82,])
    df_superSector[23,col] <- sum(matrixFit[83:110,])
    df_superSector[24,col] <- sum(matrixFit[111:130,])
    df_superSector[25,col] <- sum(matrixFit[131:147,])
    df_superSector[26,col] <- sum(matrixFit[148:161,])
    df_superSector[27,col] <- sum(matrixFit[162:172,])
    df_superSector[28,col] <- sum(matrixFit[173:180,])
    df_superSector[29,col] <- sum(matrixFit[181:191,])
    df_superSector[30,col] <- sum(matrixFit[272:282,])
    df_superSector[31,col] <- sum(matrixFit[283:291,])
    #Transportation sum is 32. 
    df_superSector[33,col] <- sum(matrixFit[292,])
    df_superSector[34,col] <- sum(matrixFit[293,])
    df_superSector[35,col] <- sum(matrixFit[294,])
    df_superSector[36,col] <- sum(matrixFit[295,])
    df_superSector[37,col] <- sum(matrixFit[296,])
    df_superSector[38,col] <- sum(matrixFit[297,])
    df_superSector[39,col] <- sum(matrixFit[298:299,])
    df_superSector[40,col] <- sum(matrixFit[300,])
    #41 is INFO sum. 
    df_superSector[42,col] <- sum(matrixFit[301:305,])
    df_superSector[43,col] <- sum(matrixFit[306:307,])
    df_superSector[44,col] <- sum(matrixFit[308:309,])
    df_superSector[45,col] <- sum(matrixFit[310:315,])
    #46 is Finance and Insurance
    df_superSector[47,col] <- sum(matrixFit[316:317,])
    
    df_superSector[48,col] <- sum(matrixFit[318:319,])
    df_superSector[49,col] <- sum(matrixFit[320:322,])
    df_superSector[50,col] <- sum(matrixFit[323,])
    #51 is real estate
    df_superSector[52,col] <- sum(matrixFit[324:326,])
    df_superSector[53,col] <- sum(matrixFit[327:330,])
    #54 Professional and Tech 
    df_superSector[55,col] <- sum(matrixFit[331,])
    df_superSector[56,col] <- sum(matrixFit[335:344,])
    df_superSector[57,col] <- sum(matrixFit[332:334,])
    df_superSector[58,col] <- sum(matrixFit[345,])
    #59 is Admin and Waste
    df_superSector[60,col] <- sum(matrixFit[346:353,])
    df_superSector[61,col] <- sum(matrixFit[354,])
    df_superSector[62,col] <- sum(matrixFit[355:357,])
    #63 is health care
    df_superSector[64,col] <- sum(matrixFit[358:364,])
    df_superSector[65,col] <- sum(matrixFit[365:367,])
    df_superSector[66,col] <- sum(matrixFit[368:370,])
    #67 is Arts,Entertainment...
    df_superSector[68,col] <- sum(matrixFit[371:375,]) #371:378,379:382
    df_superSector[69,col] <- sum(matrixFit[376:378,])
    #70 is Accomadation and Food Serives
    df_superSector[71,col] <- sum(matrixFit[379,])
    df_superSector[72,col] <- sum(matrixFit[380:382,])
    
    df_superSector[73,col] <- sum(matrixFit[383:394,])
    df_superSector[74,col] <- sum(matrixFit[395:403,])
    #df_superSector[75,col] <- sum(matrixFit[390,])
    
    df_superSector[75,col] <- sum(df_superSector[1:74,col]) #Households?matrixFit[1:390,]
    
    df_superSector[1,col] <- sum(df_superSector[2:3,col])
    df_superSector[4,col] <- sum(df_superSector[5:7,col])
    df_superSector[10,col] <-sum(df_superSector[11:29,col])
    df_superSector[32,col] <-sum(df_superSector[33:40,col])
    df_superSector[41,col] <-sum(df_superSector[42:45,col])
    df_superSector[46,col] <-sum(df_superSector[47:50,col])
    df_superSector[51,col] <-sum(df_superSector[52:53,col])
    df_superSector[54,col] <-sum(df_superSector[55:58,col])
    df_superSector[59,col] <-sum(df_superSector[60:62,col])
    df_superSector[63,col] <-sum(df_superSector[64:66,col])
    df_superSector[67,col] <-sum(df_superSector[68:69,col])
    df_superSector[70,col] <-sum(df_superSector[71:72,col])
    return (df_superSector)
  }
  print("pssed df_3digit")
  #test<<-df_3digit
  df_3digit <-aggregate2((GDP),4, (aggregate2((totalEffectsJobs1),3,(aggregate2(((totalEffectsEarnings)),2,(aggregate2((totalEffectsOutput),1,df_3digit)))))))
 # test<<-df_3digit
  #This is the same process as in the previous function, except the aggregate function has way more indeces. 
  
  three_digit_industry_woHH<-aggregate2((GDP_woHH),4, (aggregate2((totalEffectsJobs1_woHH),2,(aggregate2(((totalEffectsEarnings_woHH)),3,(aggregate2((totalEffectsOutput_woHH),1,three_digit_industry_woHH)))))))
  
  colnames(df_3digit) <- c("Output", "Earnings", "Employment", "GDP")
  return(list(df_3digit,three_digit_industry_woHH))
  
}

