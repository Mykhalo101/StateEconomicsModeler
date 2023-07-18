

tourismCalculator <- function(tourismData,rehab_output, value)
{
  for(i in 1:dim(tourismData)[1] )
  {
  bin=unlist(strsplit(as.character(tourismData[i,3]),","))
  recon_value = tourismData[i,2]/length(bin)
  
  for(a in bin){  
    
    rehab_output[as.numeric(a),3] = rehab_output[as.numeric(a),3]+(recon_value*value)/1000
  
  }
  }
  return (rehab_output)
  
}

#getTables <- function()
#{
  
  
  
  
#}