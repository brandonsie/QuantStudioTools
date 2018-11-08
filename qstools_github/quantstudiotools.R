
removeTopMatter <- function(table, keyword, rm.keyrow = TRUE){
  #removeTopMatter looks for occurance of keyword string in the first column of
  #table, and removes all rows leading up to that keyword. 
  #if rm.keyrow, then  keyword row is set as the column names and row is removed
  options(stringsAsFactors = FALSE)
  
  rowstart <- grep(keyword,table[,1])
  if(rm.keyrow){
    rm.range <- 1:rowstart
    names(table) <- table[rowstart,]
  } else{rm.range <- 1:(rowstart-1)}
  
  table <- table[-rm.range,]
  
  
  return(table)
}

meltCurvePeaks <- function(path){
  #meltCurvePeaks takes the "Melt Curve Raw Data" tab from a quantstudio xls
  #export and chooses the temperature for each well with the highest derivative
  
  options(stringsAsFactors = FALSE)
  mc.data <- xlsx::read.xlsx(path,sheetName = "Melt Curve Raw Data") #load
  mc.data <- removeTopMatter(mc.data,"Well") #remove top ugly rows
  
  #create new table with just well position and derivative peaks
  mc.peak <- data.frame(Well_Position = unique(mc2$`Well Position`),
                        MC_Temp = NA)
  mc.peak$MC_Temp <- sapply(1:nrow(mc.peak),function(x){
    well.data <- na.omit(
      mc.data[mc.data$`Well Position`==mc.peak$Well_Position[x],])
    well.data$Temperature[well.data$Derivative == max(well.data$Derivative)]
    })
  
  
}


