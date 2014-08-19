corr <- function(directory, threshold = 0) {
  corSet <- numeric()
  
  for (monitor in 1:332) {
   
    prefix <- ""
    if (monitor < 10) {
      prefix <- "00"
    } else if (monitor < 100) {
      prefix <- "0"
    }
    
    path <- paste(directory, "/", prefix, monitor, ".csv", sep = "")
    contents <- read.csv(path)
   
   logicalContents <- complete.cases(contents)
   completeRows <- contents[logicalContents,]
   
   if(nrow(completeRows) > threshold) {
    icor <- cor(completeRows[, "nitrate"], completeRows[, "sulfate"]) 
    corSet <- c(corSet, icor)
   }
  }
  corSet
}