complete <- function(directory, id =1:332) {
  
  nobs = numeric()
  ids = numeric()
  
  for (monitor in id){
    
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
    nobs <- c(nobs, nrow(completeRows))
    ids <- c(ids, monitor)
  }
  
  data.frame(ids, nobs)
  }