pollutantmean <- function(directory, pollutant, id = 1:332) {
  totalsum <- 0
  totalrows <- 0
  
  for(monitor in id){
  
    prefix <- ""
    if (monitor < 10) {
      prefix <- "00"
    } else if (monitor < 100) {
      prefix <- "0"
    }
    
    path <- paste(directory, "/", prefix, monitor, ".csv", sep = "")
    contents <- read.csv(path)
    
    colInContents <- contents[,pollutant]
    colInContents <- na.omit(colInContents)
    
    sum <- sum(colInContents)
    totalsum <- totalsum + sum
    rows <- length(colInContents)
    totalrows <- totalrows + rows
  }
  realmean <- totalsum / totalrows
  realmean
}
