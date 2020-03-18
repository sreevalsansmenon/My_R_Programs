pollutantmean <- function(directory, pollutant, id = 1:332){
  fil <- sprintf('%03d.csv', id)
  file <- paste("/Users/sreevalsansmenon/Downloads", directory, fil, sep="/")  
  value <- do.call(rbind, lapply(file, read.csv))
  if (pollutant == "sulfate"){
    values =  value[, 2]
  } else
    values = value[, 3]
  mean_val = mean(values[!is.na(values)])
  mean_val
}

complete <- function(directory, id= 1:332){
  fil <- sprintf('%03d.csv', id)
 file <- paste("/Users/sreevalsansmenon/Downloads", directory, fil, sep="/")  
 value <- do.call(rbind, lapply(file, read.csv))
 idx <- is.na(value[,2]) | is.na(value[,3])
 data <- as.data.frame(table(value[!idx,4]))
 colnames(data) <- c("id", "nobs")
 data
}

corr <- function(directory, threshold = 0){
  corrv <- c()
  for (i in 1:332) {
    fil <- sprintf('%03d.csv', i)
    file <- paste("/Users/sreevalsansmenon/Downloads", directory, fil, sep="/")  
    value <- read.csv(file, header = TRUE)
    idx = is.na(value[,2]) | is.na(value[,3])
    if(sum(!idx)>threshold){
      corrv <- rbind(corrv, cor(value[!idx,2],value[!idx,3]))
    }
  }
 corrv
}
