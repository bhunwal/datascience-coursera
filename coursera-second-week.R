pollutantmean <- function(directory, pollutant, id=1:332) {
  directory = paste(sep = "", directory, "/", sprintf("%03d.csv", 1:332))
  temp <- c()
  for(i in id) {
    myData <- read.csv(directory[i])
    my_ny <- myData[pollutant]
    notNa <- my_ny[!is.na(my_ny)]
    temp = c(temp, notNa)
  }
  mean(temp)
}

complete <- function(directory, id=1:332) {
  directory = paste(sep = "", directory, "/", sprintf("%03d.csv", 1:332))
  ids <- c()
  nobs <- c()
  for(i in id) {
    myData <- read.csv(directory[i])
    subset <-  myData[complete.cases(myData), ] 
    ids <- c(ids, i)
    nobs <- c(nobs, nrow(subset))
  }
  df = data.frame(ids, nobs)
  colnames(df) <- c("ids", "nobs")
  df
}

corr <-function(directory, threshhold=0) {
  directory2 = paste(sep = "", directory, "/", sprintf("%03d.csv", 1:332))
  cor_mat <- c()
  for(i in 1:332) {
    if(complete(directory, i)[1,"nobs"]>= threshhold) {
      myData <- read.csv(directory2[i])
      subset <- myData[complete.cases(myData),]
      cor_mat <- c(cor_mat, cor(subset[,"sulfate"], subset[,"nitrate"]))
    }
  }
  cor_mat
}
