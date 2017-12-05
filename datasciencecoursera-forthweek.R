best <- function(state, outcome) {
   ## Read outcome data
   data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   ## Check that state and outcome are valid
   statelist <- data[,'State']
   if(!state %in% statelist) {
     err('invalid state')
   }
   ## rate
   sorted <- NULL
   col_name <- NULL
   if(identical(outcome, "heart attack")) {
     col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }else if(identical(outcome, "heart failure")) {
      col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }else if(identical(outcome, "pneumonia")) {
      col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
   }else {
     err('invalid outcome')
   }
   newdata2 <- subset(data, State == state, select=c(col_name, "Hospital.Name"))
   newdata2[,1] <- suppressWarnings(as.numeric(as.character(newdata2[,1])))
   ## Return hospital name in that state with lowest 30-day death
   newdata2 <- newdata2[order(newdata2[,1], newdata2[,2]),]
   newdata2[1, 'Hospital.Name']
}
