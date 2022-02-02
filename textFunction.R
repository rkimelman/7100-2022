values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
dummyData <- data.frame(values)
# test on dummyData, then implement into function
for (i in 1:nrow(dummyData)){
  splitString <- strsplit(dummyData[i, 1], "")[[1]]
  for(j in length(splitString)){
    print(1)
    if(j == 1){
      print(TRUE)
    }
  }
}



text <- function (data){
  for (i in 1:length(data)){
    splitString <- strsplit(data[i, 1], "")[[1]]
  }
}