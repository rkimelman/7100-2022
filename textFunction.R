values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
dummyData <- data.frame(values)
# test on dummyData, then implement into function
save_value <- {}
for (i in 1:nrow(dummyData)){
  splitString <- strsplit(dummyData[i, 1], "")[[1]]
  if(splitString[length(splitString)] == '-'){
    save_value <- append(save_value, splitString)
  }
}



text <- function (data, iteration){
  save_value = {}
  for (i in range(iteration:nrow(data))){
    splitString <- strsplit(data[i, 1], "")[[1]]
    if(splitString[length(splitString)] == '-' &&  splitString[1] == '-'){
        save_value <- append(save_value, strsplit(data[i+1, 1], "")[[1]])
    }
    if(splitString[length(splitString)] == '-' && !splitString[1] == '-') {
        save_value <- append(save_value, splitString)
        save_value <- append(save_value, strsplit(data[i+1, 1], "")[[1]])
    }
    if(!splitString[length(splitString)] == '-' && splitString[1] == '-') {
      save_value <- append(save_value, splitString)
    }
    else{
      iteration = iteration + 1
    }
  }
}