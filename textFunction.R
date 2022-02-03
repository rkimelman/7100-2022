values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
dummyData <- data.frame(values)
# test on dummyData, then implement into function
save_value <- c()
save_initial_row <- 0
save_word <- ""
save_value <- c()
iteration <- 1
for (i in iteration:nrow(dummyData)){
  splitString <- strsplit(dummyData[i, 1], "")[[1]]
  if(splitString[length(splitString)] == '-' ||  splitString[1] == '-'){
    if(splitString[length(splitString)] == '-' && splitString[1] != '-') {
      save_initial_row = i
      save_value <- append(save_value, splitString)
      splitStringBelow <- strsplit(dummyData[i+1, 1], "")[[1]]
      save_value <- append(save_value, splitStringBelow, "")[[1]]
      print(save_value)
      if(splitStringBelow[length(splitStringBelow)] != '-'){
        # save_value <- append(save_value, splitStringBelow, "")[[1]])
        save_value <- save_value[save_value != "-"]
        save_word <- paste(save_value, collapse = '')
        dummyData[save_initial_row,] <- save_word
        dummyData[save_initial_row+1,] <- "."
        iteration = i + 1
        save_value <- c()
      }
      else{
        dummyData[save_initial_row+1,] <- "."
        iteration <- i+1
      }
    }
    if(splitString[length(splitString)] == '-' &&  splitString[1] == '-'){
      save_value <- append(save_value, strsplit(dummyData[i+1, 1], "")[[1]])
      dummyData[i,] = "."
    }
    if(splitString[length(splitString)] != '-' && splitString[1] == '-') {
      dummyData[i,] = "."
      save_value <- save_value[save_value != "-"]
      save_word <- paste(save_value, collapse = '')
      dummyData[save_initial_row,] = save_word
      iteration = i + 1
      save_value <- c()
    }
  }
}
print(dummyData)

values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
dummyData <- data.frame(values)

text <- function(data, iteration){
  save_initial_row <- 0
  save_word <- ""
  save_value <- c()
  iteration <- 1
  for (i in iteration:nrow(data)){
    splitString <- strsplit(data[i, 1], "")[[1]]
    print(splitString)
    if(splitString[length(splitString)] == '-' ||  splitString[1] == '-'){
      if(splitString[length(splitString)] == '-' && splitString[1] != '-') {
        save_initial_row = i
        save_value <- append(save_value, splitString)
        splitStringBelow <- strsplit(data[i+1, 1], "")[[1]]
        save_value <- append(save_value, splitStringBelow)
        print(splitStringBelow)
        if(splitStringBelow[length(splitStringBelow)] != '-'){
          # save_value <- append(save_value, splitStringBelow, "")[[1]])
          save_value <- save_value[save_value != "-"]
          save_word <- paste(save_value, collapse = '')
          data[save_initial_row,] <- save_word
          data[save_initial_row+1,] <- "."
          iteration = i + 1
          save_value <- c()
        }
        else{
          iteration <- i+1
        }
      }
      if(splitString[length(splitString)] == '-' &&  splitString[1] == '-'){
          save_value <- append(save_value, strsplit(data[i+1, 1], "")[[1]])
          data[i,] = "."
      }
      if(splitString[length(splitString)] != '-' && splitString[1] == '-') {
        data[i,] = "."
        save_value <- save_value[save_value != "-"]
        save_word <- paste(save_value, collapse = '')
        data[save_initial_row,] = save_word
        iteration = i + 1
        save_value <- c()
      }
    }
    }
  return(data)
}
