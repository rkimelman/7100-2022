library(stringr)
values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
dummyData <- data.frame(values)
dummyData <- toString(dummyData[,1])
dummyData <- str_replace_all(dummyData, "-, -", "-")
word_count <- str_count(dummyData, '\\w+')
locations_add_spaces <- str_locate_all(dummyData, "-")
dummyDataList <- as.list(strsplit(dummyData, "")[[1]])
# access each character in data
save <- str_locate_all(dummyData, "-")
# find locations of each - signifying there needs to be a token added beneath the word
substr(dummyData, 1, 1)
# word count also: lengths(gregexpr("\\W+", dummyData))
dummyData <- str_replace_all(dummyData, ",", "")
dummyData <- as.list(strsplit(dummyData, '\\s+')[[1]])
dummyData <- str_replace_all(dummyData, "(\\b\\w)", ' \\1')


transpose1 <- t(dummyData)
transpose2 <- t(transpose1)
dummyData <- as.data.frame(transpose2)

text <- function(data, nullTokens = TRUE){
  if(nullTokens == FALSE){
    data <- toString(data[,1])
    data <- str_replace_all(data, "-, -", "")
    data <- str_replace_all(data, ",", "")
    data <- as.list(strsplit(data, '\\s+')[[1]])
    transpose1 <- t(data)
    transpose2 <- t(transpose1)
    data <- as.data.frame(transpose2)
  }
  else{
    save_initial_row <- 0
    save_word <- ""
    save_value <- c()
    iteration <- 1
    for (i in iteration:nrow(data)){
      splitString <- strsplit(data[i, 1], "")[[1]]
      if(splitString[length(splitString)] == '-' ||  splitString[1] == '-'){
        if(splitString[length(splitString)] == '-' && splitString[1] != '-') {
          save_initial_row = i
          save_value <- append(save_value, splitString)
          splitStringBelow <- strsplit(data[i+1, 1], "")[[1]]
          save_value <- append(save_value, splitStringBelow)
          if(splitStringBelow[length(splitStringBelow)] != '-'){
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
  }
  return(data)
}

text(dummyData)

textVectorized <- function(data, current_iteration, nullTokens = TRUE){
  splitString <- strsplit(data[current_iteration, 1], "")[[1]]
  if(splitString[length(splitString)] == '-' ||  splitString[1] == '-'){
    if(splitString[length(splitString)] == '-' && splitString[1] != '-') {
      save_initial_row = current_iteration
      save_value <- append(save_value, splitString)
      splitStringBelow <- strsplit(data[current_iteration+1, 1], "")[[1]]
      save_value <- append(save_value, splitStringBelow)
      if(splitStringBelow[length(splitStringBelow)] != '-'){
        save_value <- save_value[save_value != "-"]
        save_word <- paste(save_value, collapse = '')
        data[save_initial_row,] <- save_word
        data[save_initial_row+1,] <- "."
        current_iteration = current_iteration + 1
        save_value <- c()
      }
      else{
        current_iteration <- current_iteration+1
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
  return(data)
}
text(dummyData)
values <- c("ya'll", 'act', 'like', "you've", 'ne-', 'ver', 'seen', 'a', 'white', 'per-', 'son', 'be-', 'fore')
dummyData <- data.frame(values)
silbeFormat <- function(data){
  save_initials <- list()
  print_initial <- list()
  save_corrected <- list()
  print_corrected <- list()
  counter <- 0
  for(i in 1:nrow(data)){
    splitString <- strsplit(data[i, 1], "")[[1]]
    splitStringNext <- strsplit(data[i+1,1], "")[[1]]
    if(splitString[length(splitString)] == '-' && splitStringNext[1] != '-'){
      counter <- counter + 1
      print_initial <- append(data[i,1], data[i+1,1])
      save_initials[counter] <- list(print_initial)
      print_corrected <- append(data[i,1], '-')
      print_corrected <- append(print_corrected, data[i+1, 1])
      save_corrected[counter] <- list(print_corrected)
    }
  }
  if(counter == 0){
    return("Formatted properly.")
  }
  else{
#      print("error, improperly formatted **silbe:")
      return(cat("error, improperly formatted **silbe:", save_initials[1][[1]], "should be", save_corrected[1][[1]]))
      if(counter > 1){
        for(i in 2:counter){
          return(cat(" and", save_initials[i][[1]], "should be", save_corrected[i][[1]]))
        }
      }
  }
#  return(corrections)
}
silbeFormat(dummyData)
#print("error, improperly formatted **silbe: 'nev- er' should be 'nev- -er' and 'per- son' should be 'per- -son'")
