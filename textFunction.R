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

values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
dummyData <- data.frame(values)
dummyData <- toString(dummyData[,1])
dummyData <- str_replace_all(dummyData, "-, -", "")
dummyData <- str_replace_all(dummyData, ",", "")
dummyData <- as.list(strsplit(dummyData, '\\s+')[[1]])
transpose1 <- t(dummyData)
transpose2 <- t(transpose1)
dummyData <- as.data.frame(transpose2)

wordAddSpace <- function(value){
  if(substr(value,1,1) == "-"){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
replaceWithNullToken <- function(booleanValue){
  if(booleanValue == TRUE){
    return(".")
  }
  else{
    return("word")
  }
}
values <- c('Now', 'let', 'me', 'wel-', '-come', 'e-', '-very-', '-bo-', '-dy', 'to', 'the', 'wild', 'wild', 'west.')
dummyData <- data.frame(values)
save <- apply(dummyData, 1, function(x){wordAddSpace(x)})
save <- as.data.frame(save)
# go through and if true then add space below
save2 <- apply(save, 1, function(x){replaceWithNullToken(x)})
save2 <- as.data.frame(save2)
saveWords <- text(dummyData, nullTokens = FALSE)

finalData <- apply(save2, 1, function(x){replaceWord(x, saveWords, replaceWord(x, saveWords, ))})

newFunction <- function(data, rowValue){
  rowValueToString <- toString(rowValue)
  data[rowValue,1] <- paste(data[rowValue,1], rowValueToString, sep = "")
  return(data[rowValue,1])
}
newFunction2 <- function(findRowValues, iteration){
  getRowValueFinal <- sub("word*", "", findRowValues[iteration,1])
  return(getRowValueFinal)
}
replaceWord <- function(template, wordsInput){
  if(grepl("word", template) == TRUE){
    return(template[])
  }
  else{
    currentWord <- "."
    iteration = iteration + 1
  }
  c(currentWord, iteration)
}
newFunction3 <- function(iterate, rowValueOfWord, dataOfWords, maxLength, newDataSet){
  if(iterate != rowValueOfWord){
    return(".")
  }
  newDataSet[rowValueOfWord, 1] <- dataOfWords[iterate, 1]
  return(newDataSet)
}
dataScore <- 1
newFunction4 <- function(iterate, final, wordsArray){
  iterateToString <- toString(iterate)
  if(iterateToString %in% final){
    return(wordsArray[match(iterate,final),1])
  }
  else{
    return(".")
  }
}
numbers2 <- 1:14
numbers2 <- as.data.frame(numbers2)
numbers2[finalData[5],1] <- saveWords[5,1]
numbers <- 1:nrow(save2)
numbers <- as.data.frame(numbers)
saveNew <- apply(numbers, 1, function(x){newFunction(save2,x)})
saveNew <- as.data.frame(saveNew)
saveNew <- saveNew[!grepl(".", saveNew$saveNew, fixed = TRUE),]
finalData <- numbers
finalWordsLength <- 1:nrow(saveWords)
finalWordsLength <- as.data.frame(finalWordsLength)
saveNewDataFrame <- as.data.frame(saveNew)
finalData <- apply(finalWordsLength, 1, function(x){newFunction2(saveNewDataFrame, x)})
finalDataComplete <- numbers
data1 <- numbers
finalDataComplete <- apply(numbers, 1, function(x){newFunction4(x, finalData, saveWords)})

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
