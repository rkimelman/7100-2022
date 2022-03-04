library(humdrumR)
library(stringr)
library(tidyr)
mcf <- readHumdrum('.*rap')
spinePipe(mcf, 2:8, 1) -> mcf[rev(c('Stress', 'Tone', 'Break', 'Rhyme', 'IPA', 'Lyrics', 'Hype'))]
mcf$Token %hum>% c(~segments(Break %in% c('3', '4','5')), by ~ File) -> mcf$Phrase
mcf$Token %hum<% c(~list(paste(Lyrics, collapse = ' ')), by ~ File ~ Phrase)
rhymeSchemes <- mcf$Token %hum<% c(~list(Rhyme), by ~ File ~ Phrase)
lyrics <- mcf$Token %hum<% c(~list(Lyrics), by ~ File ~ Phrase)
text <- function(data, nullTokens = TRUE){
  if(nullTokens == FALSE){
    # if the user does not want null tokens to replace instances of syllables occurring after the first syllable of a multi-syllable word
    # data <- as.data.frame(data)
    # transform character vector to data frame
    data <- toString(data)
    # transform character vector to string *seems as though we might not need to transform to data frame above then*
    data <- str_replace_all(data, "-, -", "")
    # remove all instances of -, -, which represents a space between two syllables which when combined form a word
    data <- str_replace_all(data, ",", "")
    # remove all instances of , which occur after every word except the last one
    data <- as.list(strsplit(data, '\\s+')[[1]])
    # get all of the words as a list
    transpose1 <- t(data)
    transpose2 <- t(transpose1)
    data <- as.character(transpose2)
    # transform the data further to get desired character vector
  }
  else{
    # if the user does want null tokens to replace instances of syllables occurring after the first syllable of a multi-syllable word
    wordAddSpace <- function(value){
      # create function to be used later which will either print TRUE or FALSE, depending on if there is a - at the beginning of a syllable
      # when we split up syllables from multi-syllable words, some will either have - at the beginning or - at the end.
      if(substr(value,1,1) == "-"){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    replaceWithNullToken <- function(booleanValue){
      # create a function for replacing cells with null tokens, to be used in an apply function later
      if(booleanValue == TRUE){
        return(".")
      }
      else{
        return("word")
        # return "word" for identification/logic purposes later
      }
    }
    saveData <- data
    # save current character vector ("data") in a new variable to be used later
    data <- as.data.frame(data)
    # transform character vector ("data") into a data frame
    save <- apply(data, 1, function(x){wordAddSpace(x)})
    # for each row value in the data frame (which in this case corresponds to a syllable) determine if this is a row that needs to be deleted in the future by returning TRUE for that
    # index
    save <- as.data.frame(save)
    # go through and if true then add space below
    save2 <- apply(save, 1, function(x){replaceWithNullToken(x)})
    # save a vector with null tokens in the correct spots, and the spots that will be filled with words are each labeled "word"
    save2 <- as.data.frame(save2)
    # transform to data frame
    saveWords <- text(saveData, nullTokens = FALSE)
    # run this text function with null tokens = false to get the full words of the character vector
    saveWords <- as.data.frame(saveWords)
    # transform these words into a data frame
    newFunction <- function(dataValue, rowValue){
      rowValueToString <- toString(rowValue)
      # save given row value as a string to be used below
      dataValue[rowValue,1] <- paste(dataValue[rowValue,1], rowValueToString, sep = "")
      # paste the row value to each word for parsing and reading into final data frame/character vector later
      return(dataValue[rowValue,1])
    }
    newFunction2 <- function(findRowValues, iteration){
      getRowValueFinal <- sub("word*", "", findRowValues[iteration,1])
      # based on the row values of each word, replace each "word" with the row value corresponding to a word
      return(getRowValueFinal)
    }
    newFunction4 <- function(iterate, final, wordsArray){
      iterateToString <- toString(iterate)
      # save iteration value as a string for comparison to what is in the data frame/character vector
      if(iterateToString %in% final){
        # comparison for each cell
        return(wordsArray[match(iterate,final),1])
        # return corresponding word
      }
      else{
        return(".")
        # return null token if no match
      }
    }
    numbers <- 1:nrow(save2)
    # number of rows to go through in apply function
    numbers <- as.data.frame(numbers)
    # save as data frame so the apply function can work properly
    saveNew <- apply(numbers, 1, function(x){newFunction(save2,x)})
    # apply function to paste row values to each word
    saveNew <- as.data.frame(saveNew)
    # save as data frame for parsing below
    saveNew <- saveNew[!grepl(".", saveNew$saveNew, fixed = TRUE),]
    # save values that are not null tokens
    finalData <- numbers
    # final data will have size nrows
    finalWordsLength <- 1:nrow(saveWords)
    # final words length will have size of total words
    finalWordsLength <- as.data.frame(finalWordsLength)
    # save as data frame for use in apply function
    saveNewDataFrame <- as.data.frame(saveNew)
    # save as data frame for use in apply function
    finalData <- apply(finalWordsLength, 1, function(x){newFunction2(saveNewDataFrame, x)})
    # apply function to replace words with row values
    finalDataComplete <- apply(numbers, 1, function(x){newFunction4(x, finalData, saveWords)})
    # apply function to match row numbers of words with corresponding words
    data <- (unlist(finalDataComplete))
    # unlist to save as character vector
  }
  return(data)
}
iteration <- 1:length(lyrics)
iteration <- as.data.frame(iteration)
save <- cbind(lyrics)
lyricsText <- apply(iteration, 1, function(x){
  text(save[x,1]$lyrics, nullTokens = FALSE)}
  )
lyricsData <- as.data.frame(cbind(lyricsText))
write.csv(lyricsData,"Path to export the DataFrame\\File Name.csv", row.names = FALSE)