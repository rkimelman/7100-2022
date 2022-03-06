library(humdrumR)
library(stringr)
library(tidyr)

segments <- function(x, reverse = FALSE) {
  if (!is.logical(x)) x <- c(TRUE, head(x, -1L) != tail(x, -1L))
  if (reverse) x <- rev(x)
  
  x <- cumsum(x)
  
  if (reverse) {
    x <- rev(-x) + max(x) + 1
  }
  
  x
  
}

mcf <- readHumdrum('.*rap')
spinePipe(mcf, 2:8, 1) -> mcf[rev(c('Stress', 'Tone', 'Break', 'Rhyme', 'IPA', 'Lyrics', 'Hype'))]
mcf$Token %hum>% c(~segments(Break %in% c('3', '4','5')), by ~ File) -> mcf$Phrase
mcf$Token %hum<% c(~list(paste(Lyrics, collapse = ' ')), by ~ File ~ Phrase)
rhymeSchemes <- mcf$Token %hum<% c(~list(Rhyme), by ~ File ~ Phrase)
lyrics <- mcf$Token %hum<% c(~list(Lyrics), by ~ File ~ Phrase)
countFrequencies <- function(element){
  frequencyCount <- unlist(element)
  frequencyCount <- table(frequencyCount)
  return(frequencyCount)
}

recordMoreThanOneInstance <- function(table){
  saveList <- unlist(table)
  saveList <- as.data.frame(saveList)
  saveList <- t(saveList)
  determine <- which(saveList > 1)
  saveList <- as.data.frame(saveList)
  internalRhymes <- saveList[determine]
  return(internalRhymes)
}

recordFreqDifference <- function(table){
  if(NCOL(table$internalRhymesList) == 0){
    return(0)
  }
  # record frequency difference between non rhyming words and the most frequent internal rhyme
  df <- as.data.frame(table)
  nonRhymingWordFrequency <- df[,1]
  vector <- which(colnames(table$internalRhymesList) != ".")
  if(length(vector) >= 1){
    maxInternalRhyme <- min(table$internalRhymesList[,vector])
    maxInternalRhymeIndex <- which(table$internalRhymesList == maxInternalRhyme)
    difference <- nonRhymingWordFrequency - maxInternalRhyme
  }
  else{
    return(0)
  }
  return(difference)
}

# implement functions
iteration <- 1:length(rhymeSchemes)
iteration <- cbind(iteration)
df <- rbind(rhymeSchemes)
df <- as.data.frame(t(df))
frequencyTables <- apply(iteration, 1, function(x){countFrequencies(df[x,1])})
frequencyTablesDataFrame <- cbind(frequencyTables)
internalRhymesList <- apply(iteration, 1, function(x){recordMoreThanOneInstance(frequencyTablesDataFrame[x][1])})
internalRhymesListDataFrame <- rbind(internalRhymesList)
internalRhymesListDataFrame <- as.data.frame(internalRhymesListDataFrame)
save <- apply(iteration, 1, function(x){as.data.frame(internalRhymesListDataFrame[1,x])})
findMaxDifference <- apply(iteration, 1, function(x){recordFreqDifference(internalRhymesListDataFrame[1,x])})
maxDifference <- max(findMaxDifference)
tableWithMaxDifference <- which(findMaxDifference == maxDifference)
findTableWithMaxDifference <- frequencyTables[tableWithMaxDifference]

# test function
df2 <- as.data.frame(frequencyTables[[968]])
determine <- which(df2$Freq > 1)
internalRhymes <- df2$frequencyCount[determine]

# largest distance between two internal rhymes
# function
rowRhymeSchemes <- cbind(rhymeSchemes)
replaceWithRepeating <- function(string){
  save <- gsub("\\s*\\([^\\)]","s",as.character(string))
  save <- gsub("\\s*\\)","t",as.character(save))
  # this allows R to read these as repeating rhymes
  return(save)
}

getIndices <- function(pattern1, wholeString){
  if(length(pattern1) >= 1){
    iteration <- 1:length(pattern1)
    iteration <- cbind(iteration)
    save11 <- apply(iteration, 1, function(x){ 
      if(grepl("\\(", pattern1[x]) == TRUE){
        pattern1[x] <- paste(pattern1[x], ")", sep = "")
      }
      if(grepl("\\[", pattern1[x]) == TRUE){
        pattern1[x] <- paste(pattern1[x], "]", sep = "")
      }
      return(grep(pattern1[x], wholeString))
    }
    
    )
  }
  else{
    return(0)
  }
}

getIndicesOfLetters <- function(string){
  if(length(string)<1){
    return(0)
  }
  else{
    return(which(string != "."))
  }
}

# iteration <- 1:length(indicesOfLetters)
iterationForRhymes <- 1:length(rhymeSchemes)
iterationForRhymes <- cbind(iterationForRhymes)
letters <- apply(iterationForRhymes, 1, function(x){getIndicesOfLetters(rowRhymeSchemes[x,]$rhymeSchemes)})
letters <- cbind(letters)
getPatterns <- apply(iterationForRhymes, 1, function(x){return(rowRhymeSchemes[x,]$rhymeSchemes[letters[x,]$letters])}) 
replaceWithRepeatingPrint <- apply(iterationForRhymes, 1, function(x){replaceWithRepeating(getPatterns[[x]])})
renameRhymeSchemes <- apply(iterationForRhymes, 1, function(x){replaceWithRepeating(rowRhymeSchemes[x,]$rhymeSchemes)})
convertToVectors <- apply(iterationForRhymes, 1, function(x){return(as.vector(replaceWithRepeatingPrint[[x]]))})
convertToVectors2 <- apply(iterationForRhymes, 1, function(x){return(as.vector(getPatterns[[x]]))})

checkIfInternalRhyme <- function(pattern2, wholeString2){
  save3 <- list(FALSE)
  if(length(pattern2) >= 1){
    iteration <- 1:length(pattern2)
    iteration <- cbind(iteration)
      save3 <- apply(iteration, 1, function(x){ 
        if(grepl("\\(", pattern2[x]) == TRUE){
          pattern2[x] <- paste(pattern2[x], ")", sep = "")
        }
        if(grepl("\\[", pattern2[x]) == TRUE){
          pattern2[x] <- paste(pattern2[x], "]", sep = "")
        }
        if(length(grep(pattern2[x], wholeString2)) > 1){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
        
        )
    }
    else{
      return(FALSE)
    }
    if(any(save3) == TRUE){
      return(TRUE)
    }
}

checkIfInternalRhymePrint <- apply(iterationForRhymes, 1, function(x){checkIfInternalRhyme(convertToVectors2[[x]], rhymeSchemes[[x]])})

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

removeAmbiguousCharsFunc <- function(string){
  newString <- str_replace_all(string, "[.]", "")
  newString2 <- str_replace_all(newString, "[\\(]", "")
  newString3 <- str_replace_all(newString2, "[\\)]", "")
  newString4 <- str_replace_all(newString3, "[,]", "")
  newString5 <- str_replace_all(newString4, "[ ]", "")
  return(newString5)
}

calcDifferences <- function(rhymeScheme, letters, index){
  return(diff(grep(substr(letters, index, index), rhymeScheme)))
}

returnMax <- function(numberList){
  numbers <- unlist(numberList)
  return(max(numbers))
}

checkIfInternalRhymePrint2 <- nullToNA(checkIfInternalRhymePrint)
checkIfInternalRhymePrint3 <- cbind(checkIfInternalRhymePrint2)
checkIfInternalRhymePrint4 <- as.data.frame(checkIfInternalRhymePrint3)
checkIfInternalRhymePrint4[is.na(checkIfInternalRhymePrint4)] <- FALSE
indices <- which(checkIfInternalRhymePrint4 == TRUE)
internalRhymesFinal <- rhymeSchemes[indices]
uniqchars <- function(string1){ unique(unlist(strsplit(string1, "")))}
iteration <- 1:length(internalRhymesFinal)
iteration <- cbind(iteration)
findUniqueChars <- apply(iteration, 1, function(x){uniqchars(internalRhymesFinal[[x]])})
removeAmbiguousChars <- apply(iteration, 1, function(x){removeAmbiguousCharsFunc(toString(findUniqueChars[[x]]))})
lengthsOfLetters <- apply(iteration, 1, function(x){return(nchar(removeAmbiguousChars[[x]]))})
saveDifferencesFinal <- apply(iteration, 1, function(x){calcDifferences(internalRhymesFinal[[x]], removeAmbiguousChars[[x]], lengthsOfLetters[x])})
maxDistances <- apply(iteration, 1, function(x){returnMax(saveDifferencesFinal[[x]])})  
findMaxFinal <- max(maxDistances)
findIndexMaxFinal <- which(maxDistances == findMaxFinal)
maxInternalRhymeDifference <- internalRhymesFinal[[findIndexMaxFinal]]
meanSylb <- mean(unlist(saveDifferencesFinal))

# experiment
vowelPhonemes <- c("aa", "ae", "ah", "eh", "ey", "ih", "iy", "uw")
mappingsToData <- c("Q" , "\\{", "V", "E", "1", "I", "7", "u")
combo2 <- t(combn(vowelPhonemes, 2))
combo2 <- as.data.frame(combo2)
combo3 <- t(combn(vowelPhonemes, 3))
combo3 <- as.data.frame(combo3)

# obtain two random words with "aa" vowel sound

df <- read.csv(file = "pseudoworddata.csv")
dfNew <- df[which(df$X.1=="3"),,]
# use random configurations to generate words, but always make each word have at least 1 phonological neighbor
# should each rhyming word have same number of syllables, and vowels occur in same positions? can code for that
# generalize above
testFunction3Phonemes <- function(pronunciation){
  testIfPresent <- apply(iteration, 1, function(x){
    if(grepl(pronunciation, dfNew[x,2])){
      return(dfNew[x,2])
    }
    else{
      return(NA)
    }
  }
  )
  removeNA <- testIfPresent[!is.na(testIfPresent)]
  return(removeNA)
}
length1 <- 1:length(mappingsToData)
length1 <- as.data.frame(length1)
singleVowelWords <- apply(length1, 1, function(x){
  return(testFunction3Phonemes(mappingsToData[x]))
})
get2Words <- apply(length1, 1, function(x){
  return(sample(singleVowelWords[[x]], size = 2))
})
get2Words <- t(get2Words)
# these are the sets of words we will use for a single vowel internal rhyme
#-------------------------------------- The above code works -----------------------
length1 <- 1:length(mappingsToData)
length1 <- as.data.frame(length1)
# matches <- apply(length1, 1, function(x){
#   save <- testFunction(mappingsToData[x])
#   return(save)
# })
# getLengths <- apply(length1, 1, function(x){
#   return(length(unlist(matches[x])))
# })

library(gtools)
perms <- permutations(8,2)
lengthPerms <- 1:56
iteration <- as.data.frame(lengthPerms)
permsVowels <- apply(iteration, 1, function(x){
  return(c(vowelPhonemes[perms[x,1]], vowelPhonemes[perms[x,2]]))
})
permsVowels2 <- as.data.frame(t(permsVowels))
permsPhonemes <- apply(iteration, 1, function(x){
  return(c(mappingsToData[perms[x,1]], mappingsToData[perms[x,2]]))
})
permsPhonemes2 <- as.data.frame(t(permsPhonemes))

dfNew2 <- df[which(df$X.1=="3"),,]
iterationNew <- 1:nrow(dfNew2)
iterationNew <- as.data.frame(iterationNew)
testFunction3Phonemes <- function(pronunciation){
  testIfPresent <- apply(iterationNew, 1, function(x){
    if(grepl(pronunciation, dfNew2[x,2])){
      return(dfNew2[x,2])
    }
    else{
      return(NA)
    }
  }
  )
  removeNA <- testIfPresent[!is.na(testIfPresent)]
  return(removeNA)
}

mappingsToData2 <- rep(mappingsToData, 7)
length2 <- 1:length(mappingsToData2)
length2 <- as.data.frame(length2)
words1 <- apply(length2, 1, function(x){
  return(testFunction3Phonemes(permsPhonemes2$V1[x]))
})
words2 <- apply(length2, 1, function(x){
  return(testFunction3Phonemes(permsPhonemes2$V2[x]))
})
getWords1 <- apply(length2, 1, function(x){
  return(sample(words1[[x]], size = 1))
})
getWords2 <- apply(length2, 1, function(x){
  return(sample(words2[[x]], size = 1))
})
getWords1 <- t(getWords1)
getWords2 <- t(getWords2)
getWords1 <- as.vector(getWords1)
getWords2 <- as.vector(getWords2)
permsWords <- cbind(getWords1, getWords2)
permsWords <- as.data.frame(permsWords)
# can also just do all in one with replace in permutations?
kuSymbols <- c("p", "t", "k", "b", "d", "g", "C", "J", "s", "S", "z", "Z", "f", "T", "v", "D", "h", "n", "m", "G", "l", "w", "y", "i", "I", "E", "e", "@", "a", "W", "Y", "^", "O", "o", "U", "u")
currentSy <- c("p", "t", "k", "b", "d", "g", "J", "_", "s", "S", "z", "Z", "f", "T", "v", "D", "h", "n", "m", "N", "l", "w", "y", "i", "I", "E",  "E", "\\{","Q", "6", "2", "V","4", "o", "U", "u")
uniqKuSy <- c("C", "J", "G", "e", "@", "a", "W", "Y", "\\^", "O")
uniqCur <-  c("J", "_", "N", "E", "\\{", "Q", "6", "2", "V", "4")

iteration <- 1:length(dfNew2)
iteration <- as.data.frame(iteration)
iteration2 <- 1:length(uniqKuSy)
iteration2 <- as.data.frame(iteration2)
checkEachLetter <- function(word, letterVector, letterReplace, lengthValue){
  save <- apply(lengthValue, 1, function(x){
    if(grepl(letterVector[x], word)){
      newWord <- gsub(letterVector[x], letterReplace[x], word)
      # save2 <- apply(lengthValue, 1, function(x){
      #   if(grepl(letterVector[x], newWord)){
      #     newWord <- gsub(letterVector[x], letterReplace[x], newWord)
      #     return(newWord)
      #   }
      # })
      return(newWord)
      
    }
  })
  return(save)
}
lengthUniqKuSy <- 1:length(uniqKuSy)
lengthUniqKuSy <- as.data.frame(lengthUniqKuSy)
iteration <- 1:nrow(dfNew2)
iteration <- as.data.frame(iteration)
replaceSymbols <- apply(iteration, 1 , function(x){
  checkEachLetter(dfNew2[x,,]$X, uniqCur, uniqKuSy, lengthUniqKuSy)
})

get2Words <- as.data.frame(get2Words)
get2WordsData <- as.data.frame(get2Words)
get2WordsData <- as.data.frame(t(get2WordsData))
#write.csv(get2WordsData,"oneVowelRhymeCombos.csv", row.names = FALSE)
colnames(permsWords) <- c("V1", "V2")
oneAndTwoVowelWords <- rbind(get2Words, permsWords)
iteration <- 1:nrow(oneAndTwoVowelWords)
iteration <- as.data.frame(iteration)
replaceSymbols <- apply(iteration, 1 , function(x){
  checkEachLetter(oneAndTwoVowelWords[x,1], uniqCur, uniqKuSy, lengthUniqKuSy)
})

# -------------------


replaceSymbols2 <- unlist(replaceSymbols)
replaceSymbols3 <- unlist(replaceSymbols2)
replaceSymbols2 <- as.data.frame(cbind(replaceSymbols))
replaceSymbolsAgainIndices <- apply(iteration, 1, function(x){
  if(length(unlist(replaceSymbols2[x,1][[1]])) > 1){
    return(x)
  }
})
replaceSymbolsAgainIndices <- unlist(replaceSymbolsAgainIndices)


replaceSymbolsAgainWords <- apply(iteration, 1, function(x){
  if(length(unlist(replaceSymbols2[x,1][[1]])) > 1){
    return(unlist(replaceSymbols2[x,1][[1]])[2])
  }
})
iteration <- 1:nrow(replaceSymbols2)
iteration <- as.data.frame(iteration)
replaceSymbolsOnlyOnceWords <-  apply(iteration, 1, function(x){
  if(length(unlist(replaceSymbols2[x,1][[1]])) == 1){
    return(unlist(replaceSymbols2[x,1][[1]]))
  }
})



replaceSymbolsOnlyOnceWordsIndices <-  apply(iteration, 1, function(x){
  if(length(unlist(replaceSymbols2[x,1][[1]])) == 1){
    return(x)
  }
})

dontReplaceWordsIndices <- apply(iteration, 1, function(x){
  if(is.null(unlist(replaceSymbols2[x,1]))){
    return(x)
  }
})



dontReplaceWordsIndices <- unlist(dontReplaceWordsIndices)
replaceSymbolsOnlyOnceWordsIndices <- unlist(replaceSymbolsOnlyOnceWordsIndices)
replaceSymbolsOnlyOnceWords <- unlist(replaceSymbolsOnlyOnceWords)

oneAndTwoVowelWords2 <- oneAndTwoVowelWords
oneAndTwoVowelWords2[replaceSymbolsOnlyOnceWordsIndices,1] <- replaceSymbolsOnlyOnceWords

replaceSymbolsAgainWords <- unlist(replaceSymbolsAgainWords)
iteration <- 1:length(replaceSymbolsAgainWords)
iteration <- as.data.frame(iteration)
replaceSymbolsAgainWordsTranslation <- apply(iteration, 1 , function(x){
  checkEachLetter(replaceSymbolsAgainWords[x], uniqCur, uniqKuSy, lengthUniqKuSy)
})

replaceSymbolsAgainWordsTranslation <- unlist(replaceSymbolsAgainWordsTranslation)
# iteration <- 1:length(replaceSymbolsAgainWords)
# iteration <- as.data.frame(iteration)
# replaceSymbolsAgain <- apply(iteration, 1 , function(x){
#   checkEachLetter(replaceSymbolsAgainWords[x], uniqCur, uniqKuSy, lengthUniqKuSy)
# })
# replaceSymbolsAgain <- unlist(replaceSymbolsAgain)
oneAndTwoVowelWords2[replaceSymbolsAgainIndices,1] <- replaceSymbolsAgainWordsTranslation
listofFirstSetOfWords <- oneAndTwoVowelWords2[,1]
listofFirstSetOfWords <- toString(listofFirstSetOfWords)

iteration <- 1:length(oneAndTwoVowelWords2[,1])
iteration <- as.data.frame(iteration)
translateMiscellaneous <- apply(iteration, 1, function(x){
  if(grepl("1", oneAndTwoVowelWords2[x,1])){
    return(gsub("1", "ei", oneAndTwoVowelWords2[x,1]))
  }
  else{
    return(oneAndTwoVowelWords2[x,1])
  }
})

translateMiscellaneous2 <- apply(iteration, 1, function(x){
  if(grepl("7", translateMiscellaneous[x])){
    return(gsub("7", "i", translateMiscellaneous[x]))
  }
  else{
    return(translateMiscellaneous[x])
  }
})

finalTranslation <- toString(translateMiscellaneous2)

# stimuli

allPossibleWords <- df[7:nrow(df),,]$X
oneSetOfTwoVowelWords <- permsWords

# after the above, run again to get second set for rhyming
# after running relevant chunk, get second set below
# -------------------------------------------------------------        RUN BELOW SEPARATELY            -----------------------------------------------------------------------
secondSetOfTwoVowelWords <- permsWords
twoVowelRhymes <- cbind(oneSetOfTwoVowelWords, secondSetOfTwoVowelWords)
#write.csv(twoVowelRhymes,"twoVowelRhymeCombos.csv", row.names = FALSE)
#-----------------
# ----- above code works

iteration <- 1:100
iteration <- as.data.frame(iteration)
first100Stimuli <- apply(iteration, 1, function(x){
  return(as.data.frame(t(vector(mode = "character", length = 9))))
})

second100Stimuli <- apply(iteration, 1, function(x){
  vector1 <- vector(mode = "character", length = 9)
  vector2 <- vector(mode = "character", length = 9)
  return(as.data.frame(rbind(vector1, vector2)))
})
saveVector <- get2WordsData[floor(runif(8, min=1, max=8)),]
saveVector2 <- twoVowelRhymes[floor(runif(56, min=1, max=56)),]
names(saveVector2)[names(saveVector2) == 'getWords1'] <- 'V3'
names(saveVector2)[names(saveVector2) == 'getWords2'] <- 'V4'
saveVector2$V5 <- paste(saveVector2$V1, saveVector2$V2)
saveVector2$V6 <- paste(saveVector2$V3, saveVector2$V4)
saveVector2 <- saveVector2[,-(1:4)]
names(saveVector2)[names(saveVector2) == 'V5'] <- 'V1'
names(saveVector2)[names(saveVector2) == 'V6'] <- 'V2'
oneAndTwoVowelRhymesComboDf <- rbind(saveVector, saveVector2)
iteration2 <- 1:8
iteration2 <- as.data.frame(iteration)
first100StimuliDataOneVowelRhymesOnly <- apply(iteration2, 1, function(x){
  # get2WordsData, twoVowelRhymes
  first100Stimuli[[x]][1] <- oneAndTwoVowelRhymesComboDf[x,1]
  first100Stimuli[[x]][9] <- oneAndTwoVowelRhymesComboDf[x,2]
  return(first100Stimuli[[x]])
})
first100StimuliDataOneVowelRhymesOnlyData <- cbind(first100StimuliDataOneVowelRhymesOnly)
first100StimuliDataOneVowelRhymesOnlyData2 <- as.data.frame(first100StimuliDataOneVowelRhymesOnlyData)
iteration3 <- 1:42
iteration3 <- as.data.frame(iteration3)
saveVector3 <- twoVowelRhymes[floor(runif(56, min=1, max=56)),]
names(saveVector3)[names(saveVector3) == 'getWords1'] <- 'V3'
names(saveVector3)[names(saveVector3) == 'getWords2'] <- 'V4'
first100StimuliDataTwoVowelRhymesOnly <- apply(iteration3, 1, function(x){
  # get2WordsData, twoVowelRhymes
  first100Stimuli[[x]][1:2] <- saveVector3[x,1:2]
  first100Stimuli[[x]][8:9] <- saveVector3[x,3:4]
  return(first100Stimuli[[x]])
})
first100StimuliDataTwoVowelRhymesOnlyData <- cbind(first100StimuliDataTwoVowelRhymesOnly)
first100StimuliDataTwoVowelRhymesOnlyData2 <- as.data.frame(first100StimuliDataTwoVowelRhymesOnlyData)
names(first100StimuliDataOneVowelRhymesOnlyData2)[names(first100StimuliDataOneVowelRhymesOnlyData2) == 'first100StimuliDataOneVowelRhymesOnly'] <- 'V1'
names(first100StimuliDataTwoVowelRhymesOnlyData2)[names(first100StimuliDataTwoVowelRhymesOnlyData2) == 'first100StimuliDataTwoVowelRhymesOnly'] <- 'V1'
first50RhymingStimuli <- rbind(first100StimuliDataOneVowelRhymesOnlyData2, first100StimuliDataTwoVowelRhymesOnlyData2)
iteration <- 1:50
iteration <- as.data.frame(iteration)
first50RhymingStimuliData <- apply(iteration, 1, function(x){
  return(unlist(first50RhymingStimuli[x,]))
})
first50RhymingStimuliData <- t(first50RhymingStimuliData)
first50RhymingStimuliData <- as.data.frame(first50RhymingStimuliData)
# write.csv(first50RhymingStimuliData,"first50RhymingStimuliData.csv", row.names = FALSE)
iteration4 <- 1:14
iteration4 <- as.data.frame(iteration4)

first100StimuliDataTwoVowelRhymesOnlyEndRhyme <- apply(iteration4, 1, function(x){
  # get2WordsData, twoVowelRhymes
  if(x%%2 == 1){
    first100Stimuli[[x]][8:9] <- saveVector3[x,1:2]
  }
  else{
    first100Stimuli[[x]][8:9] <- saveVector3[x-1,3:4]
  }
  return(first100Stimuli[[x]])
})
first100StimuliDataTwoVowelRhymesOnlyEndRhymeData <- cbind(first100StimuliDataTwoVowelRhymesOnlyEndRhyme)
first100StimuliDataTwoVowelRhymesOnlyEndRhymeData <- as.data.frame(first100StimuliDataTwoVowelRhymesOnlyEndRhymeData)
iteration <- 1:14
iteration <- as.data.frame(iteration)
first100StimuliDataTwoVowelRhymesOnlyEndRhymeData <- apply(iteration, 1, function(x){
  return(unlist(first100StimuliDataTwoVowelRhymesOnlyEndRhymeData[x,]))
})
first100StimuliDataTwoVowelRhymesOnlyEndRhymeData <- t(first100StimuliDataTwoVowelRhymesOnlyEndRhymeData)
write.csv(first100StimuliDataTwoVowelRhymesOnlyEndRhymeData,"first14EndTwoVowelRhymingStimuliData.csv", row.names = FALSE)



# replaceSymbolsNext <- apply(iteration, 1, function(x){
#   if(!is.null(replaceSymbols2[x,1][[1]])){
#     return(unlist(replaceSymbols2[x,1][[1]]))
#   }
# })
# 
# replaceSymbolsNext2 <- apply(iteration, 1, function(x){
#   if(!is.null(replaceSymbolsNext[[x]])){
#     return(replaceSymbolsNext[[x]][length(replaceSymbolsNext[[x]])])
#   }
# })

# replaceSymbolsFinal <- unlist(replaceSymbolsNext2)
# replaceSymbolsNext3 <- replaceSymbolsNext2 %>% replace(.=="NULL", NA)
# 
# replaceSymbolsIndices <- apply(iteration, 1, function(x){
#   if(!is.na(replaceSymbolsNext3[x])){
#     return(x)
#   }
# })
# 
# replaceSymbolsIndices <- unlist(replaceSymbolsIndices)
# 
# iteration <- 1:length(replaceSymbolsFinal)
# iteration <- as.data.frame(iteration)
# replaceSymbolsAgain <- apply(iteration, 1 , function(x){
#   checkEachLetter(replaceSymbolsFinal[x], uniqKuSy, uniqCur, lengthUniqKuSy)
# })
# 
# replaceSymbolsAgain2 <- apply(iteration, 1, function(x){
#   if(!is.null(replaceSymbolsAgain[[x]])){
#     save <- unlist(replaceSymbolsAgain[[x]])
#     return(save[1])
#   }
# })
# 
# replaceSymbolsAgain2 <- word(replaceSymbolsAgain2)
# replace symbols again 2 are the second and final iteration of replacement. the other 29 words either did not need replacement or were already replaced and did not need a second
# replacement iteration

# ----------- ABOVE CODE WORKS

# replaceSymbols <- replaceSymbols %>% replace(.=="NULL", NA)
# replaceSymbols <- unlist(replaceSymbols)
# iteration <- 1:length(replaceSymbols)
# iteration <- as.data.frame(iteration)
# 
# replaceSymbolsIndices <- unlist(replaceSymbolsIndices)
# iteration <- 1:nrow(replaceSymbols)
# iteration <- as.data.frame(iteration)
# checkEachLetterAgain <- function(word, letterVector, letterReplace, lengthValue){
#   newWord <- word
#   save <- apply(lengthValue, 1, function(x){
#     if(grepl(letterVector[x], word)){
#       newWord <- gsub(letterVector[x], letterReplace[x], word)
#       # save2 <- apply(lengthValue, 1, function(x){
#       #   if(grepl(letterVector[x], newWord)){
#       #     newWord <- gsub(letterVector[x], letterReplace[x], newWord)
#       #     return(newWord)
#       #   }
#       # })
#       return(newWord)
#       
#     }
#   })
# }
#   
# checkEachLetter2 <- apply(iteration, 1, function(x){
#   checkEachLetterAgain(replaceSymbols[x,1], uniqCur, uniqKuSy, lengthUniqKuSy)
# })
# iteration <- 1:length(checkEachLetter2)
# iteration <- as.data.frame(iteration)
# checkEachLetter2Indices <- apply(iteration, 1, function(x){
#   if(any(!is.null(checkEachLetter2[x][[1]]))){
#     return(x)
#   }
#   else{
#     return(0)
#   }
# }
# )
# checkEachLetter2Indices <- checkEachLetter2Indices[checkEachLetter2Indices!=0]
# checkEachLetter2 <- unlist(checkEachLetter2)
# dup <- duplicated(checkEachLetter2)
# iteration <- 1:length(dup)
# iteration <- as.data.frame(iteration)
# dupIndices <- which(dup)
# frequenciesOne <- which(countFrequencies(checkEachLetter2) == 1)
# iteration <- 1:length(frequenciesOne)
# iteration <- as.data.frame(iteration)
# data_frequencies_one <- t(as.data.frame(frequenciesOne))
# getIndicesFrequenciesOne <- apply(iteration, 1, function(x){
#   if(which(checkEachLetter2 == colnames(data_frequencies_one)[x]) >= 1){
#     return(which(checkEachLetter2 == colnames(data_frequencies_one)[x]))
#   }
# })
# checkEachLetter2Indices <- unlist(checkEachLetter2Indices)
# checkEachLetter2 <- unlist(checkEachLetter2)
# checkEachLetter2 <- unique(checkEachLetter2)
# iteration <- 1:nrow(replaceSymbols)
# iteration <- as.data.frame(iteration)
# newWordsReplace <- apply(iteration,1,function(x){
#   if(any(checkEachLetter2Indices == x)){
#     return(checkEachLetter2[x])
#   }
#   else{
#     return(replaceSymbols[x,1])
#   }
# })
# uniqueCheckEachLetter2 <- checkEachLetter2[getIndicesFrequenciesOne]
# uniqueCheckEachLetter2_2 <- checkEachLetter2[which(dup)]
# combine <- c(uniqueCheckEachLetter2, uniqueCheckEachLetter2_2)
# iteration <- 1:length(combine)
# iteration <- as.data.frame(iteration)
# final <- apply(iteration, 1, function(x){
#   checkEachLetterAgain(combine[x], uniqCur, uniqKuSy, lengthUniqKuSy)
# })
# combine[2] <- "CaT"
# combine[3] <- "C^l"
# # --------- above represents words
# replaceSymbols <- cbind(replaceSymbols)
# replaceSymbols <- as.data.frame(replaceSymbols)
# iteration <- 1:ncol(replaceSymbols)
# iteration <- as.data.frame(iteration)
# replaceSymbolsUniq <- sapply(iteration, function(x){unique(replaceSymbols[,x])})
# replaceSymbolsUniq <- cbind(replaceSymbolsUniq)
# replaceSymbolsUniq <- as.data.frame(replaceSymbolsUniq)
# replaceSymbolsUniqFinal <- apply(iteration, 1, function(x){
#   unlisted <- unlist(replaceSymbolsUniq[x,1])
#   uniqueValues <- unique(unlisted)
#   return(uniqueValues)
# })
# 
