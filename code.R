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

# 1. look for most common number of internal rhymes in each line
# 2. look to see if most occur in the first or second half of the line for example - is there a difference in perception there? Maybe the longer an artist goes without rhyming,
#    the more likely you are to catch the internal rhyme, or there is a greater reaction (brain or implicit reaction). Theory is that if internal rhyme occurs earlier in the line
#    the listener is not ready for it and it cannot be processed fully because the person has not completely focused in on the audio. Each line sort of starts a new processing
#    scheme/event?
# 3. is it easier to perceive an internal rhyme with the rhymes sort of far apart if the song were to be rehearsed such that the line was broken up into two lines and the rhymes
#    occurred at the end of each line?
#    keep diving into it more. naming these things.
#   initial experiment - internal rhyme. When do you notice an internal rhyme. bring in statistical thing. 1. Computational  - doing analyses (improving or
#   expanding MCFlow. Start writing abstract. Collab on overleaf. Two challenges - what are the stimuli? Other - what is the task? Both EEG and behavioral.
#   first 2 hypotheses - likelihood of rhyme and perception of it. can manipulate where we put it. 
# 4. look at likelihood of internal rhymes in a similar way as nat's theory with phonemes -- is an internal rhyme more likely to be perceived as a rhyme if it is an internal rhyme
#    structure that is not seen very often in rap music?
# 5. Is a rhyme perceived if the artist puts intonation on the end word of a line and the first word on the next line technically rhymes with that word but they 
#    say it in a "normal" talking voice. What if they put intonation on the first word, it rhymes with the last word but there's no intonation on it.
# 6. Compare rhymes in a normal talking voice and rapping voice.
# 7. What is largest difference between # of non-rhyming words and the max # of internal rhymes in any given line? How does the length of syllables of this line compare to previous?
#    How often can person detect the rhyme there? Once you go past the amount of syllables as the previous line, we don't know when the rhyme is going to come. Maybe the person's
#    level of attention decreases as the line moves forward and then an internal rhyme doesn't catch their attention - or maybe the opposite. In function below.
#    Can also look at how the distance between two internal rhyming words interacts with the number of non-rhyming words occurring before the first instance of an internal rhyming word
#    since there has been so much time passing without a rhyme. Ties back to original theory where which is more likely? Becomes increasingly unlikely to hear a rhyme. Compare
#    to when there's a rhyme at the end of one line and also the beginning of next one.
# 8. Does perception of rhyme differ when you flip the order of a double internal rhyme? I.e. is ab ab perceived the same or in a similar way as ba ab. I.e. inverse. Same for jJj and JjJ?

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

iteration <- 1:length(indicesOfLetters)
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

# # find all words with "Q" in their pronunciations
# iteration <- 1:nrow(df)
# iteration <- as.data.frame(iteration)
# # find lengths of all pronunciations
# lengths <- apply(iteration, 1, function(x){
#   return(nchar(df[x, 2]))
# })
# # test if Q is inside pronunciation, if not fill with na to be removed later
# testIfQ <- apply(iteration, 1, function(x){
#   if(grepl("Q", df[x,2])){
#     return(df[x,2])
#   }
#   else{
#     return(NA)
#   }
# }
# )
# removeNA_Q <- testIfQ[!is.na(testIfQ)]
# QWords <- sample(removeNA_Q, size = 2)
# QWordsPronunciations <- c("sahnz", "kahskt")
# saveQ <- apply(iteration, 1, function(x))
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
matches <- apply(length1, 1, function(x){
  save <- testFunction(mappingsToData[x])
  return(save)
})
getLengths <- apply(length1, 1, function(x){
  return(length(unlist(matches[x])))
})
# getLengths <- cbind(getLengths)
# getLengths <- as.data.frame(getLengths)
# getOneLength <- apply(getLengths, 1, function(y){
#   getOneLengthEmbedded <- apply(length1, 1, function(x){
#     if(!is.na(nchar(matches[[x]][y[x]]))){
#       if(nchar(matches[[x]][y]) == 3){
#         return(matches[[x]][y])
#       }
#       else{
#         return(NA)
#       }
#     }
#   })
#     
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


# iterationNew <- 1:nrow(dfNew2)
# iterationNew <- as.data.frame(iterationNew)
# testFunction4Phonemes <- function(pronunciation){
#   testIfPresent <- apply(iterationNew, 1, function(x){
#     if(grepl(pronunciation[1], dfNew2[x,2])){
#       if(grepl(pronunciation[2], dfNew2[x,2])){
#         return(dfNew2[x,2])
#       }
#     }
#     else{
#       return(NA)
#     }
#   }
#   )
#   removeNA <- testIfPresent[!is.na(testIfPresent)]
#   return(removeNA)
# }


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
checkEachLetter <- function(word, letterVector, lengthValue){
  save <- apply(lengthValue, 1, function(x){
    print(letterVector[x])
    print(word)
    return(grepl(letterVector[x], word))
  })
}
lengthUniqKuSy <- 1:length(uniqKuSy)
lengthUniqKuSy <- as.data.frame(lengthUniqKuSy)
value <- checkEachLetter("hello", uniqKuSy, lengthUniqKuSy)
replaceSymbols <- apply(iteration, 1 , function(x){
  checkEachLetter(dfNew2[x,,]$X, )
})