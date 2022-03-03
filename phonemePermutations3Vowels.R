# experiment
vowelPhonemes <- c("aa", "ae", "ah", "eh", "ey", "ih", "iy", "uw")
mappingsToData <- c("Q" , "\\{", "V", "E", "1", "I", "7", "u")
combo2 <- t(combn(vowelPhonemes, 2))
combo2 <- as.data.frame(combo2)
combo3 <- t(combn(vowelPhonemes, 3))
combo3 <- as.data.frame(combo3)

# obtain two random words with "aa" vowel sound

df <- read.csv(file = "pseudoworddata2.csv")
dfNew <- df[which(df$X.1=="4"),,]
# use random configurations to generate words, but always make each word have at least 1 phonological neighbor
# should each rhyming word have same number of syllables, and vowels occur in same positions? can code for that
# generalize above

testFunction3Phonemes <- function(pronunciation, iteration){
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
iteration <- 1:64
iteration <- as.data.frame(iteration)
singleVowelWords <- apply(length1, 1, function(x){
  return(testFunction3Phonemes(mappingsToData[x], iteration))
})
iteration <- 1:8
iteration <- as.data.frame(iteration)
get3Words <- apply(iteration, 1, function(x){
  return(sample(singleVowelWords[[x]], size = 3, replace = TRUE))
})
get3Words <- t(get3Words)
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
perms <- permutations(8,3)
lengthPerms <- 1:nrow(perms)
iteration <- as.data.frame(lengthPerms)
permsVowels <- apply(iteration, 1, function(x){
  return(c(vowelPhonemes[perms[x,1]], vowelPhonemes[perms[x,2]], vowelPhonemes[perms[x,3]]))
})
permsVowels2 <- as.data.frame(t(permsVowels))
permsPhonemes <- apply(iteration, 1, function(x){
  return(c(mappingsToData[perms[x,1]], mappingsToData[perms[x,2]], mappingsToData[perms[x,3]]))
})
permsPhonemes2 <- as.data.frame(t(permsPhonemes))

dfNew2 <- df[which(df$X.1=="4"),,]
iterationNew <- 1:nrow(dfNew2)
iterationNew <- as.data.frame(iterationNew)
testFunction4Phonemes <- function(pronunciation){
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
words1 <- apply(iteration, 1, function(x){
  return(testFunction3Phonemes(permsPhonemes2$V1[x]))
})
words2 <- apply(iteration, 1, function(x){
  return(testFunction4Phonemes(permsPhonemes2$V2[x]))
})
words3 <- apply(iteration, 1, function(x){
  return(testFunction4Phonemes(permsPhonemes2$V3[x]))
})
getWords1 <- apply(iteration, 1, function(x){
  return(sample(words1[[x]], size = 1))
})
getWords2 <- apply(iteration, 1, function(x){
  return(sample(words2[[x]], size = 1))
})
getWords3 <- apply(iteration, 1, function(x){
  return(sample(words3[[x]], size = 1))
})
getWords1 <- t(getWords1)
getWords2 <- t(getWords2)
getWords3 <- t(getWords3)
getWords1 <- as.vector(getWords1)
getWords2 <- as.vector(getWords2)
getWords3 <- as.vector(getWords3)
permsWords <- cbind(getWords1, getWords2, getWords3)
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

get3Words <- as.data.frame(get3Words)
colnames(permsWords) <- c("V1", "V2", "V3")
threeVowelCombos <- rbind(get3Words, permsWords)
iteration <- 1:nrow(threeVowelCombos)
iteration <- as.data.frame(iteration)
replaceSymbols <- apply(iteration, 1 , function(x){
  checkEachLetter(threeVowelCombos[x,1], uniqCur, uniqKuSy, lengthUniqKuSy)
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
  if(length(unlist(replaceSymbols2[x,1][[1]])) == 2){
    return(unlist(replaceSymbols2[x,1][[1]])[2])
  }
})
replaceSymbols3rdTimeIndices <- apply(iteration, 1, function(x){
  if(length(unlist(replaceSymbols2[x,1][[1]])) == 3){
    return(x)
  }
})
replaceSymbols3rdTimeWords <- apply(iteration, 1, function(x){
  if(length(unlist(replaceSymbols2[x,1][[1]])) == 3){
    return(unlist(replaceSymbols2[x,1][[1]])[3])
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
replaceSymbols3rdTimeIndices <- unlist(replaceSymbols3rdTimeIndices)
replaceSymbols3rdTimeWords <- unlist(replaceSymbols3rdTimeWords)

threeVowelCombos2 <- threeVowelCombos
threeVowelCombos2[replaceSymbolsOnlyOnceWordsIndices,1] <- replaceSymbolsOnlyOnceWords

replaceSymbolsAgainWords <- unlist(replaceSymbolsAgainWords)
iteration <- 1:length(replaceSymbolsAgainWords)
iteration <- as.data.frame(iteration)
replaceSymbolsAgainWordsTranslation <- apply(iteration, 1 , function(x){
  checkEachLetter(replaceSymbolsAgainWords[x], uniqCur, uniqKuSy, lengthUniqKuSy)
})

replaceSymbolsAgainWordsTranslation <- unlist(replaceSymbolsAgainWordsTranslation)

iteration <- 1:length(replaceSymbols3rdTimeWords)
iteration <- as.data.frame(iteration)
replaceSymbols3rdTimeWordsTranslation <- checkEachLetter(replaceSymbols3rdTimeWords, uniqCur, uniqKuSy, lengthUniqKuSy)

replaceSymbols3rdTimeWordsTranslation <- unlist(replaceSymbols3rdTimeWordsTranslation)
replaceSymbols3rdTimeWordsTranslation <- replaceSymbols3rdTimeWordsTranslation[1]

# iteration <- 1:length(replaceSymbolsAgainWords)
# iteration <- as.data.frame(iteration)
# replaceSymbolsAgain <- apply(iteration, 1 , function(x){
#   checkEachLetter(replaceSymbolsAgainWords[x], uniqCur, uniqKuSy, lengthUniqKuSy)
# })
# replaceSymbolsAgain <- unlist(replaceSymbolsAgain)
threeVowelCombos2[replaceSymbolsAgainIndices,1] <- replaceSymbolsAgainWordsTranslation
threeVowelCombos2[replaceSymbols3rdTimeIndices,1] <- replaceSymbols3rdTimeWordsTranslation
listofFirstSetOfWords <- threeVowelCombos2[,1]
listofFirstSetOfWords <- toString(listofFirstSetOfWords)

iteration <- 1:length(threeVowelCombos2[,1])
iteration <- as.data.frame(iteration)
translateMiscellaneous <- apply(iteration, 1, function(x){
  if(grepl("1", threeVowelCombos2[x,1])){
    return(gsub("1", "ei", threeVowelCombos2[x,1]))
  }
  else{
    return(threeVowelCombos2[x,1])
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
