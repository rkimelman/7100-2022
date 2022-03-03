pseudoWordData <- read.csv(file = "pseudoworddata.csv")
vowelPhonemes <- c("aa", "ae", "ah", "eh", "ey", "ih", "iy", "uw")
mappingsToData <- c("Q" , "\\{", "V", "E", "1", "I", "7", "u")
phonemePermutations <- function(numberOfVowelPhonemes, vowelPhonemes, mappingsToCurrentSystem, numberOfWordPhonemes, df)
  # experiment
  combo <- t(combn(vowelPhonemes, numberofVowelPhonemes))
  combo <- as.data.frame(combo)
  
  # obtain two random words with "aa" vowel sound
  

  dfNew <- df[which(df$X.1==numberOfVowelPhonemes),,]
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
  length1 <- 1:length(mappingsToCurrentSystem)
  length1 <- as.data.frame(length1)
  singleVowelWords <- apply(length1, 1, function(x){
    return(testFunction3Phonemes(mappingsToCurrentSystem[x]))
  })
  getWords <- apply(length1, 1, function(x){
    return(sample(singleVowelWords[[x]], size = numberOfVowelPhonemes))
  })
  getWords <- t(getWords)
  # these are the sets of words we will use for a single vowel internal rhyme
  #-------------------------------------- The above code works -----------------------
  length1 <- 1:length(mappingsToCurrentSystem)
  length1 <- as.data.frame(length1)
  
  library(gtools)
  perms <- permutations(numberOfVowelPhonemes,numberOfWordPhonemes)
  lengthPerms <- 1:length(perms)
  iteration <- as.data.frame(lengthPerms)
  permsVowels <- apply(iteration, 1, function(x){
    return(c(vowelPhonemes[perms[x,1:numberOfWordPhonemes]]))
  })
  permsVowels2 <- as.data.frame(t(permsVowels))
  permsPhonemes <- apply(iteration, 1, function(x){
    return(c(mappingsToCurrentSystem[perms[x,1:numberOfWordPhonemes]]))
  })
  permsPhonemes2 <- as.data.frame(t(permsPhonemes))
  
  dfNew2 <- df[which(df$X.1==numberOfVowelPhonemes),,]
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
  
  mappingsToCurrentSystem2 <- rep(mappingsToCurrentSystem, length(vowelPhonemes)-1)
  length2 <- 1:length(mappingsToCurrentSystem2)
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
  
  getWords <- as.data.frame(getWords)
  colnames(permsWords) <- c("V1", "V2")
  oneAndTwoVowelWords <- rbind(getWords, permsWords)
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