library(humdrumR)
# pacCaliLoveData <- readHumdrum("2pac_CaliforniaLove.rap")
# pacHowDoUWantItData <- readHumdrum("2pac_HowDoUWantIt.rap")
# census(pacCaliLoveData)
# reference(pacCaliLoveData)
# spines(pacCaliLoveData)
# interpretations(pacCaliLoveData)
# # error - object 'exclusive' not found
# sections(pacCaliLoveData)
# # error - could not find function 'sections'
# summary(pacCaliLoveData)
# # error - code stops at interpretations
# interpretations(pacHowDoUWantItData)
# # error
# sections(pacHowDoUWantItData)
# # error
# summary(pacHowDoUWantItData)
# # error - code stops at interpretations
# 
# centCandyShopData <- readHumdrum("50Cent_CandyShop.rap")
# census(centCandyShopData)
# reference(centCandyShopData)
# spines(centCandyShopData)
# interpretations(centCandyShopData)
# sections(centCandyShopData)
# summary(centCandyShopData)
# 
# # seems that the functions interpretations and sections are not working for the rap datasets.
# 
# rapData <- readHumdrum('.*rap')
# 
# census(rapData)
# reference(rapData)
# spines(rapData)
# interpretations(rapData)
# sections(rapData)
# summary(rapData)
# 
# filterhumdrum(rapData)

# rapData[2]

library(humdrumR)

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

rhymeSchemes3 <- unlist(rhymeSchemes[[1000]])

occurrences <- table(rhymeSchemes3)

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
    maxInternalRhyme <- max(table$internalRhymesList[,vector])
    maxInternalRhymeIndex <- which(table$internalRhymesList == maxInternalRhyme)
    difference <- nonRhymingWordFrequency - maxInternalRhyme
  }
  else{
    return(0)
  }
  return(difference)
}
# test function
df2 <- as.data.frame(frequencyTables[[968]])
determine <- which(df2$Freq > 1)
internalRhymes <- df2$frequencyCount[determine]
# implement functions
rhymeSchemes <- mcf$Token %hum<% c(~list(Rhyme), by ~ File ~ Phrase)
iteration <- 1:length(rhymeSchemes)
iteration <- cbind(iteration)
df <- rbind(rhymeSchemes)
df <- as.data.frame(t(df))
frequencyTables <- apply(iteration, 1, function(x){countFrequencies(df[x,1])})
frequencyTablesDataFrame <- cbind(frequencyTables)
# frequencyTablesDataFrame <- as.data.frame(frequencyTablesDataFrame)
internalRhymesList <- apply(iteration, 1, function(x){recordMoreThanOneInstance(frequencyTablesDataFrame[x][1])})
internalRhymesListDataFrame <- rbind(internalRhymesList)
internalRhymesListDataFrame <- as.data.frame(internalRhymesListDataFrame)
save <- apply(iteration, 1, function(x){as.data.frame(internalRhymesListDataFrame[1,x])})
# internalRhymesListDataFrame <- t(internalRhymesListDataFrame)
findMaxDifference <- apply(iteration, 1, function(x){recordFreqDifference(internalRhymesListDataFrame[1,x])})
maxDifference <- max(findMaxDifference)
# findMaxDifference <- apply(iteration, 1, function(x){recordFreqDifference(internalRhymesListDataFrame[1,x]$internalRhymesList)})
tableWithMaxDifference <- which(findMaxDifference == maxDifference)
findTableWithMaxDifference <- frequencyTables[tableWithMaxDifference]

# test summary functions again
# library(humdrumR)
# mcf <- readHumdrum('.*rap')
# census(mcf)
# reference(mcf)
# spines(mcf)
# interpretations(mcf)
# sections(mcf)
# summary(mcf)

# largest distance between two internal rhymes


save <- rowRhymeSchemes[5,]$rhymeSchemes
indicesOfLetters <- which(save != ".")
letters <- save[indicesOfLetters]

# gsub("\\s*\\([^\\)]","s",as.character(rowRhymeSchemes[5,]$rhymeSchemes))
# gsub("\\s*\\)","t",as.character(rowRhymeSchemes[5,]$rhymeSchemes))
save2 <- replaceWithRepeating(rowRhymeSchemes[5,]$rhymeSchemes)
indices <- grep("bt", save2)

# function
rowRhymeSchemes <- cbind(rhymeSchemes)
replaceWithRepeating <- function(string){
  save <- gsub("\\s*\\([^\\)]","s",as.character(string))
  save <- gsub("\\s*\\)","t",as.character(save))
  # this allows R to read these as repeating rhymes
  return(save)
}
getIndices <- function(pattern1, wholeString){
  return(grep(pattern1, wholeString))
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
getIndicesPrint <- apply(iterationForRhymes, 1, function(x){getIndices(rowRhymeSchemes[x,]$rhymeSchemes)})
printDistance <- apply(iteration, 1, function(x){grep(save[x], rowRhymeSchemes[5,]$rhymeSchemes)})

