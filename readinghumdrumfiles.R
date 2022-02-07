library(humdrumR)
pacCaliLoveData <- readHumdrum("2pac_CaliforniaLove.rap")
pacHowDoUWantItData <- readHumdrum("2pac_HowDoUWantIt.rap")
census(pacCaliLoveData)
reference(pacCaliLoveData)
spines(pacCaliLoveData)
interpretations(pacCaliLoveData)
# error - object 'exclusive' not found
sections(pacCaliLoveData)
# error - could not find function 'sections'
summary(pacCaliLoveData)
# error - code stops at interpretations
interpretations(pacHowDoUWantItData)
# error
sections(pacHowDoUWantItData)
# error
summary(pacHowDoUWantItData)
# error - code stops at interpretations

centCandyShopData <- readHumdrum("50Cent_CandyShop.rap")
census(centCandyShopData)
reference(centCandyShopData)
spines(centCandyShopData)
interpretations(centCandyShopData)
sections(centCandyShopData)
summary(centCandyShopData)

# seems that the functions interpretations and sections are not working for the rap datasets.

rapData <- readHumdrum('.*rap')

census(rapData)
reference(rapData)
spines(rapData)
interpretations(rapData)
sections(rapData)
summary(rapData)

filterhumdrum(rapData)

rapData[2]

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
# 2. look to see if most occur in the first or second half of the line for example - is there a difference in perception there?
# 3. is it easier to perceive an internal rhyme with the rhymes sort of far apart if the song were to be rehearsed such that the line was broken up into two lines and the rhymes
#    occurred at the end of each line?
# 4. look at likelihood of internal rhymes in a similar way as nat's theory with phonemes -- is an internal rhyme more likely to be perceived as a rhyme if it is an internal rhyme
#    structure that is not seen very often in rap music?
recordFreqDifference <- function(table){
  # record frequency difference between non rhyming words and the most frequent internal rhyme
  # df <- as.data.frame(table)
  nonRhymingWordFrequencyRowIndex <- which(table$frequencyTables.frequencyCount == ".")
  nonRhymingWordFrequency <- table[nonRhymingWordFrequencyRowIndex,]$frequencyTables.Freq
  vector <- which(table$frequencyTables.frequencyCount != ".")
  maxInternalRhymeIndex <- max(vector)
  maxInternalRhymeVector <- table[maxInternalRhymeIndex,]$frequencyTables.Freq
  maxInternalRhyme <- max(maxInternalRhymeVector)
  difference <- nonRhymingWordFrequency - maxInternalRhyme
  return(difference)
}
# test function
df2 <- as.data.frame(frequencyTables[[968]])
determine <- which(df2$Freq > 1)
internalRhymes <- df2$frequencyCount[determine]
# implement functions
rhymeSchemes <- mcf$Token %hum<% c(~list(Rhyme), by ~ File ~ Phrase)
iteration <- 1:length(rhymeSchemes)
iteration <- as.data.frame(iteration)
df <- rbind(rhymeSchemes)
df <- as.data.frame(t(df))
frequencyTables <- apply(iteration, 1, function(x){countFrequencies(df[x,1])})
frequencyTablesDataFrame <- cbind(frequencyTables)
# frequencyTablesDataFrame <- as.data.frame(frequencyTablesDataFrame)
internalRhymesList <- apply(iteration, 1, function(x){recordMoreThanOneInstance(frequencyTablesDataFrame[x][1])})
internalRhymesListDataFrame <- rbind(internalRhymesList)
internalRhymesListDataFrame <- as.data.frame(internalRhymesListDataFrame)
# internalRhymesListDataFrame <- t(internalRhymesListDataFrame)
findMaxDifference <- apply(iteration, 1, function(x){recordFreqDifference(internalRhymesListDataFrame[1,x]$internalRhymesList)})
findMaxDifference <- findMaxDifference[!is.na(findMaxDifference)]
maxDifference <- max(unlist(findMaxDifference))
findMaxDifference <- apply(iteration, 1, function(x){recordFreqDifference(internalRhymesListDataFrame[1,x]$internalRhymesList)})
tableWithMaxDifference <- which(findMaxDifference == maxDifference)
findTableWithMaxDifference <- frequencyTables[tableWithMaxDifference]
