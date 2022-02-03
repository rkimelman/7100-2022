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

mcf$Token %hum<% c(~list(Rhyme), by ~ File ~ Phrase)

