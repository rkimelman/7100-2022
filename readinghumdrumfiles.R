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
