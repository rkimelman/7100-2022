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
