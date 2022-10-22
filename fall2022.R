#most of the libraries needed
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations

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
NewJackHustler <- readHumdrum('Ice-T_NewJackHustler.rap')

NewJackHustlerDF <- as.data.frame(NewJackHustler)


threegramed <- within(NewJackHustler, paste(Token, Token[lag = -1:-2]), subset = Spine == 1, complement = Token)

df <- as.data.frame(threegramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,1])
  rhythm_list <- append(rhythm_list, df[i,1])
}

capture.output(rhythm_list, file = "rhythm_NewJackHustler_threegramed.csv")

fivegramed <- within(californiaLove, paste(Token, Token[lag = -1:-4]), subset = Spine == 2, complement = Token)

df <- as.data.frame(fourgramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_fivegramed.csv")

sixgramed <- within(californiaLove, paste(Token, Token[lag = -1:-5]), subset = Spine == 2, complement = Token)

df <- as.data.frame(sixgramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_sixgramed.csv")

sevengramed <- within(SoManyTears, paste(Token, Token[lag = -1:-15]), subset = Spine == 1, complement = Token)

df <- as.data.frame(sevengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,1])
  rhythm_list <- append(rhythm_list, df[i,1])
}

capture.output(rhythm_list, file = "rhythm_SoManyTears_sixteengramed.csv")

eightgramed <- within(californiaLove, paste(Token, Token[lag = -1:-7]), subset = Spine == 2, complement = Token)

df <- as.data.frame(eightgramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_eightgramed.csv")

ninegramed <- within(californiaLove, paste(Token, Token[lag = -1:-8]), subset = Spine == 2, complement = Token)

df <- as.data.frame(ninegramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_ninegramed.csv")

tengramed <- within(californiaLove, paste(Token, Token[lag = -1:-9]), subset = Spine == 2, complement = Token)

df <- as.data.frame(tengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_tengramed.csv")

elevengramed <- within(howUWantIt, paste(Token, Token[lag = -1:-8]), subset = Spine == 2, complement = Token)

df <- as.data.frame(elevengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_howUWantIt_ninegramed.csv")

twelvegramed <- within(CandyShop, paste(Token, Token[lag = -1:-15]), subset = Spine == 1, complement = Token)

df <- as.data.frame(twelvegramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,1])
  rhythm_list <- append(rhythm_list, df[i,1])
}

capture.output(rhythm_list, file = "rhythm_CandyShop_sixteengramed.csv")

thirteengramed <- within(californiaLove, paste(Token, Token[lag = -1:-12]), subset = Spine == 2, complement = Token)

df <- as.data.frame(thirteengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_thirteengramed.csv")



fifteengramed <- within(californiaLove, paste(Token, Token[lag = -1:-14]), subset = Spine == 1, complement = Token)

df <- as.data.frame(fifteengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,1])
  rhythm_list <- append(rhythm_list, df[i,1])
}

capture.output(rhythm_list, file = "rhythm_caliLove_fifteengramed.csv")

sixteengramed <- within(californiaLove, paste(Token, Token[lag = -1:-15]), subset = Spine == 1, complement = Token)

df <- as.data.frame(sixteengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,1])
  rhythm_list <- append(rhythm_list, df[i,1])
}

capture.output(rhythm_list, file = "rhythm_caliLove_sixteengramed.csv")

seventeengramed <- within(californiaLove, paste(Token, Token[lag = -1:-16]), subset = Spine == 1, complement = Token)

df <- as.data.frame(seventeengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,1])
  rhythm_list <- append(rhythm_list, df[i,1])
}

capture.output(rhythm_list, file = "rhythm_caliLove_seventeengramed.csv")


# californiaLoveIPA <- list(californiaLoveDF[2:417,6])
# 
# californiaLoveStress <- list(californiaLoveDF[2:417,2])
# 
# californiaLoveRhyme <- list(californiaLoveDF[2:417, 5])

SoManyTears <- readHumdrum('2pac_SoManyTears.rap')

SoManyTearsDF <- as.data.frame(SoManyTears)


threegramed <- within(SoManyTears, paste(Token, Token[lag = -1:-11]), subset = Spine == 2, complement = Token)

df <- as.data.frame(threegramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_SoManyTears_twelvegramed.csv")

fivegramed <- within(californiaLove, paste(Token, Token[lag = -1:-4]), subset = Spine == 2, complement = Token)

df <- as.data.frame(fourgramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_fivegramed.csv")

sixgramed <- within(californiaLove, paste(Token, Token[lag = -1:-5]), subset = Spine == 2, complement = Token)

df <- as.data.frame(sixgramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_sixgramed.csv")

sevengramed <- within(californiaLove, paste(Token, Token[lag = -1:-6]), subset = Spine == 2, complement = Token)

df <- as.data.frame(sevengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_sevengramed.csv")

eightgramed <- within(californiaLove, paste(Token, Token[lag = -1:-7]), subset = Spine == 2, complement = Token)

df <- as.data.frame(eightgramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_eightgramed.csv")

ninegramed <- within(californiaLove, paste(Token, Token[lag = -1:-8]), subset = Spine == 2, complement = Token)

df <- as.data.frame(ninegramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_ninegramed.csv")

tengramed <- within(californiaLove, paste(Token, Token[lag = -1:-9]), subset = Spine == 2, complement = Token)

df <- as.data.frame(tengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_caliLove_tengramed.csv")

elevengramed <- within(howUWantIt, paste(Token, Token[lag = -1:-15]), subset = Spine == 1, complement = Token)

df <- as.data.frame(elevengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,1])
  rhythm_list <- append(rhythm_list, df[i,1])
}

capture.output(rhythm_list, file = "rhythm_howUWantIt_sixteengramed.csv")

twelvegramed <- within(PapazSong, paste(Token, Token[lag = -1:-15]), subset = Spine == 1, complement = Token)

df <- as.data.frame(twelvegramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,1])
  rhythm_list <- append(rhythm_list, df[i,1])
}

capture.output(rhythm_list, file = "rhythm_PapazSong_sixteengramed.csv")

thirteengramed <- within(CandyShop, paste(Token, Token[lag = -1:-11]), subset = Spine == 2, complement = Token)

df <- as.data.frame(thirteengramed)

rhythm_list <- list()

for (i in 1:nrow(df)){
  print(df[i,2])
  rhythm_list <- append(rhythm_list, df[i,2])
}

capture.output(rhythm_list, file = "stress_CandyShop_twelvegramed.csv")


ipa_list <- list()

for (i in 1:nrow(ColorsDF)){
  ipa_list <- append(ipa_list, ColorsDF[i,6])
}

rhyme_list <- list()

for (i in 1:nrow(ColorsDF)){
  rhyme_list <- append(rhyme_list, ColorsDF[i,5])
}

stress_list <- list()

for (i in 1:nrow(ColorsDF)){
  stress_list <- append(stress_list, ColorsDF[i,2])
}

capture.output(ipa_list, file = "IPA_Colors.csv")

capture.output(rhyme_list, file = "rhyme_Colors.csv")

capture.output(stress_list, file = "stress_Colors.csv")
count <- 0
phrase_list <- list()
for (i in 1:nrow(ColorsDF)){
  print(ColorsDF[i,7])
  if(ColorsDF[i,7] == "."){
    count <- count + 1
  }
  phrase_list <- append(phrase_list, count)
}

rhythm_list <- list()

for (i in 1:nrow(howUWantItDF)){
  print(howUWantItDF[i,1])
  rhythm_list <- append(rhythm_list, howUWantItDF[i,1])
}

capture.output(rhythm_list, file = "rhythm_howUWantIt.csv")

capture.output(phrase_list, file = "phrase_Colors.csv")


spinePipe(californiaLove, 2:8) -> californiaLove[rev(c('Stress', 'Tone', 'Break', 'Rhyme', 'IPA', 'Lyrics', 'Hype'))]
californiaLove$Token |> c(~segments(Break %in% c('3', '4','5')), by ~ File) -> californiaLove$Phrase
californiaLove$Token %hum<% c(~list(paste(Lyrics, collapse = ' ')), by ~ File ~ Phrase)
rhymeSchemes <- californiaLove$Token %hum<% c(~list(Rhyme), by ~ File ~ Phrase)
lyrics <- californiaLove$Token %hum<% c(~list(Lyrics), by ~ File ~ Phrase)
stress <- californiaLove$Token %hum<% c(~list(Stress), by ~ File ~ Phrase)
IPA <- californiaLove$Token %hum<% c(~list(IPA), by ~ File ~ Phrase)
Tone <- californiaLove$Token %hum<% c(~list(Tone), by ~ File ~ Phrase)
Phrase <- californiaLove$Token |> c(~list(Phrase), by ~ File ~ Phrase)
Break <- californiaLove$Token %hum<% c(~list(Break), by ~ File ~ Phrase)
Hype <- californiaLove$Token %hum<% c(~list(Hype), by ~ File ~ Phrase)

# spinePipe(mcf, 2:8, 1) -> mcf[rev(c('Stress', 'Tone', 'Break', 'Rhyme', 'IPA', 'Lyrics', 'Hype'))]
# mcf$Token %hum>% c(~segments(Break %in% c('3', '4','5')), by ~ File) -> mcf$Phrase
# mcf$Token %hum<% c(~list(paste(Lyrics, collapse = ' ')), by ~ File ~ Phrase)
# rhymeSchemes <- mcf$Token %hum<% c(~list(Rhyme), by ~ File ~ Phrase)
# lyrics <- mcf$Token %hum<% c(~list(Lyrics), by ~ File ~ Phrase)
# stress <- mcf$Token %hum<% c(~list(Stress), by ~ File ~ Phrase)
# IPA <- mcf$Token %hum<% c(~list(IPA), by ~ File ~ Phrase)
# Tone <- mcf$Token %hum<% c(~list(Tone), by ~ File ~ Phrase)
# Phrase <- mcf$Token %hum<% c(~list(Phrase), by ~ File ~ Phrase)
# Break <- mcf$Token %hum<% c(~list(Break), by ~ File ~ Phrase)
# Hype <- mcf$Token %hum<% c(~list(Hype), by ~ File ~ Phrase)

foldRecords(mcf, c('rhyme', 'stress', 'ipa'), 'recip') -> mcf
within(mcf$Token, 
       Rhyme <- str_extract(Rhyme, '[A-Za-z]+'),
       soi <- SOI(duration(Token))$Onset, 
       Rest <- Ipa == 'R', by = File) -> mcf
subset(mcf, Stress == '1' & !Rest) -> mcf
circleplot <- function(soi, rhymes, Bar, OTL) {
  bars <- floor(soi)
  newbar <- bars == soi
  soi <- soi
  soi <- soi / ceiling(max(soi))
  rad <- (soi * 2 * pi) + (pi / 2)
  x <- cos(rad)
  y <- sin(rad)
  plot(x, y, type = 'p', pch = 16, cex = .6, axes = FALSE, xlab = '', ylab = '')
  # text(x, y, labels = seq_along(x))
  text(x[newbar] * 1.1, y[newbar] * 1.1, paste0('=',bars[newbar]), xpd=T)
  mtext(paste0("Bars ", min(Bar), ' through ', max(Bar), ' of\n', OTL), side = 3)  
  for (r in unique(rhymes[!is.na(rhymes)])) {
    hits <- which(rhymes == r)
    XY <- subset(expand.grid(I = hits, J = hits), J > I)
    with(XY, arrows(x[I], y[I], x[J], y[J], angle = 20, length = .1))
  }
}
pdf('rhymeplots.pdf')
for (i in 1:length(mcf)) {
  print(i)
  if (i %in% c(10, 26, 29)) next
  with(subset(mcf[i], Bar <= 8), circleplot(soi, Rhyme,Bar,OTL))
  with(subset(mcf[i], Bar <= 16), circleplot(soi, Rhyme,Bar,OTL))
}
dev.off()

count <- 0

for (i in 1:length(stress)){
  for (j in 1:length(stress[i][[1]]))
    count <- count + 1
}

print(count)

df <- do.call(rbind.data.frame, stress)

capture.output(stress, file = "stress_caliLove.csv")

# sigmoid <- function(x){
#   value <- 1/(1+exp(-x))
#   return(value)
# }

# rap <- rep(1, 124)
# 
# library(dbnR)
# library(data.table)
# data(motor)
# size <- 3
# one <- c("A","A", "A")
# two <- c("D","D", "E")
# three <- c("E", "F", "F")
# four <- c("G", "G", "G")
# 
# data1<-data.frame(one)
# data2<-data.frame(two)
# data3 <- data.frame(three)
# data4 <- data.frame(four)
# 
# 
# 
# blacklist <- c("data1_t_0", "data2_0")
# blacklist <- rbind(blacklist, c("data1_t_2", "data2_t_1"))
# blacklist_tr <- c("data3_t_1", "data4_t_0")
# blacklist_tr <- rbind(blacklist_tr, c("data3_t_2", "data2_t_1"))
# net <- dbnR::learn_dbn_struc(data1, size, method = "dmmhc", blacklist = blacklist,
#                              blacklist_tr = blacklist_tr, restrict = "mmpc", maximize = "hc",
#                              restrict.args = list(test = "cor"),
#                              maximize.args = list(score = "bic-g", maxp = 10))
# 
# f_dt_train <- fold_dt(dt_train, size)
# f_dt_val <- fold_dt(dt_val, size)
# fit <- dbnR::fit_dbn_params(net, f_dt_train, method = "mle")
# 
# dbnR::plot_dynamic_network(fit)
# 
# library(visNetwork)
# 
# dbnR::plot_dynamic_network(fit)
