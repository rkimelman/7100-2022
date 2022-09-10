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
californiaLove <- readHumdrum('2pac_CaliforniaLove.rap')
spinePipe(californiaLove, 2:8, 1) -> californiaLove[rev(c('Stress', 'Tone', 'Break', 'Rhyme', 'IPA', 'Lyrics', 'Hype'))]
californiaLove$Token %hum>% c(~segments(Break %in% c('3', '4','5')), by ~ File) -> californiaLove$Phrase
californiaLove$Token %hum<% c(~list(paste(Lyrics, collapse = ' ')), by ~ File ~ Phrase)
rhymeSchemes <- californiaLove$Token %hum<% c(~list(Rhyme), by ~ File ~ Phrase)
lyrics <- californiaLove$Token %hum<% c(~list(Lyrics), by ~ File ~ Phrase)
stress <- californiaLove$Token %hum<% c(~list(Stress), by ~ File ~ Phrase)
IPA <- californiaLove$Token %hum<% c(~list(IPA), by ~ File ~ Phrase)
Tone <- californiaLove$Token %hum<% c(~list(Tone), by ~ File ~ Phrase)
Phrase <- californiaLove$Token %hum<% c(~list(Phrase), by ~ File ~ Phrase)
Break <- californiaLove$Token %hum<% c(~list(Break), by ~ File ~ Phrase)
Hype <- californiaLove$Token %hum<% c(~list(Hype), by ~ File ~ Phrase)


sigmoid <- function(x){
  value <- 1/(1+exp(-x))
  return(value)
}

rap <- rep(1, 124)

library(dbnR)
library(data.table)
data(motor)
size <- 3
one <- c("A","A", "A")
two <- c("D","D", "E")
three <- c("E", "F", "F")
four <- c("G", "G", "G")

data1<-data.frame(one)
data2<-data.frame(two)
data3 <- data.frame(three)
data4 <- data.frame(four)



blacklist <- c("data1_t_0", "data2_0")
blacklist <- rbind(blacklist, c("data1_t_2", "data2_t_1"))
blacklist_tr <- c("data3_t_1", "data4_t_0")
blacklist_tr <- rbind(blacklist_tr, c("data3_t_2", "data2_t_1"))
net <- dbnR::learn_dbn_struc(data1, size, method = "dmmhc", blacklist = blacklist,
                             blacklist_tr = blacklist_tr, restrict = "mmpc", maximize = "hc",
                             restrict.args = list(test = "cor"),
                             maximize.args = list(score = "bic-g", maxp = 10))

f_dt_train <- fold_dt(dt_train, size)
f_dt_val <- fold_dt(dt_val, size)
fit <- dbnR::fit_dbn_params(net, f_dt_train, method = "mle")

dbnR::plot_dynamic_network(fit)

library(visNetwork)

dbnR::plot_dynamic_network(fit)
