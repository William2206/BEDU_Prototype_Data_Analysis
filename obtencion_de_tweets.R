# install.packages(c("twitteR", "RColorBrewer", "plyr",
#                    "ggplot2", "devtools", "httr"))
# require(devtools)
# 
# install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
# install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

library(slam)
library(sentiment)
library(twitteR)


api_key <- "fLhZ1l48o1vAMXXUQzob8o6g0"
api_secret <- "tXYLvXyAQ0vnJs5pBqO1Jivv5bGjnJFrDqXesirmGxsqDBsNWF"
access_token <- "2705517390-731ZwVNQ512okLjdaJKBKlQ9fc8UfVDvEIaNEJR"
access_token_secret <- "KPDyffRYWMiE33oLFugrdINKQoTFj4X8mXptmV1NlxfSs"

setup_twitter_oauth(api_key, api_secret,
                    access_token, access_token_secret)

tweets <- searchTwitter("@Infonavit", n = 10000)
df_tweets <- do.call("rbind", lapply(tweets, as.data.frame))

texts <- sapply(tweets, function(x) x$getText())

clean.data <- function(text){
  #eliminar re-tweets y @ del texto original
  text = gsub("(RT|VIA)((?:\\b\\W*@\\w+)+)", "", text)
  #text = gsub("@\\w+", "", text)
  
  #eliminar signos de puntuación y dígitos del 0 al 9
  text = gsub("[[:punct:]]", "", text)
  text = gsub("[[:digit:]]", "", text)
  
  #eliminar links html, tabulaciones y espacios adicionales
  text = gsub("http\\w+","",text)
  text = gsub("[ \t]{2,}", "", text)
  text = gsub("^\\s+|\\s+$", "", text)
}
texts <- clean.data(texts)

library(stringi)
texts <- stri_trans_general(texts, "Latin-ASCII")

head(texts)

handle.error <- function(x){
  #crear el valor omitido
  y = NA
  #try_catch error
  try_error <- tryCatch(tolower(x), error=function(e) e)
  #si no hay error
  if(!inherits(try_error, "error"))
    y = tolower(x)
  #devolvemos el resultado
  return(y)
}

texts = sapply(texts, handle.error)

head(texts)

texts <- texts[!is.na(texts)]

names(texts) <- NULL
library(data.table)
texts <- as.data.frame(texts)
texts <- tm_map(texts,
                content_transformer(function(x) iconv(x, to='ASCII', sub='byte'))
)

texts <- iconv(texts,'utf-8','ascii', sub = '')

fwrite(texts, 'C:/Users/IN334839/Documents/BEDU_Prototype_Data_Analysis/texts.csv')



