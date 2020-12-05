# install.packages(c("twitteR", "RColorBrewer", "plyr",
#                    "ggplot2", "devtools", "httr"))
# require(devtools)
# 
# install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
# install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

library(slam)
library(sentiment)
library(twitteR)


api_key <- "QKjDC3rPXjGifSNnE5WFvNi8j"
api_secret <- "UHp3UfOdFdKRRdEbhtRxhuHG6g9NQ8NvhublfD5bQLTuP2lPUj"
access_token <- "2705517390-5hUOgMLry1aMl76cAUfnpwBCP1TbZhfqkaZXCdH"
access_token_secret <- "LH6RutQkH5dYR734y366gNjGkZTXsJOTYWlQOGMAwvU2w"

setup_twitter_oauth(api_key, api_secret,
                    access_token, access_token_secret)

tweets_Infonavit <- searchTwitter("@Infonavit", n = 10000)
tweets_Fovissste <- searchTwitter("@FOVISSSTEmx", n = 10000)
#df_tweets <- do.call("rbind", lapply(tweets, as.data.frame))

texts_Infonavit <- sapply(tweets_Infonavit, function(x) x$getText())
texts_Fovissste <- sapply(tweets_Fovissste, function(x) x$getText())

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

texts_Infonavit <- clean.data(texts_Infonavit)
texts_Fovissste <- clean.data(texts_Fovissste)

library(stringi)
texts_Infonavit <- stri_trans_general(texts_Infonavit, "Latin-ASCII")
texts_Fovissste <- stri_trans_general(texts_Fovissste, "Latin-ASCII")

head(texts_Infonavit)
head(texts_Fovissste)

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

texts_Infonavit = sapply(texts_Infonavit, handle.error)
texts_Fovissste = sapply(texts_Fovissste, handle.error)

head(texts_Infonavit)
head(texts_Fovissste)

texts_Infonavit <- texts_Infonavit[!is.na(texts_Infonavit)]
texts_Fovissste <- texts_Fovissste[!is.na(texts_Fovissste)]

names(texts_Infonavit) <- NULL
names(texts_Fovissste) <- NULL

library(data.table)
texts_Infonavit <- as.data.frame(texts_Infonavit)
fwrite(texts_Infonavit, 'C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/tweets_Infonavit.csv')

texts_Fovissste <- as.data.frame(texts_Fovissste)
fwrite(texts_Fovissste, 'C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/tweets_Fovissste.csv')


