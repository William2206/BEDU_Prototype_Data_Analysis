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

tweets_INFONAVIT <- searchTwitter("@Infonavit", n = 10000)

#tweets1 <- searchTwitter('from:FOVISSSTEmx', resultType="recent", n=10000)
#tweets2 <- searchTwitter('from:Infonavit', resultType="recent", n=10000)
#tweets3 <- searchTwitter("#Infonavit", n = 150000, lang = "es", geocode='25.67507,-100.31847,100mi')
#tweets4 <- searchTwitter("#Infonavit", n = 150000, lang = "es", geocode='22.14982,-100.97916,100km')
#tweets5 <- searchTwitter("Infonavit", n = 150000, lang = "es")
#tweets6 <- searchTwitter('#FOVISSSTE', n=10000, since='2020-10-01', until='2020-10-31')

#tweets <- searchTwitter("#Infonavit", n = 150000, since = '2020-09-01', until = '2020-12-01',
#                        geocode='19.42847,-99.12766,200mi')
df <- do.call("rbind", lapply(tweets, as.data.frame))

userInfo <- lookupUsers(df$screenName)
userFrame <- twListToDF(userInfo)
head(userFrame)
locatedUsers <- !is.na(userFrame$location)

library(dismo)
library(maps)


locations <- geodcode(userFrame$location[locatedUsers])
head(locations)

with(locations, plot(longitude, latitude))

texts <- sapply(tweets, function(x) x$getText())

clean.data <- function(text){
  #eliminar re-tweets y @ del texto original
  text = gsub("(RT|VIA)((?:\\b\\W*@\\w+)+)", "", text)
  text = gsub("@\\w+", "", text)
  
  #eliminar signos de puntuación y dígitos del 0 al 9
  text = gsub("[[:punct:]]", "", text)
  text = gsub("[[:digit:]]", "", text)
  
  #eliminar links html, tabulaciones y espacios adicionales
  text = gsub("http\\w+","",text)
  text = gsub("[ \t]{2,}", "", text)
  text = gsub("^\\s+|\\s+$", "", text)
}

texts <- clean.data(texts)

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


##analisis de sentimiento
class_emo <- classify_emotion(texts, algorithm = "bayes", prior = 1)

head(class_emo)

emotion <- class_emo[,7]
emotion[is.na(emotion)]<-"unknown"
head(emotion)

#analisis de la polaridad

class_pol <- classify_polarity(texts, algorithm = "bayes")

head(class_pol)

polarity <- class_pol[,4]

sent_df <- data.frame(text = texts, 
                      emotion = emotion, polarity = polarity, stringsAsFactors = F)

sent_df <- within(sent_df, emotion <- factor(emotion, levels = names(sort(table(emotion), decreasing= T))))

library(RColorBrewer)
library(ggplot2)

ggplot(sent_df, aes(x=emotion))+
  geom_bar(aes(y = ..count.., fill=emotion))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Categorías de emocion", y = "Número de Tweets")+
  labs(title = "Análisis de Sentimiento acerca de Machine Learning")


ggplot(sent_df, aes(x=polarity))+
  geom_bar(aes(y = ..count.., fill = polarity))+
  scale_fill_brewer(palette = "Set3")+
  labs(x="Categorías de polaridad", y = "Número de Tweets")+
  labs(title="Análisis de Sentimiento acerca de Machine Learning")