library(slam)
library(sentiment)
library(RColorBrewer)
library(ggplot2)

base_tweets_clean <- read.csv("C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/datos/tweets_Infonavit-clean.csv")

##analisis de sentimiento
class_emo <- classify_emotion(base_tweets_clean, algorithm = "bayes", prior = 1)
head(class_emo)

emotion <- class_emo[,7]
emotion[is.na(emotion)]<-"unknown"
head(emotion)

#analisis de la polaridad
class_pol <- classify_polarity(base_tweets_clean, algorithm = "bayes")
head(class_pol)

polarity <- class_pol[,4]

sent_df <- data.frame(text = base_tweets_clean, 
                      emotion = emotion, polarity = polarity, stringsAsFactors = F)

sent_df <- within(sent_df, emotion <- factor(emotion, levels = names(sort(table(emotion), decreasing= T))))

emocion <- ggplot(sent_df, aes(x=emotion))+
  geom_bar(aes(y = ..count.., fill=emotion))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Categorías de emocion", y = "Número de Tweets")+
  labs(title = "Análisis de Sentimiento acerca de Infonavit")

ggsave(emocion, 
       file = "C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/imagenes/emocion.png", 
       height = 8, width = 15, units="in", dpi = 300)

polaridad <- ggplot(sent_df, aes(x=polarity))+
  geom_bar(aes(y = ..count.., fill = polarity))+
  scale_fill_brewer(palette = "Set3")+
  labs(x="Categorías de polaridad", y = "Número de Tweets")+
  labs(title="Análisis de Sentimiento acerca de Infonavit")

ggsave(polaridad, 
       file = "C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/imagenes/polaridad.png", 
       height = 8, width = 15, units="in", dpi = 300)
