library(tidyverse)
library(data.table)
library("tidyverse")
library(lubridate)
library(cluster)
library(factoextra)
library(NbClust)

base_cluster <- fread("C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/datos/base_cluster.csv")
names(base_cluster)
# str(base_cluster)
base_cluster <- base_cluster %>% select(norte, Mas_de_02_y_hasta_04_UMA, 
                                        Vivienda_Popular, periodo, variacion)

base_cluster[, name:= periodo]

analisis_cluster <- as.data.frame(base_cluster)
rownames(analisis_cluster) = analisis_cluster$name
analisis_cluster$name = NULL
analisis_cluster$periodo = NULL

analisis_cluster <- as.data.frame(scale(analisis_cluster))

#-------------------------------------------------------------------------------
# calcular la matriz de distancias
m.distancia <- get_dist(analisis_cluster, method = "euclidean")
im_matriz_dist <- fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#===============================================================================
# k-means
#===============================================================================
# encontrar el optimo k

resnumclust <- NbClust(analisis_cluster, distance = "euclidean", min.nc = 2, max.nc = 9, 
                       method = "kmeans", index = "alllong")

im_k_optimo <- fviz_nbclust(resnumclust) + theme_minimal()

# Aplicar el algoritmo de k-means con k óptimo
set.seed(29) 
kmeans <- kmeans(analisis_cluster, 3, iter.max = 300, nstart = 10)

#Visualización de los clusters
im_clus_kmeans <- fviz_cluster(kmeans, data = analisis_cluster, main = 'Cluster colocacion de créditos Infonavit')
dev.off()

#===============================================================================
# Clusterting Jerárquico
#===============================================================================

# Utilizar el dendrograma para encontrar el número óptimo de clusters
res_dendrogram <- hcut(analisis_cluster, k = 3, stand = T)
im_dendrograma <- fviz_dend(res_dendrogram, rect = T, cex = .5, k_colors = c("red", "black", "green"))
# Ajustar el clustering jerárquico a nuestro dataset
hc = hclust(dist(analisis_cluster, method = "euclidean"), method = "ward.D")
y_hc = cutree(hc, k = 3)

# Visualizar los clusters

clusplot(analisis_cluster, 
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 3,
         plotchar = FALSE,
         span = TRUE,
         main = "Cluster colocacion de créditos Infonavit",
         xlab = "x",
         ylab = "y")

ggsave(im_matriz_dist, 
       file = "C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/imagenes/im_matriz_dist.png", 
       height = 8, width = 15, units="in", dpi = 300)

ggsave(im_k_optimo, 
       file = "C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/imagenes/im_k_optimo.png", 
       height = 8, width = 15, units="in", dpi = 300)

ggsave(im_clus_kmeans, 
       file = "C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/imagenes/im_clus_kmeans.png", 
       height = 8, width = 15, units="in", dpi = 300)

ggsave(im_dendrograma, 
       file = "C:/Users/IN334839/Desktop/Cursos/Data Analysis/Prototype/imagenes/im_dendrograma.png", 
       height = 8, width = 15, units="in", dpi = 300)

