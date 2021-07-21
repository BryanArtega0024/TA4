#libreria
library(ggplot2)

#exportamos datos
datos <- read.csv("C:/R/datos.csv", sep=";")
View(datos);


#instaciamos datos
plot(datos);




distancia <- dist(datos)
dcompleta <- hclust(distancia, method = "complete")
plot(dcompleta)



abline(h=2 )
rect.hclust(dcompleta, k=4, border = "blue")
d2 <- hclust(distancia, method = "ward.D2")
plot(d2)
abline(h=2, col = "green")
dl2 <- cutree(d2, h=2)
table(dl2)



clust_datos <- cutree(dcompleta, h = 3)
table(clust_datos)
km_datos <- kmeans(scale(datos), centers = 3,
                   nstart = 20, iter.max = 50)
km_datos$cluster



plot(datos, col = km_datos$cluster)
points(km_datos$centers, cex= 2, col= 11,pch = 19)
segment_customers <- cbind(datos, cluster = clust_datos)
View(segment_customers)

