# csomagok betoltese ----
library(tidyverse)
library(cluster)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(stats)
library(dplyr)
library(ggfortify)


# working dir. beallitasa ----
setwd("C:/Users/bbali/OneDrive/Desktop/szakdolgozat/iris second cluster analysis/iris_cluster2")

# iris adathalmaz betoltese ----
iris_data <- iris

# ismerkedes az adathalmazzal ----
glimpse(iris_data)
head(iris_data)
View(iris_data)

# vizualizacio ----

## Cseszelevel-hossza vs. Cszeszelevel-szelessege

ggplot(iris_data)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width), stroke = 1)+
  facet_wrap(~ Species)+ 
  labs(x = "Cseszelevel-hossza", y = "Cseszelevel-szelessege")+
  theme_economist() +
  scale_fill_economist()


## Szirom-hossza vs. Szirom-szelessege

ggplot(iris_data)+
  geom_point(aes(x = Petal.Length, y = Petal.Width), stroke = 1)+
  facet_wrap(~ Species)+ 
  labs(x = "Szirom hossza", y = "Szirom szelessege")+
  theme_economist() +
  scale_fill_economist()

## Cseszelevel-hossza vs. Szirom-hossza

ggplot(iris_data)+
  geom_point(aes(x = Sepal.Length, y = Petal.Length), stroke = 1)+
  facet_wrap(~ Species)+ 
  labs(x = "Cseszelevel hossza", y = "Szirom hossza")+
  theme_economist() +
  scale_fill_economist()

## Cseszelevel-szelessege vs. Szirom-szelessege

ggplot(iris_data)+
  geom_point(aes(x = Sepal.Width, y = Petal.Width), stroke = 1)+
  facet_wrap(~ Species)+ 
  labs(x = "Cseszelevel-szelessege", y = "Szirom-szelessege")+
  theme_economist() +
  scale_fill_economist()

## Box plotok - EZEKET MIND KI KELL MEG MENTENI PNG-BEN !!!!!!! 

ggplot(iris_data)+
  geom_boxplot(aes(x = Species, y = Sepal.Length, fill = Species))+
  labs(x = "Tipus", y = "Cseszelevel-hossza")+
  theme_economist() +
  scale_fill_economist()
ggplot(iris_data)+
  geom_boxplot(aes(x = Species, y = Sepal.Width, fill = Species))+
  labs(x = "Tipus", y = "Cseszelevel-szelessege")+
  theme_economist() +
  scale_fill_economist()
ggplot(iris_data)+
  geom_boxplot(aes(x = Species, y = Petal.Length, fill = Species))+
  labs(x = "Tipus", y = "Szirom-hossza")+
  theme_economist() +
  scale_fill_economist()
ggplot(iris_data)+
  geom_boxplot(aes(x = Species, y = Petal.Width, fill = Species))+
  labs(x = "Tipus", y = "Szirom-szelessege")+
  theme_economist() +
  scale_fill_economist()

# K-means Clustering ----

## Optimalis klaszterszam megtalalasa, Elbow method hasznalataval

set.seed(123) # for reproduction
wcss <- vector()

for (i in 1:10) wcss[i] <- sum(kmeans(iris_data[, -5], i)$withinss)
plot(1:10,
     wcss,
     type = "b",
     main = paste("The Elbow Method"),
     xlab = "Klaszterek szama",
     ylab = "Klaszteren beluli negyzetosszegek"
)

## k(centers) = 3 <--- tehat 3 klaszterunk lesz


set.seed(123)
km <- kmeans( x = iris_data[, -5] , centers = 3)
yclus <- km$cluster
table(yclus)

## az eljaras 3 csoportba sorolta az adatokat: 
## 1. klaszter - 50 megfigyeles
## 2. klaszter - 62 megfigyeles
## 3. klaszter - 38 megfigyeles

## cimkezetlen ertekekkel kell dolgoznunk, az utolso oszlopot mellozni fogjuk
x = select(iris, c(1,2,3,4))

## optimalis klaszter szam: ahol jol latszik a gorbe elhajlasa
## ez jelen esetben: 3
KM = kmeans(x,3)

autoplot(KM, x, frame=TRUE)
## jol elkulonulnek a klaszterek, azt mondhatjuk, hogy sikeres volt a klaszterezes


iris_data$cluster.kmean <- yclus
cm <- table(iris_data$Species, iris_data$cluster.kmean)
cm

(50+48+36) / 150
# A k-means klaszter outputja 89,3% -ban egyezik a hozzatartozo klaszterrel
# A Versicolor (2. klaszter) es Virginica (3. klaszter) csoportokban van par
# atfedes, ami az elso klaszterezes soran nem latszik az abran


## Tiles plot : Tipusok vs. kmeans klaszter

mtable <- melt(cm)
ggplot(mtable)+
  geom_tile(aes(x = Var1, y = Var2, fill = value))+
  labs(x = "Tipus", y = "kmeans klaszter")+
  theme_bw()


# SCATTER PLOTOK ----

## Cseszelevel-hossza vs. Cseszelevel-szelessege

iris_data$cluster.kmean <- as.factor(iris_data$cluster.kmean)

# Cseszelevel-hossza vs. Cseszelevel-szelessege (tipusonkent)
ggplot(iris_data)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, 
                 color = Species) , size = 5)+ 
  labs(x = "Cseszelevel hossza", y = "Cseszelevel szelessege")+
  ggtitle("Tipusok")+
  theme_economist() +
  scale_fill_economist()


# Cseszelevel-hossza vs. Cseszelevel-hossza (klaszterenkent)
ggplot(iris_data)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, 
                 color = cluster.kmean) , size = 5)+ 
  labs(x = "Cseszelevel hossza", y = "Cseszelevel szelessege")+
  ggtitle("kmeans klaszter")+
  theme_economist() +
  scale_fill_economist()

# Szirom-hossza vs. Szirom-szelessege (tipusoknkent)

ggplot(iris_data)+
  geom_point(aes(x = Petal.Length, y = Petal.Width, 
                 color = Species) , size = 5)+ 
  labs(x = "Szirom hossza", y = "Szirom szelessege")+
  ggtitle("Tipusok")+
  theme_economist() +
  scale_fill_economist()


# Szirom-hossz vs. Szirom-szelessege (klaszterenkent)
ggplot(iris_data)+
  geom_point(aes(x = Petal.Length, y = Petal.Width, 
                 color = cluster.kmean) , size = 5)+ 
  labs(x = "Szirom hossza", y = "Szirom-szelessege")+
  ggtitle("kmeans klaszter")+
  theme_economist() +
  scale_fill_economist()