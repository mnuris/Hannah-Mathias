library("pcalg")
library(tidyr)
library(igraph)
library("Rgraphviz")


dataset <- read.csv("EPFL/labimmersion/data/data_for_hannah.csv")
dataset$poor <- as.numeric(dataset$householdIncome < 6000)
dataset$pub_transp <- as.numeric(dataset$carAvail == 'never')
dataset$city <- as.numeric(dataset$municipalityType == 'urban')

sample <- dataset[, c('poor', 'infected', 'pub_transp', 'city')]

sample$infected <-as.numeric(sample$infected=='True')
sample$female <-as.numeric(dataset$sex=='f')

data = list(x = sample, g = graph_from_data_frame(sample, directed = FALSE, vertices = NULL))

#age groups: 0-18 ; 18-55; 55 - inf
#sample$age[sample$age < 18] <- 'child'
#sample$age[sample$age > 55] <- 'elderly'
#sample$age[(sample$age > 18) & (sample$age< 55)] <- 'adult'

#sufficient stats
dm_ <- as.matrix(data$x)
ss <- list(dm = dm_, adaptDF = TRUE) #true ou false ?
fci.fit <- fci(suffStat = ss, indepTest = disCItest(), alpha = 0.05,labels = as.character(1:5))

#renvoie l'erreur : Erreur dans is.data.frame(dm <- suffStat$dm) : 
#l'argument "suffStat" est manquant, avec aucune valeur par défaut

par(mfrow = c(1,2))

plot(data$g, main = "graph")
plot(fci.fit, main = "fci fit")

