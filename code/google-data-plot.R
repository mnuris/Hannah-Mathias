#use Google infection data
library(readxl)
epidemiology <- read.csv("EPFL/labimmersion/data/epidemiology.csv")

#partie 2

#prendre uniquement les cas confirmés
#prendre uniquement les 3 premiers mois ?
confirmed_google_ <- epidemiology[c("cumulative_confirmed")]
confirmed_google <- confirmed_google_[c(4501:4591),]
#enlever les cas des jours d'avant pour ne prendre que les cas additionnels chaque jour
confirmed_google_norm <- confirmed_google - 1484

#vecteur avec les valeurs du modèle
source("EPFL/labimmersion/code/modele-calibrage-children.R")

model_cumulative <- cumsum(output[c("I1")] + output[c("I2")] + output[c("I3")])

#plot les deux courbes l'une sur l'autre
#je fais une fausse deuxième courbe pour faire un essai de plot
plot(confirmed_google_norm,type="l",col="red")
lines(model_cumulative,col="green")
