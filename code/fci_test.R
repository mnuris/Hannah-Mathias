library("pcalg")
library(tidyr)
library(igraph)
library("Rgraphviz")


data <- read.csv2("T:/2021-10-22_10-03-52_BAGEPI-1086-cortes_study_data_cases.csv")

#renaming columns with difficult names
colnames(data)[1] <- "id"
colnames(data)[3] <- "age"
colnames(data)[5] <- "canton"
colnames(data)[6] <- "munic"
colnames(data)[7] <- "nip"

#case_death (column 11), 1= died and 0 = didn't die
data[11][is.na(data[11])] <- 0


#sample to use for test: columns fall_dt, canton, sex,
#canton, case death

#pas pris les autres car ce sont des dates, ne fonctionne 
#peut-être pas si l'évènement n'a pas eu lieu

sample_all <- data[, c("age", "sex","case_death")]

#only 200 rows, a subset of our data
sample_200 <- sample_all[c(1:15000),]

#drop NA values
sample<- drop_na(sample_200)

#age groups: 0-18 ; 18-55; 55 - inf
sample$age[sample$age < 18] <- 'child'
sample$age[sample$age > 55] <- 'elderly'
sample$age[(sample$age > 18) & (sample$age< 55)] <- 'adult'

#début du code du tuto
op.orig <-
  options(SweaveHooks=
            list(fig=function() par(mar=c(5.1, 4.1, 1.1, 2.1))),
          width = 75,
          digits = 5,
          prompt = "R> ")

###################################################
#conversion de la matrice en numerical values

sample$sex <- xtfrm(select(sample, 'sex')$sex)
sample$age <- xtfrm(select(sample, 'age')$age)

#y <- select(sample, 'canton')
#sample$canton <- xtfrm(y$canton)

#sample$fall_dt <- xtfrm(select(sample, 'fall_dt')$fall_dt)


#matrice : sample (équivalent de "gmG$x" dans l'exemple)
#graph : à créer
###################################################

graph <- graph_from_data_frame(sample, directed = FALSE, vertices = NULL)

#################
#suffStat1 <- list(C = cor(sample), n = nrow(sample))
#pag.est <- fci(suffStat1, indepTest = gaussCItest,
#               p = ncol(sample), alpha = 0.01)
#par(mfrow = c(1,2))

#plot(graph, main = "")
#plot(pag.est)

##################
#skeleton
suffStat <- list(dm = as.matrix(sample), adaptDF = FALSE)
skel.fit <- skeleton(suffStat, indepTest = disCItest,
                     p = ncol(sample), alpha = 0.01)
par(mfrow = c(1,2))
plot(graph, main = "")
plot(skel.fit, main = "")

#fci fit
fci.fit <- fci(suffStat, indepTest = disCItest,
             p = ncol(sample), alpha = 0.01)
par(mfrow = c(1,2))
plot(graph, main = "graph")
plot(fci.fit, main = "fci fit")
