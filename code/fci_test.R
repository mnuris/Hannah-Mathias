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

sample_all <- data[, c("fall_dt", "age", "sex", "canton" ,"vacc_status" ,"case_death")]

#ne prendre que 200 valeurs
sample_200 <- sample_all[c(1:200),]

#ne pas prendre les valeurs non définies (NA)
sample<- drop_na(sample_200)

