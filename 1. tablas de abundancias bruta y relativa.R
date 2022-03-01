
setwd("C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/data")

library(tidyverse)
library(readxl)

# juntamos las matrices de abundancias ####

# leemos las tablas
ucsc <- read_excel("allplots.xlsx", sheet = "UCSC", col_names = T)
sanjoaquin <- read_excel("allplots.xlsx", sheet = "SJER", col_names = T)
merced <- read_excel("allplots.xlsx", sheet = "M", col_names = T)
lahonda <- read_excel("allplots.xlsx", sheet = "LHC", col_names = T)
ventas <- read_excel("allplots.xlsx", sheet = "V", col_names = T)
navalagrulla <- read_excel("allplots.xlsx", sheet = "N", col_names = T)
montes1 <- read_excel("allplots.xlsx", sheet = "M1", col_names = T)
montes2 <- read_excel("allplots.xlsx", sheet = "M2", col_names = T)

# las juntamos con la funcion merge
# all=T retiene las filas de la tabla2 que no se encuentran en la tabla1
plots1 <- merge(ucsc, sanjoaquin, by.x = "species", by.y = "species", all=T)
plots2 <- merge(merced, lahonda, by.x = "species", by.y = "species", all=T)
plots3 <- merge(ventas, navalagrulla, by.x = "species", by.y = "species", all=T)
plots4 <- merge(montes1, montes2, by.x = "species", by.y = "species", all=T)
plots5 <- merge(plots1, plots2, by.x = "species", by.y = "species", all=T)
plots6 <- merge(plots3, plots4, by.x = "species", by.y = "species", all=T)
# plots final es nuestra tabla final
plotsfinal <- merge(plots5, plots6, by.x = "species", by.y = "species", all=T)

# eliminamos los objetos que nos sobran
rm(plots1, plots2, plots3, plots4, plots5, plots6)

rownames(plotsfinal) <- plotsfinal$species
plotsfinal$species <- NULL

# repasamos que haya 120 plots
dim(plotsfinal)

# cambiamos NA por 0
plotsfinal[is.na(plotsfinal)] <- 0

# y la transponemos porque nuestras observaciones son los plots y las especies las variables
plotsfinal <- t(plotsfinal)

# movemos site y region al principio
plots <- plotsfinal[,c(
  which(colnames(plotsfinal)=="site"),
  which(colnames(plotsfinal)=="region"),
  which(colnames(plotsfinal) != c("region", "site"))
)]

# eliminamos plotsfinal
rm(plotsfinal)

# como hay texto y variables numericas convertimos la matriz en data frame
plots <- as.data.frame(plots)
str(plots)
# clasificamos las variables correctamente
plots[,3:dim(plots)[2]] <- apply(plots[,3:dim(plots)[2]], 2, as.numeric)
str(plots)

# write.table(plots, 'results/plots.txt')


# matriz de abundancias relativas ####

# transformamos las abundancias brutas a coberturas %
class(plots)
# creamos una copia de plots para no tocar el original
plots2 <- plots

# creamos una matriz de las mismas dimensiones que la original
relative_cover <- matrix(nrow=dim(plots)[1] , ncol=dim(plots)[2])
rownames(relative_cover) <- rownames(plots)
colnames(relative_cover) <- colnames(plots)

# sumamos las abundancias de todas las especies para cada plot
plots2$total_cover <- rowSums(plots2[,3:dim(plots)[2]])

# rellenamos la nueva matriz con este loop
for (a in 1:dim(plots2)[1]) {
  
  for (c in colnames(plots2[,3:dim(plots)[2]])) {
    relative_cover[a,c] <- plots2[a,c]/plots2$total_cover[a]*100
    
  }
}

# comprobamos que todas las filas sumen 100
rowSums(relative_cover[,3:dim(plots)[2]])

# lo pasamos a dataframe
relative_cover <- as.data.frame(relative_cover)
relative_cover$site <- plots2$site
relative_cover$region <- plots2$region

# eliminamos plots2
rm(plots2)

