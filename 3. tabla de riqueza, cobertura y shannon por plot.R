
source('C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/scripts/1. tablas de abundancias bruta y relativa.R')


# impacto de especies exoticas en la flora local Martin-Fores et al 2017

# vamos a trabajar con especies que aparezcan en mas de 10 plots para asegurar un tamano muestral adecuado
# view(Nplots_spp)
# commons10 <- Nplots_spp[which(Nplots_spp[,'Nplots spain'] > 10 & Nplots_spp[,'Nplots california'] > 10),]


# recuperamos la matriz de abundancias
class(relative_cover)
relative_cover4 <- relative_cover
relative_cover4$site <- NULL
relative_cover4$region <- NULL

# importamos una tabla con informaci?n de invasiveness para todas las especies,
# porque algunas estan en plots pero no en traits
plot_traits <- read_excel("allplots.xlsx", sheet = "plot_traits", col_names = T)


# vamos a crear una tabla con 6 variables: shannon natives Y exotics,
# richness natives y exotics, cover natives y exotics
# creamos una matriz de resultados
com_met <- matrix(ncol = 10, nrow = 120)
colnames(com_met) <- c('shannon_natives', 'shannon_exotics', 'shannon',
                       'richness_natives', 'richness_exotics', 'richness',
                       'cover_natives', 'cover_exotics',
                       'ln_cover_exotics', 'arcsin_cover_exotics')
rownames(com_met) <- rownames(relative_cover4)

# loop

library(vegan)

for (r in rownames(relative_cover4)) {
  
  # dataframe con informacion del plot r
  mat <- as.data.frame(relative_cover4[r,])
  mat <- t(mat)
  mat <- as.data.frame(mat)
  colnames(mat) <- "abundance"
  mat$species <- rownames(mat)
  rownames(mat) <- 1:length(mat$species)
  
  # eliminamos especies ausentes
  mat <- filter(mat, abundance > 0)
  
  # trait info
  mat <- left_join(mat, plot_traits, by="species")
  # separamos exoticas y nativas
  nat <- subset(mat, origin=="native")
  exo <- subset(mat, origin=="exotic")
  
  
  
  # calculamos total richness y shannon
  com_met[r,'richness'] <- length(mat$species)
  
  mat <- mat[,1:2]
  rownames(mat) <- mat$species
  mat$species <- NULL
  com_met[r,'shannon'] <- diversity(mat, index = "shannon", MARGIN = 2)
  

  
  # calculamos  riqueza y abundancia de nativas y exoticas
  com_met[r,'richness_natives'] <- length(nat$species)
  com_met[r,'richness_exotics'] <- length(exo$species)
  com_met[r,'cover_natives'] <- sum(nat$abundance)
  com_met[r,'cover_exotics'] <- sum(exo$abundance)
  
  
  # calculamos shannon
  # nativas
  
  if (length(nat$species) > 0){
    
    nat <- nat[,1:2]
    rownames(nat) <- nat$species
    nat$species <- NULL
    com_met[r,'shannon_natives'] <- diversity(nat, index = "shannon", MARGIN = 2)
    
  } else {
    com_met[r,'shannon_natives'] <- 0
  }
  
  # exoticas
  
  if (length(exo$species) > 0){
    
    exo <- exo[,1:2]
    rownames(exo) <- exo$species
    exo$species <- NULL
    com_met[r,'shannon_exotics'] <- diversity(exo, index = "shannon", MARGIN = 2)
    
  } else {
    com_met[r,'shannon_exotics'] <- 0
  }
  
  rm(mat, nat, exo)
}


rm(relative_cover4)

View(com_met)
com_met <- as.data.frame(com_met)

# dejamos la cobertura transformada para probar en los modelos
com_met$arcsin_cover_exotics <- asin(sqrt(com_met$cover_exotics/100))
com_met$ln_cover_exotics <- log(com_met$cover_exotics/100)


# pegamos la info de site y region
com_met <- cbind(relative_cover[,1:2], com_met)
View(com_met)










