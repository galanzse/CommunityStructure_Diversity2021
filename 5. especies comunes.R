source('C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/scripts/1. tablas de abundancias bruta y relativa.R')


# especies presentes en California y Espana ####

# creamos un vector con los nombres de las especies de Espana
spain <- subset(plots, region=="Spain")
spain$region <- NULL
spain$site <- NULL
spain_species <- as.data.frame(colSums(spain))
spain_species <- as.data.frame(which(colSums(spain)>0))
spain_species <- rownames(spain_species)

# creamos un vector con los nombres de las especies de California
california <- subset(plots, region=="California")
california$region <- NULL
california$site <- NULL
california_species <- as.data.frame(colSums(california))
california_species <- as.data.frame(which(colSums(california)>0))
california_species <- rownames(california_species)

# comparamos los vectores y nos quedamos con los elementos en comun
commons <- Reduce(intersect, list(spain_species,california_species))

# eliminamos Trifolium sp
commons <- commons[-which(commons == "Trifolium sp")]
# segun CalFlora Juncus bufonius es nativa de California
commons <- commons[-which(commons == "Juncus bufonius")]





