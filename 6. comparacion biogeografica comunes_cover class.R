
library(tidyverse)

# selecciono comunes y anado variable region
com.pl <- plots[,commons]
com.pl$region <- c(rep("California",60), rep("Spain",60))

# COMUNES ####

# creamos una matriz de resultados
commons_class <- matrix(nrow = length(commons), ncol = 2)
rownames(commons_class) <- commons
colnames(commons_class) <- c("mean_class_spain",
                             "mean_class_california")

# rellenamos la matriz de resultados
spain_cl <- subset(com.pl, region=='Spain')
spain_cl[spain_cl==0] <- NA

california_cl <- subset(com.pl, region=='California')
california_cl[california_cl==0] <- NA

for (a in rownames(commons_class)) {
  
  b <- spain_cl[,a]
  commons_class[a,"mean_class_spain"] <- mean(b[b!=0], na.rm = T)
  
  b <- california_cl[,a]
  commons_class[a,"mean_class_california"] <- mean(b[b!=0], na.rm = T)
}


class(commons_class)
str(commons_class)
# transformamos commons_class en un data frame
commons_class <- as.data.frame(commons_class)


# miro el plot de cover at home and abroad
plot(commons_class)
abline(1,1)

# regression cover ~ rel_ab
commons_class
commons_cover


rownames(commons_class) == rownames(commons_cover)

plot(commons_cover$mean_cover_spain, commons_class$mean_class_spain)
abline(lm(commons_class$mean_class_spain ~ commons_cover$mean_cover_spain))


# GIVEN PLOT ####

rc <- relative_cover[1,][which(relative_cover[1,]!=0)] %>% t()
cc <- plots[1,][which(plots[1,]!=0)] %>% t()

rc.cc <- cbind(rc[3:18,], cc[3:18,]) %>% as.data.frame()
colnames(rc.cc) <- c("relative", "class")
str(rc.cc)
rc.cc$relative <- as.numeric(rc.cc$relative)
rc.cc$class <- as.numeric(rc.cc$class)

plot(rc.cc)

sum(rc.cc$class)
sum(rc.cc$relative)

# d.rel <- dist(rc.cc$relative)
# d.cls <- dist(rc.cc$class)
# plot(d.rel, d.cls)
