

source('C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/scripts/6. comparacion biogeografica comunes tablas.R')


# hacemos LMM con la estructura random species (species/site/plot) Firn

# datos
log_table
str(log_table)

# cover esta transformada arc-sine
hist(log_table$cover, 30) 


library(lmerTest)
# install.packages('multcomp')
library(multcomp)
library(MuMIn)
library(car)

# COBERTURA general ####

mod1 <- lmer(ln_cover ~ 1 + region + (1|species/site), data = log_table)


# el modelo es bien
plot(mod1)
plot(mod1, region ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod1))
qqline(resid(mod1))
qqnorm(unlist(ranef(mod1)))
qqline(unlist(ranef(mod1)))

r.squaredGLMM(mod1)

# veo que info me da el modelo
summary(mod1)
anova(mod1)

rm(mod1)


# occurrence general ####

mod1 <- lmer(ln_occurrence ~ 1 + region + (1|species) + (1|site), data = occurrence_data)
# no me deja anadir el efecto de sitio por falta de tamano muestral


# el modelo es bien
plot(mod1)
plot(mod1, region ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod1))
qqline(resid(mod1))
qqnorm(unlist(ranef(mod1)))
qqline(unlist(ranef(mod1)))

r.squaredGLMM(mod1)


# veo que info me da el modelo
summary(mod1)
anova(mod1)

rm(mod1)

# COBERTURA por categoria de invasiveness ####

# no puedo usar plot como random factor porque tienes demasiados niveles
# Error: number of levels of each grouping factor must be < number of observations


# creo una variable sitioxinvasiveness
log_table2 <- log_table
log_table2$region_invasiveness <- paste(log_table2$region, log_table2$invasiveness)


mod1 <- lmer(ln_cover ~ 1 + region * invasiveness + (1|region/site) + (1|species), data = log_table2)

# guardo la tabla para los barplots
write.table(summary(mod1)$coefficients, 'C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/results/ln_cover_regionxinvasiveness.txt')


# el modelo es bien
plot(mod1)
plot(mod1, region_invasiveness ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod1))
qqline(resid(mod1))
qqnorm(unlist(ranef(mod1)))
qqline(unlist(ranef(mod1)))


r.squaredGLMM(mod1)

# veo que info me da el modelo
summary(mod1)
anova(mod1)
Anova(mod1, type="III")

# posthoc
summary(glht(model = mod1, linfct = mcp(region_invasiveness = "Tukey")))


rm(mod1)

# occurrence por categoria de invasiveness ####



# occurrence la transformamos log. igual que Pearson 2017
mod1 <- lmer(ln_occurrence ~ 1 + region_invasiveness + (1|species) + (1|site), data = occurrence_data)

# guardo la tabla para los barplots
write.table(summary(mod1)$coefficients, 'C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/results/ln_occurrence_regionxinvasiveness.txt')


# no me deja anadir el efecto de sitio por falta de tamano muestral

# el modelo es bien
plot(mod1)
plot(mod1, region_invasiveness ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod1))
qqline(resid(mod1))
qqnorm(unlist(ranef(mod1)))
qqline(unlist(ranef(mod1)))

r.squaredGLMM(mod1)

# veo que info me da el modelo
summary(mod1)
anova(mod1)


# posthoc
summary(glht(model = mod1, linfct = mcp(region_invasiveness = "Tukey")))


rm(mod1)

    
    
    
    
    
    
    
        



  
  

