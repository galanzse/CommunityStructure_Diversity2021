
source('C:/Users/Javier/OneDrive/TESIS Y PUBLICACIONES/TESIS/3. Community plant structure of exotic species at home and abroad/analyses/scripts/3. tabla de riqueza, cobertura y shannon por plot.R')


# install.packages("lmerTest")
library(lmerTest)
# install.packages('MuMIn')
library(MuMIn)



# data
com_met


# exploramos los datos ####

# Cleveland dotplots
dotchart(x = com_met$richness, groups = com_met$site, main='richness')
dotchart(x = com_met$shannon, groups = com_met$site, main='shannon')

# se observan dos patrones
    # 1/ los plots espanoles son mas ricos y diversos 
    # 2/ la varianza de riqueza y shannon varia mucho entre sitios



# modelos ####

# within sites ####

# preparo la tabla longitudinal para Espana para poder hacer un modelo mixto

rich_sp <- com_met %>%
      subset(region == "Spain") %>%
      select(site, richness_natives, richness_exotics)
rich_sp$plot <- rownames(rich_sp)
colnames(rich_sp)[2:3] <- c("native", "exotic")
rich_sp <- gather(rich_sp, "origin", "richness", 2:3)

cov_sp <- com_met %>%
  subset(region == "Spain") %>%
  select(site, cover_natives, cover_exotics)
cov_sp$plot <- rownames(cov_sp)
colnames(cov_sp)[2:3] <- c("native", "exotic")
cov_sp$native <- log(cov_sp$native)
cov_sp$exotic <- log(cov_sp$exotic)
cov_sp <- gather(cov_sp, "origin", "cover", 2:3)

com_met_SP_lon <- merge(rich_sp, cov_sp, by=c("site", "plot", "origin"))


# richness and cover natives vs colonisers within Spain
mod <- lmer(cover ~ 1 + origin + (1|site/plot),
            data = com_met_SP_lon, REML = T)
anova(mod)


mod <- lmer(richness ~ 1 + origin + (1|site/plot),
             data = com_met_SP_lon)
anova(mod)


# preparo la tabla longitudinal para California para poder hacer un modelo mixto

rich_cal <- com_met %>%
  subset(region == "California") %>%
  select(site, richness_natives, richness_exotics)
rich_cal$plot <- rownames(rich_cal)
colnames(rich_cal)[2:3] <- c("native", "exotic")
rich_cal <- gather(rich_cal, "origin", "richness", 2:3)

cov_cal <- com_met %>%
  subset(region == "California") %>%
  select(site, cover_natives, cover_exotics)
cov_cal$plot <- rownames(cov_cal)
colnames(cov_cal)[2:3] <- c("native", "exotic")
cov_cal$native <- log(cov_cal$native)
cov_cal$exotic <- log(cov_cal$exotic)
cov_cal <- gather(cov_cal, "origin", "cover", 2:3)

com_met_cal_lon <- merge(rich_cal, cov_cal, by=c("site", "plot", "origin"))
com_met_cal_lon[com_met_cal_lon == "-Inf"] <- 0


# richness and cover natives vs colonisers within California
mod <- lmer(cover ~ 1 + origin + (1|site/plot),
            data = com_met_cal_lon, REML = T)
anova(mod)


mod <- lmer(richness ~ 1 + origin + (1|site/plot),
             data = com_met_cal_lon)
anova(mod)




# riqueza total entre sitios  ####

plot(x=com_met$site, y=com_met$richness, ylab="richness", xlab="region", las=2)


mod <- glmer(richness ~ 1 + region + (1|site), data = com_met, family = 'poisson')


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))



 
# riqueza nativas entre sitios ####

par(mfrow = c(1, 1))
plot(x=com_met$site, y=com_met$richness_natives, ylab="richness", xlab="region")


mod <- glmer(richness_natives ~ 1 + region + (1|site), data = com_met, family = 'poisson')


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))




# riqueza exoticas entre sitios ####

par(mfrow = c(1, 1))
plot(x=com_met$site, y=com_met$richness_natives, ylab="richness", xlab="region")


mod <- glmer(richness_exotics ~ 1 + region + (1|site), data = com_met, family = 'poisson')


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))




# shannon total entre sitios ####

par(mfrow = c(1, 1))
plot(x=com_met$site, y=com_met$shannon, ylab="shannon", xlab="region", las=2)


mod <- lmer(shannon ~ 1 + region + (1|site), data = com_met, REML = T)


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))




# shannon nativas entre sitios ####

par(mfrow = c(1, 1))
plot(x=com_met$site, y=com_met$shannon_natives, ylab="richness", xlab="region")


mod <- lmer(shannon_natives ~ 1 + region + (1|site), data = com_met)


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))


mean(com_met$shannon_natives[com_met$region == 'Spain'])
sd(com_met$shannon_natives[com_met$region == 'Spain'])
mean(com_met$shannon_natives[com_met$region == 'California'])
sd(com_met$shannon_natives[com_met$region == 'California'])




# shannon exoticas entre sitios ####

par(mfrow = c(1, 1))
plot(x=com_met$site, y=com_met$shannon_exotics, ylab="richness", xlab="region")

# riqueza esta normalmente distribuida
# hay heterocedasticidad entre sitios

mod <- lmer(shannon_exotics ~ 1 + region + (1|site), data = com_met)


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))


mean(com_met$shannon_exotics[com_met$region == 'Spain'])
sd(com_met$shannon_exotics[com_met$region == 'Spain'])
mean(com_met$shannon_exotics[com_met$region == 'California'])
sd(com_met$shannon_exotics[com_met$region == 'California'])




# cobertura exoticas entre sitios ####
# los residuos quedan mucho mejor con la arcsin transformation, pero la significacion no cambia,
# y asi utilizo log_trans para todos los modelos

par(mfrow = c(1, 1))
plot(x=com_met$site, y=com_met$cover_exotics, ylab="cover_exotics", xlab="site")


mod <- lmer(com_met$ln_cover_exotics ~ 1 + region + (1|site), data = com_met)


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))


rm(mod)




com_met$ln_cover_natives <- log(1-com_met$cover_exotics/100)
com_met$ln_cover_natives[com_met$ln_cover_natives == "-Inf"] <- 0

mod <- lmer(com_met$ln_cover_natives ~ 1 + region + (1|site), data = com_met, REML = T)


summary(mod)
anova(mod)
r.squaredGLMM(mod)


plot(mod)
plot(mod, site ~ resid(.))
par(mfrow = c(1, 2))
qqnorm(resid(mod))
qqline(resid(mod))
qqnorm(unlist(ranef(mod)))
qqline(unlist(ranef(mod)))


rm(mod)







