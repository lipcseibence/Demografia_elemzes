library(rgeos)
library(ggplot2)
library(mapproj)
library(questionr)
library(dplyr)
library(plyr)
library(spdep)

# Fájlok beolvasása

setwd("E:/Egyetem/Hetedik félév/MNB verseny")
fajlok <- list("Termékenységi arányszám NUTS 3.xlsx", "Az anya átlagos életkora.xlsx",
               "Az anya életkorának mediánja.xlsx")

adatbazisok <- list()
for ( i in 1:3) {
  adatbazisok[[i]] <- readxl::read_xlsx(fajlok[[i]])
}

# Országkódok a térképhez (kód innen: http://viktoriawagner.weebly.com/blog/euromed-base-map-in-r)

ctry <- c("Poland", "Hungary", "Czech Republic", "Slovakia", "Belgium", "Bulgaria",
          "Denmark", "Germany", "Greece", "Spain", "France",
          "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania", "Luxembourg",
          "Hungary", "Malta", "Netherlands", "Austria", "Poland", "Portugal",
          "Romania", "Slovenia", "Slovakia", "Finland", "Sweden", "United Kingdom",
          "Liechtenstein", "Norway", "Switzerland")

lista <- list()
for (i in 1:32){
  lista[[i]] <- raster::getData("GADM", country = ctry[i], level = 1)
}


abrakeszito <- function(adatbazis, valtozo_szama, evszam, uj_nev){
  
  abra <- ggplot()
  for (i in 1:32){
    orszag <- lista[[i]]
    name_1 <- as.data.frame(orszag@data$NAME_1)
    adatbazis_1 <- left_join(name_1, adatbazis[ , c( 1, valtozo_szama)], 
                             by = c("orszag@data$NAME_1" = "GEO/TIME" ))
    orszag@data$id <- rownames(orszag@data)
    adatbazis_1 <- rename.variable(adatbazis_1, "orszag@data$NAME_1", "NAME_1")
    orszag@data <- join(orszag@data, adatbazis_1, by = "NAME_1")
    orszag@data <- rename.variable(orszag@data, evszam, uj_nev)
    orszag_df <- fortify(orszag)
    memory.limit(size = 56000000)
    orszag_df <- join(orszag_df, orszag@data, by="id")
    valtozo <- orszag_df[ ,18]
    pluszreteg <- geom_polygon(data = orszag_df, aes(x = long, y = lat, group = group, 
                                                     fill = arany), color = "black",  
                               size = 0.05)
    abra <- abra + pluszreteg
  }
  return(abra)
}


plot1 <- abrakeszito(adatbazisok[[1]], 5, "2018", "arany")
plot1  + xlab("Hosszúsági fokok") + ylab("Szélességi fokok") + theme_bw()

abrakeszito2 <- function(adatbazis, valtozo_szama, evszam, uj_nev){
  
  abra <- ggplot()
  for (i in 1:32){
    orszag <- lista[[i]]
    name_1 <- as.data.frame(orszag@data$NAME_1)
    adatbazis_1 <- left_join(name_1, adatbazis[ , c( 1, valtozo_szama)], 
                             by = c("orszag@data$NAME_1" = "GEO/TIME" ))
    orszag@data$id <- rownames(orszag@data)
    adatbazis_1 <- rename.variable(adatbazis_1, "orszag@data$NAME_1", "NAME_1")
    orszag@data <- join(orszag@data, adatbazis_1, by = "NAME_1")
    orszag@data <- rename.variable(orszag@data, evszam, uj_nev)
    orszag_df <- fortify(orszag)
    memory.limit(size = 56000000)
    orszag_df <- join(orszag_df, orszag@data, by="id")
    valtozo <- orszag_df[ ,18]
    pluszreteg <- geom_polygon(data = orszag_df, aes(x = long, y = lat, group = group, 
                                                     fill = atlag), color = "black",  
                               size = 0.05)
    abra <- abra + pluszreteg
  }
  return(abra)
}

plot2 <- abrakeszito2(adatbazisok[[2]], 5, "2018", "atlag")
plot2  + xlab("Hosszúsági fokok") + ylab("Szélességi fokok") + theme_bw()

abrakeszito3 <- function(adatbazis, valtozo_szama, evszam, uj_nev){
  
  abra <- ggplot()
  for (i in 1:32){
    orszag <- lista[[i]]
    name_1 <- as.data.frame(orszag@data$NAME_1)
    adatbazis_1 <- left_join(name_1, adatbazis[ , c( 1, valtozo_szama)], 
                             by = c("orszag@data$NAME_1" = "GEO/TIME" ))
    orszag@data$id <- rownames(orszag@data)
    adatbazis_1 <- rename.variable(adatbazis_1, "orszag@data$NAME_1", "NAME_1")
    orszag@data <- join(orszag@data, adatbazis_1, by = "NAME_1")
    orszag@data <- rename.variable(orszag@data, evszam, uj_nev)
    orszag_df <- fortify(orszag)
    memory.limit(size = 56000000)
    orszag_df <- join(orszag_df, orszag@data, by="id")
    valtozo <- orszag_df[ ,18]
    pluszreteg <- geom_polygon(data = orszag_df, aes(x = long, y = lat, group = group, 
                                                     fill = median), color = "black",  
                               size = 0.05)
    abra <- abra + pluszreteg
  }
  return(abra)
}

plot3 <- abrakeszito3(adatbazisok[[3]], 5, "2018", "median")
plot3  + xlab("Hosszúsági fokok") + ylab("Szélességi fokok") + theme_bw()

### Nézzük meg, hogy mely régiókra nincs megfigyelésünk

orszag <- lista[[1]]
name_1 <- as.data.frame(orszag@data$NAME_1)
adatbazis_1 <- left_join(name_1, adatbazisok[[1]][ , c( 1, 5)], 
                         by = c("orszag@data$NAME_1" = "GEO/TIME" ))
hianyzok_1 <- adatbazis_1[!complete.cases(adatbazis_1),]
megvan_1 <- adatbazis_1[complete.cases(adatbazis_1),]
adatbazis_2 <- adatbazis_1

hianyzok <- list()
megvan <- list()
for (i in 2:32){
  
  orszag <- lista[[i]]
  name_1 <- as.data.frame(orszag@data$NAME_1)
  adatbazis_1 <- left_join(name_1, adatbazisok[[1]][ , c( 1, 5)], 
                           by = c("orszag@data$NAME_1" = "GEO/TIME" ))
  hianyzok[[i]] <- adatbazis_1[!complete.cases(adatbazis_1),]
  megvan[[i]] <-  adatbazis_1[complete.cases(adatbazis_1),]
  hianyzok_1 <- rbind(hianyzok_1, hianyzok[[i]])
  megvan_1 <- rbind(megvan_1, megvan[[i]])
  adatbazis_2 <- rbind(adatbazis_2, adatbazis_1)
}

megvan_1
hianyzok_1

##### és minden évre elkészíteni az adatbázisokat

adatbazis_3 <- as.data.frame(adatbazis_2[,1])
adatbazis_4 <- list()

for (k in 2:5){
  
  for (j in 1:3){
    
    orszag <- lista[[1]]
    name_1 <- as.data.frame(orszag@data$NAME_1)
    adatbazis_1 <- left_join(name_1, adatbazisok[[j]][ , c( 1, k)], 
                             by = c("orszag@data$NAME_1" = "GEO/TIME" ))
    adatbazis_2 <- adatbazis_1
    
    for (i in 2:32){
      
      orszag <- lista[[i]]
      name_1 <- as.data.frame(orszag@data$NAME_1)
      adatbazis_1 <- left_join(name_1, adatbazisok[[j]][ , c( 1, k)], 
                               by = c("orszag@data$NAME_1" = "GEO/TIME" ))
      adatbazis_2 <- rbind(adatbazis_2, adatbazis_1)
    }
    adatbazis_3[,j+1] <-adatbazis_2[,2]
  }
  adatbazis_4[[k-1]] <- adatbazis_3
}

#### Mindenre

lista_osszeg <- lista[[1]]
for (i in c(2:32)){
  lista_osszeg <- rbind(lista_osszeg, lista[[i]])
}
lista_osszeg

##### Elemzéshez a szomszédsági mátrix

szomszedok <- poly2nb(st_as_sf(lista_osszeg))
szomszedok

lista_osszeg <- lista_osszeg[-c(111, 114, 125, 131, 132, 141, 184, 185, 191, 235, 279, 290, 368, 387),]
szomszedok <- poly2nb(st_as_sf(lista_osszeg))
szomszedok
szomszedlista <- nb2listw(szomszedok)

cents <- coordinates(lista_osszeg)
cents <- SpatialPointsDataFrame(coords = cents, data = lista_osszeg@data, 
                                proj4string = CRS("+proj=longlat +ellps=clrk66"))

plot(lista_osszeg, col = 'white', border = 'grey')
plot(szomszedok, cents, col = 'black', lwd = 2, add = TRUE)

###### Modellezéshez végleges adatbázis összeállítása

t <- 1
names(adatbazis_4[[t]]) <- c("regio, arany", "atlag", "median", "nepsuruseg", "lakossag_med", "ferfi_med", 
                             "no_med", "szuletesek")

# Figyelni rá, hogy az Adatbazis_1-en ne változtassunk!
write.csv(lista_osszeg, "Adatbazis.csv")
write.csv(as.data.frame(adatbazis_4[[t]]), "2018_ra.csv")

############# Modellezés

setwd("E:/Egyetem/Hetedik félév/MNB verseny")
elemzeshez_2015 <- readxl::read_xlsx("Adatbazis_1.xlsx")
elemzeshez_2016 <- readxl::read_xlsx("Adatbazis_2.xlsx")
elemzeshez_2017 <- readxl::read_xlsx("Adatbazis_3.xlsx")
elemzeshez_2018 <- readxl::read_xlsx("Adatbazis_4.xlsx")

### 
elemzeshez <- elemzeshez_2018 #### Itt kell megadni az adatbázist
elemzeshez <- cbind(elemzeshez$NAME_1, as.data.frame(lapply(elemzeshez[, c(12:19)], as.numeric)))

### elemzeshez <- mice::complete(elemzeshez)
# Leíró statisztikák
psych::describe(elemzeshez[ , c(2:4)])

# Térbeli autokorreláció keresése
# Közvetlenül
moran.test(elemzeshez$arany, szomszedlista, na.action = na.omit, zero.policy = T)
moran.mc(elemzeshez$arany, szomszedlista, nsim = 999,
         na.action = na.omit, zero.policy = T)

# Vagy egy OLS után
ols_model <- lm(arany ~ median + lakossag_med, elemzeshez, 
                na.action = na.omit)
summary(ols_model)
AIC(ols_model)
logLik(ols_model)

lm.LMtests(ols_model, szomszedlista, test = "all", zero.policy = T)
lm.morantest(ols_model, szomszedlista, alternative = "two.sided", zero.policy = T)


# Modellezés
library(spatialreg)
lag_model <- lagsarlm( arany ~  median + lakossag_med, elemzeshez, listw = szomszedlista,
                       na.action = na.omit, zero.policy = T)
summary(lag_model)
AIC(lag_model)
logLik(lag_model)
W <- as(szomszedlista, "CsparseMatrix")
trMC <- trW(W, type = "MC")
im <- impacts(lag_model, tr = trMC, R = 100, zstats = T)
summary(im, zstats = TRUE)


error_model <- errorsarlm(arany ~ median + lakossag_med, elemzeshez, listw = szomszedlista,
                          na.action = na.omit, zero.policy = T)
summary(error_model)
AIC(error_model)
logLik(error_model)
