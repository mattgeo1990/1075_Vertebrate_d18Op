
# Contingencies -----------------------------------------------------------

library(tidyverse)
library(dplyr)

 # read in V1075_MCwater.csv (LOCAL)
  setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
  V1075_MCwater <- read.csv("V1075_MCwater.csv")

# MAT ---------------------------------------------------------------------

  # Latitude vs MAT (see Table 1, Suarez et al. 2011)

    # Leaf physiognomy Wolfe and Upchurch, 1987 <- - 0.0006 * lat2−0.2025* lat+30.25 
    #LeafPhysCret <- -0.004* lat2+0.0796* lat−4.329 # Leaf physiognomy Wolfe and Upchurch, 1987; Spicer and Corfield, 1992
    
    # Empirical Data
      # Temps from V1075 dual-taxon results
        TempFishTurt <- 22.2 
        TemplowCI <- 22.2 - 5.5
        TemphighCI <- 22.2 + 5.4
        
      # V1075 d18O water data
        # Glyptops
          turtle_1075 <- -8.4
          turtle_1075_loCI <- turtle_1075 - 1.1
          turtle_1075_hiCI <- turtle_1075 +1.1
        # Croc G 
          # CrocG_1075 <- 
          # CrocG_1075_loCI <-
          # CrocG_1075_hiCI <-
          
      # Suarez et al. 2021 Ruby Ranch Data
          
        # Ruby Ranch paleolats
          RRR_paleolats <- c(53-6, 47-6, 51-6, 50-6) # See 4 model options at paleolatitude.org, based on 6° difference from modern Cloverly lat/long of 45°N/-108°W
          RRlat <- mean(RRR_paleolats)
          
          # create new dataframe with V1075 temp datapoint and paleolat
          RRtemps <- c(38.6, 32.4, 31.3, 44.5, 19.8)
          RRTempLat <- data.frame(cbind(temp = RRtemps, lat = RRlat, proxy = "D47 CO3"))
          

        
      #Kate's Antlers Data
          # Trinity paleolats
          OKTX_paleolats <- c(53-8, 47-8, 51-8, 50-8) # See 4 model options at paleolatitude.org, based on 6° difference from modern Cloverly lat/long of 45°N/-108°W
          OKTXlat <- mean(OKTX_paleolats)

        # create new dataframe with V1075 temp datapoint and paleolat
        OKTX_Temps <- c(31,31,27,26)
        OKTX_TempLat <- data.frame(cbind(temp = OKTX_Temps, lat = OKTXlat, proxy = "D47 CO3"))
        
    # Models
      lat <- seq(0,90,by=1)
      LatTempModern <- -0.003* (lat^2) - 0.2514 * lat+30.113 #Rozanski et al., 1993
      LatTemp_CoolK  <- -0.004 * (lat^2) - 0.0586 * lat + 25.662 #Barron (1983)
      LatTemp_WarmK <- -0.003 * (lat^2) + 0.0538 * lat + 28.534 #Barron (1983)
      LatTemp_GENESIS_MOM <- -0.003 * (lat^2) - 0.0321 * lat + 32.514 #Zhou et al. (2008) 
      
      LatGrads <- data.frame(lat, LatTempModern, LatTemp_GENESIS_MOM)
      head(LatGrads)


      df <- gather(LatGrads, key = ModelID, value = temp, 
             c("LatTempModern", "LatTemp_GENESIS_MOM"))


df$ModelID <- as.factor(df$ModelID)
levels(df$ModelID)
df$ModelID <- factor(df$ModelID, levels = c("LatTempModern", "LatTemp_CoolK", "LatTemp_WarmK"))

# create new dataframe with V1075 temp datapoint and paleolat
Cloverly_paleolats <- c(53, 47, 51, 50) # See 4 model options at paleolatitude.org, based on modern lat/long of 45°N/-108°W
l <- mean(Cloverly_paleolats)
l <- 45
V1075TempLat <- data.frame(cbind(temp = c(TempFishTurt), lat = l, ModelID = "gar"))


# Base R Plot

garV1075 <- V1075TempLat$temp
par(mar = c(5,5,1,1))

plot(lat, LatTempModern, 
     las = 1, type = "l", 
     lwd = 1, 
     ylim = c(-10, 40), 
     xlim = c(30,60), 
     xlab = substitute(paste("Latitude (°N)")), 
     ylab=substitute(paste("T (°C)")), 
     font = 1, 
     cex.lab = 1, 
     cex.axis = 1) 

# Add Cretaceous models
  lines(LatTemp_CoolK, lty = 2, lwd = 1)
  lines(LatTemp_WarmK, lty = 3, lwd = 1)
  lines(LatTemp_GENESIS_MOM, lty = 5, lwd = 1)

LatTemp_CoolK[52]
LatTemp_WarmK[52]
# Add model shade band
  # Identify the x and y coordinates for "Cool K" and "GENESIS-MOM"
    x_coords <- c(lat, rev(lat))
    y_coords_coolk <- c(LatTemp_CoolK, rev(LatTemp_WarmK))
    
  # Shade in the area between "Cool K" and "GENESIS-MOM"
    polygon(x_coords, y_coords_coolk, col = "gray", border = NA)

# Add points
  points(V1075TempLat$lat, V1075TempLat$temp, pch = 1, lwd = 1, cex = 1)
  points(OKTX_TempLat$lat, OKTX_TempLat$temp, pch =2, lwd = 1, cex = 1)
  points(RRTempLat$lat, RRTempLat$temp, pch =3, lwd = 1, cex = 1)

# add legends (if you want)
  #box(lwd =3)
  #legend("topright",
  inset=c(-1.25, 0),
  xpd = "TRUE",
  box.lwd = 0,
  legend = c("Modern Gradient", "GENESIS-MOM", "Cool K", "Warm K"), 
  lty = c(1,2,3,5), lwd = 2,
  bg ="transparent")

legend("bottom", 
inset=c(0, 0),
#xpd = "TRUE",
legend = c("Vertebrate Phosphates (this study)", "Phyllosilicates", "D47 CO3"), 
pch = c(1,2,3),
box.lwd = 0,
pt.cex = 1,
cex = 0.75)

# Water Isotopes ----------------------------------------------------------

# Aptian-Albian turtles (Suarez et al., 2020)

    setwd("/Users/allen/Documents/Data Analysis/Data/Geochem")
    CelinaTurtles <- read.csv("SuarezEtAl2020_AptianAlbianTurtleDATA.csv")
    
    table(CelinaTurtles$Formation)
    table(CelinaTurtles$Member)
    
    RRCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("Ruby Ranch")), ]
    LSCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("Little Sheep")), ]
    HFCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("")), ]

# updated paleolat model
    RRCelinaTurts$Palaeolatitude <- RRlat
    
    # Holly Fork 
    HFlat <- mean(c(53-10.5, 47-10.5, 51-10.5, 50-10.5))
    HFCelinaTurts$Palaeolatitude <- HFlat
    
    # Cloverly 
    LSCelinaTurts$Palaeolatitude <- l


# Latitude vs Meteoric water d180 (see Table 1, Suarez et al. 2011)
LatMeteoric_Modern <- -0.003* (lat^2) + 0.0595* lat - 3.699 #Rozanski et al., 1993
LatMeteoric_CoolK <- -0.005 * (lat^2) + 0.1137 * lat - 5.270 #Barron (1983)
LatMeteoric_WarmK <- -0.005 * (lat^2) + 0.1299 * lat - 4.901 #Barron (1983)
LatMeteoric_GENESIS_MOM <- -0.005* (lat^2) +0.0730* lat - 4.001 #Zhou et al. (2008)

LatPedocarb <- -0.0042* (lat^2) + 0.08* lat - 4.33 #Suarez et al. 2011



# create delta notation for label
oxydeltwater <- expression(paste(delta^{18}, "O"[water], " (‰ V-SMOW)"))


par(mar = c(5,5,1,1))

# plot d18Omw vs latitude with models overlain
plot(lat, 
     LatMeteoric_Modern,
     type = "l",
     las = 1,
     lwd = 2,
     xlim = c(30, 60),
     ylim = c(-10, -2),
     xlab = substitute(paste("Latitude (°N)")),
     ylab = expression(delta^18*"O"[precipitation]*" (‰ V-SMOW)"),
     font = 1,
     cex.lab = 1,
     cex.axis = 1) 

# Add Cretaceous Models
lines(LatMeteoric_CoolK, lty = 2, lwd = 0)
lines(LatMeteoric_WarmK, lty = 3, lwd = 0)
# lines(LatMeteoric_GENESIS_MOM, lty = 3, lwd = 1)
#lines(LatPedocarb, lty = 4, lwd = 2)

# Add model shade band
# Identify the x and y coordinates for "LatMeteoric_CoolK" and "LatMeteoric_GENESIS-MOM"
x_coords <- c(lat, rev(lat))
y_coords_warmk <- c(LatMeteoric_WarmK, rev(LatMeteoric_CoolK))

# Shade in the area between "LatMeteoric_WarmK" and "LatMeteoric_GENESIS-MOM"
polygon(x_coords, y_coords_warmk, col = "gray", border = NA)

# Add points
  points(median(LSCelinaTurts$Palaeolatitude), median(LSCelinaTurts$d18Ow), pch = 4, col="black", lwd = 1, cex = 1)
  points(median(HFCelinaTurts$Palaeolatitude), median(HFCelinaTurts$d18Ow), pch = 4, col="black", lwd = 1, cex = 1)
  points(median(RRCelinaTurts$Palaeolatitude), median(RRCelinaTurts$d18Ow), pch = 4, col="black", lwd = 1, cex = 1)
  points(c(50.25), V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Glyptops sp.")], pch = 7, col="black", lwd = 1, cex = 1)
  points(50.25, V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Neosuchian A")], pch = 12, col="black", lwd = 1, cex = 1)
  points(50.25, V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Neosuchian B")], pch = 10, col="black", lwd = 1, cex = 1)
  points(50.25, V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Neosuchian G")], pch = 9, col="black", lwd = 1, cex = 1)
  points(50.25, V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Naomichelys sp.")], pch = 8, col="black", lwd = 1, cex = 1)

# Add Legends (if you want)
#box(lwd =3)
#legend("bottomright",
inset=c(-0.5, 0),
box.lwd = 0,
bg = "transparent",
legend = c("Modern", "GEN-MOM", "Siderites"), 
lty = c(1,3,4),
xpd = TRUE,
lwd = 2 )


legend("topright",
inset=c(0, 0),
xpd = TRUE,
box.lwd = 0,
bg = "transparent",
legend = c("Glyptops sp.", "Neosuchian A", "Neosuchian B", "Neosuchian G", "Naomichelys sp.", "Turtles"), 
pch = c(7, 12, 10, 9, 8, 4),
pt.cex = 1,
cex = 0.75)

# Code junkyard below

#points(RRCelinaTurts$Palaeolatitude, RRCelinaTurts$d18Ow, pch = 1, lwd = 2, cex = 1.5)
#points(HFCelinaTurts$Palaeolatitude, HFCelinaTurts$d18Ow, pch =2, lwd = 2, cex = 1.5)

# dev.off()

#TempModels <- c(LatTempModern, LatTemp_CoolK, LatTemp_WarmK, LatTemp_GENESIS_MOM)
#df <- data.frame(lat,TempModels, ModelID)
#plot(LatTempModern, ylim = c(-20, 40), xlim = c(0,90))
#points(40,fishTemp, col = "orange")
#points(40, TempFishCroc, col = "dark green")
#points(37, 31)

#points(37, 27)

