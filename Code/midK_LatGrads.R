
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
        RRtemps <- c(38.6, 32.4, 31.3, 44.5, 19.8)
        RRlat <- c(36.9, 36.9,36.9,36.9,36.9)
        RRtempLat <- data.frame(RRtemps, RRlat)
        
      #Kate's Antlers Data
        AntPlat <- c(32, 32, 32, 32)
        AntPhyTemps <- c(31,31,27,26)
        AntlersPhyloTemps <- data.frame(AntPlat, AntPhyTemps)
        AntlersPhyloTemps <- AntlersPhyloTemps %>% 
          rename(lat = AntPlat,temp = AntPhyTemps)
        
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

#GGPLOT
      

ggplot(df, aes(lat,temp, group = ModelID, color = ModelID)) +
  geom_line(size =1)

# Plot with line patterns and border
ggplot(df, aes(lat, temp, group = ModelID, linetype = ModelID)) +
  geom_line(size = 1) +
  labs(x = "Latitude (°N)", y = "MAT (°C)") +  # Label x and y axes
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) 

# SANDBOX

# Define the point and its confidence intervals
point_data <- data.frame(
  lat = 40,
  temp = 22.5,
  lower = 22.5 - 5,
  upper = 22.5 + 5
)

# Plot with line patterns and border
gg_line <- ggplot(df, aes(lat, temp, group = ModelID, linetype = ModelID)) +
  geom_line(size = 1) +
  labs(x = "Latitude (°N)", y = "MAT (°C)") +  # Label x and y axes
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) 

gg_modgrad <- ggplot(data.frame(lat = lat, temp = LatTempModern), aes(lat, temp)) +
  geom_line(size = 1) +
  labs(x = "Latitude (°N)", y = "MAT (°C)") +  # Label x and y axes
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

gg_genmom <- geom_line(data = data.frame(lat = lat, temp = LatTemp_GENESIS_MOM), aes(lat, temp), size = 1) +


gg_modgrad + gg_genmom

gg_1075temp

gg_line + geom_point(data = point_data, aes(x = lat, y = temp), color = "black", size = 3) +


ggplot(df, aes(lat, temp, group = ModelID, linetype = ModelID)) +
  geom_line(size = 1) +
  geom_point(data = point_data, aes(x = lat, y = temp), color = "black", size = 3) +
  #geom_errorbar(data = point_data, aes(x = lat, ymin = lower, ymax = upper), width = 0.1) +
  labs(x = "Latitude (°N)", y = "MAT (°C)") +  # Label x and y axes
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )


# END SANDBOX


df$ModelID <- as.factor(df$ModelID)
levels(df$ModelID)
df$ModelID <- factor(df$ModelID, levels = c("LatTempModern", "LatTemp_CoolK", "LatTemp_WarmK"))


fishTempLat <- data.frame(TempFishTurt, 45, 0)
fishTempLat$X0 <- c("gar")
fishTempLat <- fishTempLat %>% 
  rename(
    lat = X45,
    temp = TempFishTurt,
    ModelID = X0
  )


# Base R Plot

garV1075 <- fishTempLat$temp

#y1 <- LatTempModern$temp

par(mar = c(5,5,1,10))

plot(lat, LatTempModern, 
     las = 1, type = "l", 
     lwd = 1, 
     ylim = c(-10, 40), 
     xlim = c(30,50), 
     xlab = substitute(paste("Latitude (°N)")), 
     ylab=substitute(paste("T (°C)")), 
     font = 1, 
     cex.lab = 1, 
     cex.axis = 1) 

#box(lwd =3)
legend("topright",
       inset=c(-0.75, 0),
       xpd = "TRUE",
       box.lwd = 0,
       legend = c("Modern Gradient", "GENESIS-MOM"), 
       lty = c(1,2,3,5), lwd = 2,
       bg ="transparent")
legend("bottomright", 
       inset=c(-0.75, 0),
       xpd = "TRUE",
       legend = c("Fossil Phosphate 
(This Study)", "Phyllosilicates", "D47 CO3"), 
       pch = 1:2:3,
       box.lwd = 0,
       pt.cex = 1.5)
#lines(LatTemp_CoolK, lty = 2, lwd = 2 )
#lines(LatTemp_WarmK, lty = 3, lwd = 2)
lines(LatTemp_GENESIS_MOM, lty = 5, lwd = 2)
points(fishTempLat$lat, fishTempLat$temp, pch = 1, lwd = 2, cex = 1.5)
points(AntlersPhyloTemps$lat, AntlersPhyloTemps$temp, pch =2, lwd = 2, cex = 1.5)
points(RRtempLat$RRlat, RRtempLat$RRtemps, pch =3, lwd = 2, cex = 1.5)


# Water Isotopes ----------------------------------------------------------

# Aptian-Albian turtles (Suarez et al., 2020)

    setwd("/Users/allen/Documents/Data Analysis/Data/Geochem")
    CelinaTurtles <- read.csv("SuarezEtAl2020_AptianAlbianTurtleDATA.csv")
    
    table(CelinaTurtles$Formation)
    table(CelinaTurtles$Member)
    
    RRCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("Ruby Ranch")), ]
    LSCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("Little Sheep")), ]
    HFCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("")), ]



# Latitude vs Meteoric water d180 (see Table 1, Suarez et al. 2011)
LatMeteoric_Modern <- -0.003* (lat^2) + 0.0595* lat - 3.699 #Rozanski et al., 1993
LatMeteoric_CoolK <- -0.005 * (lat^2) + 0.1137 * lat - 5.270 #Barron (1983)
LatMeteoric_WarmK <- -0.005 * (lat^2) + 0.1299 * lat - 4.901 #Barron (1983)
LatMeteoric_GENESIS_MOM <- -0.005* (lat^2) +0.0730* lat - 4.001 #Zhou et al. (2008)




# create delta notation for label
oxydeltwater <- expression(paste(delta^{18}, "O"[water], " (‰ V-SMOW)"))


par(mar = c(5,5,1,10))

# plot d18Omw vs latitude with models overlain
plot(lat, 
     LatMeteoric_Modern, 
     type = "l",
     las = 1,
     lwd = 2,
     xlim = c(30, 50),
     ylim = c(-10, -4),
     xlab = substitute(paste("Latitude (°N)")),
     ylab=(oxydeltwater),
     font = 1,
     cex.lab = 1,
     cex.axis = 1) 

#box(lwd =3)
legend("bottomright",
       inset=c(-0.5, 0),
       box.lwd = 0,
       bg = "transparent",
       legend = c("Modern", "GEN-MOM"), 
       lty = c(1,3),
       xpd = TRUE,
       lwd = 2 )


legend("topright",
       inset=c(-0.5, 0),
       xpd = TRUE,
       box.lwd = 0,
       bg = "transparent",
       legend = c("Glyptops", "'Croc A'", "'Croc B'", "'Croc G'", "Naomichelys"), 
       pch = c(7, 12, 10, 9, 8),
       pt.cex = 1.5)
#lines(LatMeteoric_CoolK, lty = 2, lwd = 2 )
#lines(LatMeteoric_WarmK, lty = 3, lwd = 2)
lines(LatMeteoric_GENESIS_MOM, lty = 3, lwd = 2)
#points(median(LSCelinaTurts$Palaeolatitude), median(LSCelinaTurts$d18Ow), pch = 4, col="black", lwd = 1, cex = 1)
#points(median(HFCelinaTurts$Palaeolatitude), median(HFCelinaTurts$d18Ow), pch = 4, col="black", lwd = 1, cex = 1)
#points(median(RRCelinaTurts$Palaeolatitude), median(RRCelinaTurts$d18Ow), pch = 4, col="black", lwd = 1, cex = 1)
points(c(40), V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Aquatic Turtle")], pch = 7, col="black", lwd = 1, cex = 1)
points(40, V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Croc A")], pch = 12, col="black", lwd = 1, cex = 1)
points(40, V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Croc B")], pch = 10, col="black", lwd = 1, cex = 1)
points(40, V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Croc G")], pch = 9, col="black", lwd = 1, cex = 1)
points(40, V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Terrestrial Turtle")], pch = 8, col="black", lwd = 1, cex = 1)


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
