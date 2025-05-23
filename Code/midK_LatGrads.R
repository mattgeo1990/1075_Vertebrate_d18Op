
# Contingencies -----------------------------------------------------------

library(tidyverse)
library(dplyr)

 # read in V1075_MCwater.csv (LOCAL)
  setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
  #V1075_MCwater <- read.csv("V1075_MCwater.csv")
  V1075_all <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")
  lit_data_BySpec <- read.csv("lit_data_BySpec.csv")
  V1075_BySpec_water <- read.csv("V1075_BySpec_water.csv")
  
  # subset fish scales
  Gar_scales <- subset(V1075_all, Element.type == "ganoid scale")
  # subset Croc G
  CrocG <- subset(V1075_BySpec_water, Taxon == "Neosuchian G")
  CrocG$paleolatitude <- 52
  # subset Aquatic Turtle
  Glyp <- subset(V1075_BySpec_water, Taxon == "Glyptops sp.")
  Glyp$paleolatitude <- 52
  
  # Subset the literature data for plotting along latitude
  Suarez_all <- subset(lit_data_BySpec, formation %in% c("Holly Fork", "Cedar Mountain", "Cloverly"))
  Suarez_croc <- subset(Suarez_all, eco_type %in% c("croc"))
  Suarez_Goni <- Suarez_croc <- subset(Suarez_all, taxon %in% c("Goniopholididae", "Goniopholidae"))
  
  #calculate water from Celina's crocs
  Suarez_Goni$d18Ow <-   0.82 * Suarez_Goni$d18O - 19.93
  
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
Cloverly_paleolats <- c(52) # See 4 model options at paleolatitude.org, based on modern lat/long of 45°N/-108°W
l <- Cloverly_paleolats
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
  points(V1075TempLat$lat, 24, pch = 1, lwd = 1, cex = 1)
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


# Temperature -------------------------------------------------------------


# Set Constant Parameters
# Paleolatitude of Cloverly Formation (Vaes et al., 2023)
Cloverly_Paleolat_mean <- 51.76046
Cloverly_paleolat_CI_upper <- 50.20277
Cloverly_paleolat_CI_lower <- 53.36299
  
# Create data frames for the main series and the confidence interval

# data to include
ALbian_LTG <- read.csv("/Users/allen/Documents/GitHub/CLC_paleoclimate/Data/PhanDA_LTG_Albian.csv")

# create latitude vector for modern gradient
lat <- seq(0,90,by=1)

# create data frame with modern gradient data
modern <- data.frame(
  lat = lat,
  temp = LatTempModern
)

# create objects for Judd et al 2024
lat <- ALbian_LTG$Latitude
LatTempNewUpper <- ALbian_LTG$LTG_95
LatTempNewLower <- ALbian_LTG$LTG_05
LatTempNewMean <- ALbian_LTG$LTG_50

# create Judd et al 2024 data frame
Judd24_Albian <- data.frame(
  lat = lat,
  temp_mean = LatTempNewMean,         # Central line of the new series
  temp_lower = LatTempNewLower,       # Lower bound of confidence interval
  temp_upper = LatTempNewUpper        # Upper bound of confidence interval
)

# Read V1075 data
V1075_MC_output_summary <- read.csv("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/DataV1075_MCoutput_summary.csv")
print(V1075_MC_output_summary)

# Create V1075 objects for plotting
V1075_MAWSAT_mean <- data.frame(lat = 52, temp = V1075_MC_output_summary[4,2]) # Replace with your actual coordinates
V1075_MAWSAT_upper <- V1075_MC_output_summary[4,4]
V1075_MAWSAT_lower <- V1075_MC_output_summary[4,3]

# Create Suarez et al. (2021) Ruby Ranch objects for plotting
# Ruby Ranch paleolat (Vaes et al., 2023 reconstruction based on lat: 38.81°N, long: 104.54°W of nearby Colorado Springs, CO)
RRlat <- 45.34
RRlat_upper <- 46.94
RRlat_lower <- 43.79
 
RRtemps <- c(38.6, 32.4, 31.3, 44.5, 19.8)
RRTempLat <- data.frame(cbind(temp = RRtemps, lat = RRlat, proxy = "D47 CO3"))
RRTempLat$lat <- as.numeric(RRTempLat$lat)
RRTempLat$temp <- as.numeric(RRTempLat$temp)

  
# Kate A.'s Antlers Fm data
# Trinity paleolat based on lat: 33.73°N, long: -97.16 of the I-35 bridge over the Red River, in vicinity of Kate's sampling sites (https://doi.org/10.1016/j.palaeo.2019.109491)
OKTXlat <- 38.72
OKTXlat_upper <- 40.3
OKTXlat_lower <- 37.2

# create new dataframe with V1075 temp datapoint and paleolat
OKTX_Temps <- c(31,31,27,26)
OKTXtemp_upper <- (as.numeric(OKTX_TempLat$temp)) + 3
OKTXtemp_lower <- (as.numeric(OKTX_TempLat$temp))-3
OKTX_TempLat <- data.frame(cbind(temp = OKTX_Temps, lat = OKTXlat, proxy = "D47 CO3"))
OKTX_TempLat$temp <- as.numeric(OKTX_TempLat$temp)
OKTX_TempLat$lat <- as.numeric(OKTX_TempLat$lat)

OKTX_TempLat$lat_CI_upper <- OKTXlat_upper
OKTX_TempLat$lat_CI_lower <- OKTXlat_lower

OKTX_TempLat$temp_CI_upper <- OKTXtemp_upper
OKTX_TempLat$temp_CI_lower <- OKTXtemp_lower


# Create the ggplot with the main series, confidence interval, and V1075 MAWSAT
ggplot() +
  # Add the main series
  geom_line(data = modern, aes(x = lat, y = temp), size = 1, color = "black", linetype = "dotted") +
  # Add the shaded confidence interval
  geom_ribbon(data = Judd24_Albian, aes(x = lat, ymin = temp_lower, ymax = temp_upper), 
              fill = "gray", alpha = 0.5) +
  # Add the central line of the new series
  geom_line(data = Judd24_Albian, aes(x = lat, y = temp_mean), size = 1, color = "black") +
  # Add V1075 mean simulated MAWSAT
  geom_point(data = V1075_MAWSAT_mean, aes(x = lat, y = temp), size = 2, color = "black") +
  # Add error bars for V1075 simulated MAWSAT
  geom_errorbar(data = V1075_MAWSAT_mean, aes(x = lat, ymin = V1075_MAWSAT_lower, ymax = V1075_MAWSAT_upper), 
                width = 0.5, color = "black") +
  geom_errorbar(data = V1075_MAWSAT_mean, aes(y = temp, xmin = Cloverly_paleolat_CI_lower, xmax = Cloverly_paleolat_CI_upper), 
                width = 0.5, color = "black") +
  # Add Suarez et al. (2021) Ruby Ranch temperatures
  geom_point(data =  RRTempLat, aes(x = lat, y = temp), size = 2, color = "black", shape = 1) +
  # Add latitude 95% CI error bars for Suarez
  geom_errorbar(data = RRTempLat, aes(y = temp, xmin = RRlat_lower, xmax = RRlat_upper), 
                width = 0.5, color = "black") +
  # Add Kate A.'s Antlers Fm temperatures
  geom_point(data =  OKTX_TempLat, aes(x = lat, y = temp), size = 2, color = "black", shape = 5) +
  # Add latitude 95% CI error bars for Antlers
  geom_errorbar(data = OKTX_TempLat, aes(y = temp, xmin = lat_CI_lower, xmax = lat_CI_upper), 
                width = 0.5, color = "black") +
  # Set axis limits
  coord_cartesian(xlim = c(35, 55), ylim = c(0, 50)) +
  # Add axis labels
  labs(
    x = expression(paste("Latitude (", degree, "N)")),
    y = expression(paste("T (", degree, "C)"))
  ) +
  # Apply minimal theme
  theme_minimal() +
  # Customize axis text and title sizes
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

n <- length(temperature_simulations)
se <- 8.750734 / sqrt(n)

# Water Isotopes ----------------------------------------------------------

# Aptian-Albian turtles (Suarez et al., 2020)

    setwd("/Users/allen/Documents/Data Analysis/Data/Geochem")
    CelinaTurtles <- read.csv("SuarezEtAl2020_AptianAlbianTurtleDATA.csv")
    str(CelinaTurtles)
    
    c <- CelinaTurtles[which(CelinaTurtles$Taxon %in% c("Glyptops sp?", "Glyptops", "Trionychoidea", "Trionyclidae")),]
    
    table(c$Member)
    table(CelinaTurtles$Member)
    
    CelinaTurtles[which(CelinaTurtles$Formation %in% c("Holly Creek", "Cedar Mountain")),]
    
    Suarez_turtles <- CelinaTurtles[which(CelinaTurtles$Taxon %in% c("Glyptops sp?", "Glyptops", "Trionychoidea", "Trionyclidae")),]
    

    table(CelinaTurtles$Formation)
    table(CelinaTurtles$Member)
    
    RRCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("Ruby Ranch")), ]
    LSCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("Little Sheep")), ]
    HFCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("")), ]
    HFCelinaTurts$Taxon
    LSCelinaTurts$Taxon
    LSCelina_Glyp <- subset(LSCelinaTurts, Taxon == "Glyptops")
    
# updated paleolat model
    # Cedar Mountain
    RRlat <- 46.2 # based on 39.2, -105 @ 105 Ma (Vaes et al., 2023)
    Suarez_Goni$paleolat[which(Suarez_Goni$formation %in% "Cedar Mountain")] <- RRlat
    RRCelinaTurts$Palaeolatitude <- RRlat
    
    # Holly Fork 
    HFlat <- 38.2 
    HFCelinaTurts$Palaeolatitude <- HFlat
    
    # Cloverly 
    CFlat <- 52
    LSCelina_Glyp$Palaeolatitude <- CFlat
    V1075_BySpec_water$paleolatitude <- CFlat
    
    


# Latitude vs Meteoric water d180 (see Table 1, Suarez et al. 2011)
LatMeteoric_Modern <- -0.003* (lat^2) + 0.0595* lat - 3.699 #Rozanski et al., 1993
LatMeteoric_CoolK <- -0.005 * (lat^2) + 0.1137 * lat - 5.270 #Barron (1983)
LatMeteoric_WarmK <- -0.005 * (lat^2) + 0.1299 * lat - 4.901 #Barron (1983)
LatMeteoric_GENESIS_MOM <- -0.005* (lat^2) +0.0730* lat - 4.001 #Zhou et al. (2008)

#LatPedocarb <- -0.0042* (lat^2) + 0.08* lat - 4.33 #Suarez et al. 2011
# Latitude vs Meteoric water d180 (see Table 1, Suarez et al. 2011)


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
     ylim = c(-16, 0),
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
  points(LSCelina_Glyp$Palaeolatitude, LSCelina_Glyp$d18Ow, pch = 3, col="black", lwd = 1, cex = 1)
  #points(HFCelinaTurts$Palaeolatitude, HFCelinaTurts$d18Ow, pch = 3, col="black", lwd = 1, cex = 1)
  #points(RRCelinaTurts$Palaeolatitude, RRCelinaTurts$d18Ow, pch = 3, col="black", lwd = 1, cex = 1)
  points(Suarez_Goni$paleolat, Suarez_Goni$d18Ow,)
  points(Glyp$paleolatitude, Glyp$d18Owater, pch = 3, col="blue", lwd = 1, cex = 1)
  points(CrocG$paleolatitude, CrocG$d18Owater, pch = 1, col="blue", lwd = 1, cex = 1)
  #points(CFlat, V1075_BySpec_water$d18Owater[which(V1075_BySpec_water$Taxon == "Neosuchian B")], pch = 10, col="black", lwd = 1, cex = 1)
  #points(CFlat, V1075_BySpec_water$d18Owater[which(V1075_BySpec_water$Taxon == "Neosuchian G")], pch = 9, col="black", lwd = 1, cex = 1)
  #points(CFlat, V1075_BySpec_water$d18Owater[which(V1075_BySpec_water$Taxon == "Naomichelys sp.")], pch = 8, col="black", lwd = 1, cex = 1)

# Export using tiff() and dev.off
  
  setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Exports")
  
  tiff("d18Ow_LatGrad.tiff", units="in", width= 4, height= 4, res=500)
  
  ggsave("d18Ow_LatGrad.tiff", units="in", width= 4, height= 4, dpi=500, compression = 'lzw')
  
  dev.off()
  
  
  
  
  
  
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


# Celina's data ---------------------------------------------------------

litdata_d18Op_BySpec <- read.csv("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/lit_data_BySpec.csv")

colnames(litdata_d18Op_BySpec)
unique(litdata_d18Op_BySpec$formation)

Suarez_d18Op_BySpec <- subset(litdata_d18Op_BySpec, formation %in% c("Holly Creek", "Cedar Mountain", "Cloverly"))

unique(Suarez_d18Op_BySpec$paleolat)
 # assign updated paleolats
Suarez_d18Op_BySpec$paleolat[Suarez_d18Op_BySpec$formation == "Holly Creek"] <- 38.2
Suarez_d18Op_BySpec$paleolat[Suarez_d18Op_BySpec$formation == "Cloverly"] <- 52
Suarez_d18Op_BySpec$paleolat[Suarez_d18Op_BySpec$formation == "Cedar Mountain"] <- 46

unique(Suarez_d18Op_BySpec$eco_type)
# filter out the taxa you want
Suarez_d18Op_BySpec <- subset(Suarez_d18Op_BySpec, eco_type %in% c("croc", "turtle indet.", "aquatic turtle", "terrestrial turtle"))


# New d18Ow ---------------------------------------------------------------

# Add CLC clumped d18Owater data
CLC_D47_d18Owater_results <- read.csv("/Users/allen/Documents/GitHub/CLC_paleoclimate/Data/CLC_D47_d18Owater_results.csv")
CLC_D47_d18Owater_results$Paleolatitude <- 52

# create data frame with modern d18Ow~lat gradient
d18Ow_lat_modern <- data.frame(
  lat = lat,              # Replace 'lat' with your actual latitude vector
  d18Ow = (-0.003* (lat^2) + 0.0595* lat - 3.699)   # Replace 'LatTempModern' with your actual data vector
)

# create data frame with Suarez coolK d18Ow~lat gradient
d18Ow_lat_coolk <- data.frame(
  lat = lat,              
  d18Ow = (-0.005 * (lat^2) + 0.1137 * lat - 5.270)   
)

# create data frame with Suarez warmK d18Ow~lat gradient
d18Ow_lat_warmk <- data.frame(
  lat = lat,              
  d18Ow = (-0.005 * (lat^2) + 0.1299 * lat - 4.901)   
)

# create data frame with Suarez warmK d18Ow~lat gradient
d18Ow_lat_GENMOM <- data.frame(
  lat = lat,            
  d18Ow = (-0.005* (lat^2) +0.0730* lat - 4.001)
)


# add V1075 d18Ow data
V1075_MC_output_summary <- read.csv("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/DataV1075_MCoutput_summary.csv")
print(V1075_MC_output_summary)
V1075_dual_d18Osw <- V1075_MC_output_summary[3,1:4]
V1075_dual_d18Osw$lat <- Cloverly_Paleolat_mean
V1075_dual_d18Osw$lat_upper <- Cloverly_paleolat_CI_upper
V1075_dual_d18Osw$lat_lower <- Cloverly_paleolat_CI_lower

str(V1075_dual_d18Osw)

# Create ggplot for d18Ow~lat gradients
ggplot(data = d18Ow_lat_modern, aes(x = lat, y = d18Ow)) +
  geom_line(size = 1, color = "black", linetype = 3) + # Line style and thickness
  # CoolK
  geom_line(data = d18Ow_lat_coolk, aes(x = lat, y = d18Ow), color = "#0072B2", size = 1) +
  # WarmK
  geom_line(data = d18Ow_lat_warmk, aes(x = lat, y = d18Ow), color = "#D55E00", size = 1) +
  # GENMOM 
  geom_line(data = d18Ow_lat_GENMOM, aes(x = lat, y = d18Ow), color = "#009E73", size = 1) +
  # add V1075 data
  geom_point(data = V1075_dual_d18Osw, aes(x = lat, y = Mean), size = 2, color = "black", shape = 5) +
  # Add latitude 95% CI error bars for V1075
  geom_errorbarh(data = V1075_dual_d18Osw, aes(y = Mean, xmin = lat_lower, xmax = lat_upper), 
                  height = 0.5, color = "black") +
  # Add Celina's data
  geom_point(data = CelinaTurtles, aes(x = Palaeolatitude, y = d18Ow), size = 2, color = "black", shape = 5) +
  # Add CLC cumped d18Owater data
  geom_point(data = CLC_D47_d18Owater_results, aes(x = Paleolatitude, y =   d18Owater_VSMOW), size = 2, color = "red", shape = 2) +
  # Set axis limits
  coord_cartesian(xlim = c(35, 55), ylim = c(-20, 0)) +
  # Add axis labels
  labs(
    x = expression(paste("Latitude (", degree, "N)")),
    y = expression("δ"^18 * "O"[sw] * "(‰ V-SMOW)")
  ) +
  # Apply minimal theme
  theme_minimal() +
  # Customize axis text and title sizes
  theme(
    panel.grid = element_blank(),                      # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA), # Add border
    axis.ticks = element_line(color = "black"),        # Keep axis tick marks
    axis.text = element_text(size = 12),               # Customize axis text size
    axis.title = element_text(size = 14)               # Customize axis title size
  )


# ggplot junkyard


# Add V1075 mean simulated MAWSAT
  geom_point(data = V1075_MAWSAT_mean, aes(x = lat, y = temp), size = 2, color = "black") +
  # Add error bars for V1075 simulated MAWSAT
  geom_errorbar(data = V1075_MAWSAT_mean, aes(x = lat, ymin = V1075_MAWSAT_lower, ymax = V1075_MAWSAT_upper), 
                width = 0.5, color = "black") +
  geom_errorbar(data = V1075_MAWSAT_mean, aes(y = temp, xmin = Cloverly_paleolat_CI_lower, xmax = Cloverly_paleolat_CI_upper), 
                width = 0.5, color = "black") +
  # Add Suarez et al. (2021) Ruby Ranch temperatures
  geom_point(data =  RRTempLat, aes(x = lat, y = temp), size = 2, color = "black", shape = 1) +
  # Add latitude 95% CI error bars for Suarez
  geom_errorbar(data = RRTempLat, aes(y = temp, xmin = RRlat_lower, xmax = RRlat_upper), 
                width = 0.5, color = "black") +
  # Add Kate A.'s Antlers Fm temperatures
  geom_point(data =  OKTX_TempLat, aes(x = lat, y = temp), size = 2, color = "black", shape = 5) +
  # Add latitude 95% CI error bars for Antlers
  geom_errorbar(data = OKTX_TempLat, aes(y = temp, xmin = lat_CI_lower, xmax = lat_CI_upper), 
                width = 0.5, color = "black") +
  

# Plot all d18Omw. This might not actually be useful --------

# run turtle water on all Glyptops values
Glyp$d18Owater <- 1.01 *(Glyp$d18O..VSMOW.) - 22.3 #Barrick et al. (1999)

# add paleolatitude
Glyp$paleolat <- 52

# test your code
plot(Glyp$d18O..VSMOW., Glyp$d18Owater)

# Look OK? Proceed with plotting
par(mar = c(5,5,1,1))

# plot d18Omw vs latitude with models overlain
plot(lat, 
     LatMeteoric_Modern,
     type = "l",
     las = 1,
     lwd = 2,
     xlim = c(30, 60),
     ylim = c(-15, -2),
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
points(Glyp$d18Owater, Glyp$paleolat, pch = 4, col="black", lwd = 1, cex = 10)

