
# READ IN NIST120c STD d18Op VALUES FROM RUN 1 and RUN 2

# NEED NIST120C STD d18Op from Run 3!!!!!
  #ultimately want this to source from GitHub
    setwd("/Users/allen/Documents/Data Analysis/Data/Geochem")
    NIST120c <- read.csv("V1075_NIST120c_Run1&2.csv")

    NIST120c_mean <- mean(NIST120c$d.18O.16O)


# Gather Statistics
  
  AquaCroc <- subset(grouped_data, eco_type == "Croc G")
  AquaCroc_d18Op_mean <- mean(AquaCroc$d18O)
  AquaCroc_d18Op_SD <- sd(AquaCroc$d18O)
  
  AquaTurt <- subset(grouped_data, eco_type == "Aquatic Turtle")
  AquaTurt_d18Op_mean <- mean(AquaTurt$d18O)
  AquaTurt_d18Op_SD <- sd(AquaTurt$d18O)
  
  GarScales <- subset(grouped_data, eco_type == "Fish" & Element.type == "ganoid scale")
  GarScales_d18Op_mean <- mean(GarScales$d18O)
  GarScales_d18Op_SD <- sd(GarScales$d18O)

# Compute d18Omw and water temperatures

crocwater <- 0.82*(AquaCroc_d18Op_mean) - 19.93 #Amiot et al.(2007)
turtwater <- 1.01 *(AquaTurt_d18Op_mean) - 22.3 #Barrick et al. (1999)
meanCrocFish_temp <- 118.7 - 4.22*((GarScales_d18Op_mean  +(22.6 - NIST120c_mean)) - crocwater ) #Puceat et al. (2010)

