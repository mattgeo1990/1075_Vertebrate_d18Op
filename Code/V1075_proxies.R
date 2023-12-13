

# Install/load required packages ------------------------------------------

  # Read in cleaned data (include link to script that cleans the data?)
    # Call V1075_cl from GitHub 
    # cleaning script is here: https://github.com/mattgeo1990/1075_Vertebrate_d18Op/blob/main/Code/V1075_d18O.R
      setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
      V1075_cl <- read.csv("V1075_cl.csv")
    # Source NIST data
      # READ IN NIST120c STD d18Op VALUES FROM RUN 1 and RUN 2
      # NEED NIST120C STD d18Op from Run 3!!!!!
        standards_githubURL <-"https://raw.githubusercontent.com/mattgeo1990/1075_Vertebrate_d18Op/main/Data/V1075_NIST120c.csv"
        NIST120c <- read_csv(standards_githubURL)
      # gather stats
        NIST120c_mean <- mean(NIST120c$d.18O.16O)

  # subset fish scales
    Gar <- subset(V1075_cl, Taxon == "Lepisosteidae")
    GarScales <- subset(V1075_cl, Element.type = "ganoid scale")
  # subset Croc G
    CrocG <- subset(V1075_cl, Taxon == "Goniopholidae")
  # subset Aquatic Turtle
    AquaTurt <- subset(V1075_cl, Taxon == "Glyptops sp.")


# Gather Statistics
  
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

