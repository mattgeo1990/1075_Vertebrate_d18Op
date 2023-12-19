

# Install/load required packages ------------------------------------------

  # Read in cleaned data (include link to script that cleans the data?)
    # Call V1075_cl from GitHub 
    # cleaning script is here: https://github.com/mattgeo1990/1075_Vertebrate_d18Op/blob/main/Code/V1075_d18O.R
      setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
      V1075_BySpec <- read.csv("V1075_BySpec.csv")
    # Source NIST data
      # READ IN NIST120c STD d18Op VALUES FROM RUN 1 and RUN 2
      # NEED NIST120C STD d18Op from Run 3!!!!!
        standards_githubURL <-"https://raw.githubusercontent.com/mattgeo1990/1075_Vertebrate_d18Op/main/Data/V1075_NIST120c.csv"
        NIST120c <- read_csv(standards_githubURL)
        sd(NIST120c$d.18O.16O)
      # gather stats
        NIST120c_mean <- mean(NIST120c$d.18O.16O)

 # For now, just source from local 
        setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
        V1075_BySpec <- read.csv("V1075_BySpec.csv")
        NIST120c <- read.csv("V1075_NIST120c.csv")
        
        
        
  # subset fish scales
    GarScales <- subset(V1075_BySpec, Element.type == "ganoid scale")
  # subset Croc G
    CrocG <- subset(V1075_BySpec, eco_type == "Croc G")
  # subset Aquatic Turtle
    AquaTurt <- subset(V1075_BySpec, eco_type == "Aquatic Turtle")


# Gather Means
  # Croc G
    AquaCroc_d18Op_mean <- mean(CrocG$d18O)
  # Glyptops
    AquaTurt_d18Op_mean <- mean(AquaTurt$d18O)
  # Gar scales
    GarScales_d18Op_mean <- mean(GarScales$d18O)


# Dual-Taxon Temperature Estimates
  # Calculate d18Osurface_water
    crocwater <- 0.82*(AquaCroc_d18Op_mean) - 19.93 #Amiot et al.(2007)
    turtwater <- 1.01 *(AquaTurt_d18Op_mean) - 22.3 #Barrick et al. (1999)
  # Calculate Temps (Puceat et al., 2010)
    temp_CrocFish <- 118.7 - 4.22*((GarScales_d18Op_mean  +(22.6 - NIST120c_mean)) - crocwater ) 
    temp_TurtleFish <- 118.7 - 4.22*((GarScales_d18Op_mean  +(22.6 - NIST120c_mean)) - turtwater ) 

# Compute Endotherm-Ectotherm Combined Mean (EECM, Cullen et al., 2019)
  
  # Gather mean for each eco_type
    V1075_summary <- V1075_BySpec %>%
      group_by(eco_type) %>%
      summarise(
        thermophysiology = first(thermophysiology),  # Assuming thermophysiology is constant within each eco_type
        n = n(),
        d18Opmean = mean(d18O, na.rm = TRUE)
      )
  
  # Subset endotherms and ectotherms
    Endo <- subset(V1075_summary, thermophysiology == "endotherm")
    Ecto <- subset(V1075_summary, thermophysiology == "ectotherm")
  
  # Compute mean d18Obody_water for endotherms
    # Compute for each dinosaur group, use bird equation from Lazzerini et al. (2016)
      # Define birdwater function
        birdwater <- function(d18O) {
          return(1.077 * d18O - 34.607)
        }
      
      # Create new column in Endo to store d18Obody_water
        V1075_summary$d18Obody_water <- NA
      # Apply birdwater to dinosaurs
        Endo <- Endo %>%
          mutate(d18Obody_water = birdwater(d18Opmean))
  
  # Take mean of endotherms and mean of ectotherms
        endo_mean <- mean(Endo$d18Obody_water)
        ecto_mean <- mean(Ecto$d18Opmean)
  # Compute EECM
    # Define EECM function based on Cullen et al. (2019) and Puceat et al. (2010)
        EECM2023 <- function(d18Op_ecto, NIST120c_mean, d18Obw_endo) {
          EECM23 = 118.7 - 4.22 * ((d18Op_ecto + (22.6 - NIST120c_mean)) - d18Obw_endo)
          return(EECM)
        }
    # Define EECM based on Cullen et al. (2019)
        EECM2019 <- function(d18Op_ecto, d18Obw_endo) {
          EECM19 <- 111.4 - ( 4.3 * ( (d18Op_ecto) - (d18Obw_endo) ))
        }
    # Run EECM on means
        EECM23 <- EECM2023(ecto_mean, NIST120c_mean, endo_mean)
        EECM19 <- EECM2019(ecto_mean, endo_mean)

    




# Print out temps

  cat("Temperature Estimates:\n",
      "Dual-Taxon Croc-Fish:", round(CrocFish_temp, 1), "\n",
      "Dual-Taxon Turtle-Fish:", round(TurtleFish_temp, 1), "\n",
      "EECM19:", round(EECM19, 1), "\n",
      "EECM23:", round(EECM23, 1), "\n")
