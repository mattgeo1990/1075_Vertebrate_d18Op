

# Install/load required packages ------------------------------------------
library(ggplot2)
library(dplyr)
  # Read in cleaned data (include link to script that cleans the data?)
    # Call V1075_cl from GitHub 
    # cleaning script is here: https://github.com/mattgeo1990/1075_Vertebrate_d18Op/blob/main/Code/V1075_d18O.R
      #setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
      #V1075_BySpec <- read.csv("V1075_BySpec.csv")
    # Source NIST data
      # READ IN NIST120c STD d18Op VALUES FROM RUN 1 and RUN 2
      # NEED NIST120C STD d18Op from Run 3!!!!!
        #standards_githubURL <-"https://raw.githubusercontent.com/mattgeo1990/1075_Vertebrate_d18Op/main/Data/V1075_NIST120c.csv"
        #NIST120c <- read_csv(standards_githubURL)
        #sd(NIST120c$d.18O.16O)
      # gather stats
        #NIST120c_mean <- mean(NIST120c$d.18O.16O)

 # For now, just source from local 
        setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
        V1075_BySpec <- read.csv("V1075_BySpec.csv")
        raw <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")
        NIST120c <- read.csv("V1075_NIST120c.csv")
        
  # Remove d18OVSMOW column from V1075_BySpec, this column is an artifact
        V1075_BySpec <- V1075_BySpec %>% select(-d18O..VSMOW.)
        
# For testing exactly what data you used in calculations        
      Gar_all_d18Op <- subset(raw$d18O..VSMOW., raw$Taxon == "Lepisosteids")
      Gar_scales_d18Op <- subset(raw$d18O..VSMOW., raw$Element.type == "ganoid scale")
      Gar_teeth_d18Op <- subset(raw$d18O..VSMOW., raw$Element.type == "tooth")
  summary(Gar_teeth_d18Op)
  summary(Gar_all_d18Op)
  summary(Gar_scales_d18Op)
  
  # subset fish scales
    GarScales <- subset(V1075_BySpec, Element.type == "ganoid scale")
  # subset Croc G
    CrocG <- subset(V1075_BySpec, eco_type == "Croc G")
  # subset Aquatic Turtle
    Glyp <- subset(V1075_BySpec, Taxon == "Glyptops sp.")

# plot histograms of d18Op
    ggplot(GarScales, aes(x = d18O)) +
      geom_histogram(binwidth = 1, fill = c("#ff0000"), color = "black", alpha = 0.7) +
      labs(title = "Histogram of Gar Scale d18Op",
           x = "d18Op",
           y = "Frequency")
    
    
# Gather Mean NIST
  # NIST
    NIST120c_mean <- mean(NIST120c$d.18O.16O)


# d18Ow Turtles ----------------------------------------------------------

    # d18Owater <- 1.01 *(d18O..VSMOW.) - 22.3 #Barrick et al. (1999)
    
    # Create a new column called d18Owater
    V1075_BySpec$d18Owater <- NA
    
    # Loop through each row and calculate d18Owater if Taxon is "Glyptops sp." or "Naomichelys sp."
    for (i in 1:nrow(V1075_BySpec)) {
      if (V1075_BySpec$Taxon[i] %in% c("Glyptops sp.", "Naomichelys sp.")) {
        V1075_BySpec$d18Owater[i] <- 1.01 * V1075_BySpec$d18O[i] - 22.3
      }
    }
    
    # Test the code
    V1075_BySpec[which(V1075_BySpec$Taxon %in% c("Glyptops sp.", "Naomichelys sp.")),]
 Glyp <- subset(V1075_BySpec, Taxon == "Glyptops sp.")
summary(Glyp$d18Owater)
sd(Glyp$d18Owater)
# d18Ow Crocs --------------------------------------------------------

    # Loop through each row and calculate d18Owater if Taxon is a Neosuchian
    for (i in 1:nrow(V1075_BySpec)) {
      if (V1075_BySpec$Taxon[i] %in% c("Neosuchian A", "Neosuchian B", "Neosuchian G")) {
        V1075_BySpec$d18Owater[i] <- 0.82 * V1075_BySpec$d18O[i] - 19.93
      }
    }
    
    # Test the code
    V1075_BySpec[which(V1075_BySpec$Taxon %in% c("Neosuchian A", "Neosuchian B", "Neosuchian G")),]
    CrocG <- subset(V1075_BySpec, Taxon == "Neosuchian G")
    
    summary(CrocG$d18Owater)
   sd(CrocG$d18Owater)
# d18Ow herbivores --------------------------------------------------------

    # Create Function
    Herbivore_Water <- function(d18Op, h) {
      result <- (d18Op) - 26.8 + (8.9 * (h)) / 0.76
      return(result)
    }
    
    # Loop through each row and calculate d18Owater if Taxon is an Ornithischian or Sauropod
    for (i in 1:nrow(V1075_BySpec)) {
      if (V1075_BySpec$Taxon[i] %in% c("Ornithischians", "Sauropods")) {
        V1075_BySpec$d18Owater[i] <- Herbivore_Water(V1075_BySpec$d18O[i], h = 0.5)
      }
    }
    
    
    # Test the code
    V1075_BySpec[which(V1075_BySpec$Taxon %in% c("Ornithischians", "Sauropods")),]

# d18Owater theropods -----------------------------------------------------

    # Create Function
    TheroWater <- function(d18O, h){
      (d18O) - 21.3 + (3*(h))/0.74
    }    
    
    # Loop through each row and calculate d18Owater if Taxon is an Ornithischian or Sauropod
    for (i in 1:nrow(V1075_BySpec)) {
      if (V1075_BySpec$Taxon[i] %in% c("Maniraptorans", "Allosauroid")) {
        V1075_BySpec$d18Owater[i] <- TheroWater(V1075_BySpec$d18O[i], h = 0.5)
      }
    }
    

# d8Ow fish ---------------------------------------------------------------

    # create "d18Owater from d18Ophsophate" function
    d18O_w_from_p <- function(MAT, d18O, NIST120c_mean) {
      d18Owater <- (MAT - 118.7 + 4.22 * d18O + 4.22 * (22.6 - NIST120c_mean)) / 4.22
      return(d18Owater)
    }
    
    # Loop through each row and calculate d18Owater if Taxon is an gar or shark
    for (i in 1:nrow(V1075_BySpec)) {
      if (V1075_BySpec$Taxon[i] %in% c("Lepisosteids", "Hybodonts")) {
        V1075_BySpec$d18Owater[i] <- d18O_w_from_p(MAT = 22.3, d18O = V1075_BySpec$d18O[i], NIST120c_mean = NIST120c_mean)
      }
    }
    
    # Test the code
    V1075_BySpec[which(V1075_BySpec$Taxon %in% c("Lepisosteids", "Hybodonts")),]
    
    # Export the results
write.csv(V1075_BySpec, "V1075_BySpec_water.csv")    
    
    
    
# Dual-Taxon Temperatures -------------------------------------------------
 # We will actually take the value and CI limits calculated by Monte Carlo sim
garmean <- mean(Gar_all_d18Op)
  # Calculate d18Osurface_water
    #crocwater <- 0.82*(AquaCroc_d18Op_mean) - 19.93 #Amiot et al.(2007)
    #turtwater <- 1.01 *(AquaTurt_d18Op_mean) - 22.3 #Barrick et al. (1999)
  # Calculate Temps (Puceat et al., 2010)
    #temp_CrocFish <- 118.7 - 4.22*((GarScales_d18Op_mean  +(22.6 - NIST120c_mean)) - crocwater ) 
    temp_TurtleFish <- 118.7 - 4.22*((Gar_all_d18Op  +(22.6 - NIST120c_mean)) - turtwater ) 

    
# Endotherm-Ectotherm Combined Mean (EECM, Cullen et al., 2019) -----------
 # What does this value even mean if we have to assume relative humidity?
  
  # Gather mean for each eco_type
   #V1075_summary <- V1075_BySpec %>%
      #group_by(eco_type) %>%
      #summarise(
        #thermophysiology = first(thermophysiology),  # Assuming thermophysiology is constant within each eco_type
        #n = n(),
        #d18Opmean = mean(d18O, na.rm = TRUE)
     # )
  
  # Subset endotherms and ectotherms
    #Endo <- subset(V1075_summary, thermophysiology == "endotherm")
    #Ecto <- subset(V1075_summary, thermophysiology == "ectotherm")
  
  # Compute mean d18Obody_water for endotherms
    # Compute for each dinosaur group, use bird equation from Lazzerini et al. (2016)
      # Define birdwater function
        #birdwater <- function(d18O) {
         # return(1.077 * d18O - 34.607)
       # }
      
      # Create new column in Endo to store d18Obody_water
       # V1075_summary$d18Obody_water <- NA
      # Apply birdwater to dinosaurs
        #Endo <- Endo %>%
        #  mutate(d18Obody_water = birdwater(d18Opmean))
  
  # Take mean of endotherms and mean of ectotherms
       # endo_mean <- mean(Endo$d18Obody_water)
       # ecto_mean <- mean(Ecto$d18Opmean)
  # Compute EECM
    # Define EECM function based on Cullen et al. (2019) and Puceat et al. (2010)
       # EECM2023 <- function(d18Op_ecto, NIST120c_mean, d18Obw_endo) {
        #  EECM23 = 118.7 - 4.22 * ((d18Op_ecto + (22.6 - NIST120c_mean)) - d18Obw_endo)
        #  return(EECM)
       # }
    # Define EECM based on Cullen et al. (2019)
        #EECM2019 <- function(d18Op_ecto, d18Obw_endo) {
         # EECM19 <- 111.4 - ( 4.3 * ( (d18Op_ecto) - (d18Obw_endo) ))
       # }
    # Run EECM on means
      #  EECM23 <- EECM2023(ecto_mean, NIST120c_mean, endo_mean)
      #  EECM19 <- EECM2019(ecto_mean, endo_mean)

    

# Transfer Function -------------------------------------------------------
# water temp
Tw <- 25
# transfer function for growing season water temp to mean annual air temp from Hren and Sheldon (2012):
MAAT <- -0.0146 * (Tw^2) +
  1.753 * Tw - 
  16.079





  