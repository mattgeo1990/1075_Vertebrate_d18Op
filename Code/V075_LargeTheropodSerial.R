# Packages
 library(ggplot2)
 
# Read Data -------------------------------
  # Read raw sample data
    setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/")
    raw <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")
      
# Subset Large Theropod (Carcharodontid)
    
    Allosaur <- subset(raw, Taxon == "Carcharodontosaurid")
    
    