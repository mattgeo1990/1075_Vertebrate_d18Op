
# Packages, Data ----------------------------------------------------------

# V1075_MCwater is sourced from script "V1075_MC_alltaxa.R"
# V1075_BySpec is sourced from "V1075_d18O_wrangle.R"

# Install and load required packages
packages_to_install <- c("ggpubr", "gridExtra", "ggplot2", "knitr", "outliers", "dplyr", "readr")

if (length(setdiff(packages_to_install, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages_to_install, rownames(installed.packages())))
}

library(gridExtra)
library(ggpubr)
library(knitr)
library(ggplot2)
library(outliers)
library(dplyr)

# Here are custom delta notation objects
oxydeltphosphate <- expression("δ"^18 * "O"[phosphate] * "(‰ V-SMOW)")
oxydeltwater <- expression("δ"^18 * "O"[water] * "(‰ V-SMOW)")

# Read in raw data
#V1075raw_githubURL <- "https://github.com/mattgeo1990/1075_Vertebrate_d18Op/blob/main/Data/V1075_PhosphateData_8-18-23_copy.csv"
#V107_raw <- read.csv(V1075raw_githubURL)

# Read in cleaned data (include link to script that cleans the data?)
# Call V1075_cl from GitHub 
# cleaning script is here: https://github.com/mattgeo1990/1075_Vertebrate_d18Op/blob/main/Code/V1075_d18O.R
# V1075cl_githubURL <- "https://github.com/mattgeo1990/1075_Vertebrate_d18Op/blob/main/Data/V1075_cl.csv"
# V1075_cl <- read.csv(V1075cl_githubURL)

# For now, just source from local 
setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
V1075_BySpec <- read.csv("V1075_BySpec_water.csv")
NIST120c <- read.csv("V1075_NIST120c.csv")
V1075_MCwater <- read.csv("V1075_MCwater.csv")
V1075_alldata <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")


# Water by Taxon -----------------------------------------------------------------------

# Remove rows corresponding to "Shark" and "Fish"
V1075_MCwater <- subset(V1075_MCwater, Taxon != "Hybodontiformes" & Taxon != "Lepisosteids" & Taxon != "Carcharodontosaurid")

# Define the conditions for assigning values to the "Habitat" column
V1075_MCwater$Habitat <- ifelse(V1075_MCwater$Taxon %in% c("Glyptops sp.", "Neosuchian G", "Neosuchian B"), "Amphibious", 
                                ifelse(V1075_MCwater$Taxon %in% c("Naomichelys sp.", "Maniraptorans", "Sauropods", "Ornithischians", "Neosuchian A"), "Terrestrial", NA))

# Display the modified data frame
V1075_MCwater

V1075_MCwater$PlotOrder <- 
  
  
  # Reorder Taxon based on Habitat
  V1075_MCwater$Taxon <- factor(V1075_MCwater$Taxon, levels = unique(V1075_MCwater$Taxon[order(V1075_MCwater$Habitat)]))

# Plotting
ggplot(V1075_MCwater, aes(x = MEAN, y = Taxon)) +
  geom_point() +  # Add points for mean
  geom_errorbarh(aes(xmin = loCL, xmax = hiCL), height = 0.2) +  # Add horizontal error bars
  labs(x = "d18Owater", y = "Taxon")  # Label x and y axes

# create delta notation for label
oxydeltphosphate <- expression(paste(delta^{18}, "O"[p], " (‰ V-SMOW)"))
oxydeltwater <- expression(paste(delta^{18}, "O"[ingested_water], " (‰ V-SMOW)"))
# Plotting
ggplot(V1075_MCwater, aes(x = MEAN, y = Taxon)) +
  geom_point() +  # Add points for mean
  geom_errorbarh(aes(xmin = loCL, xmax = hiCL), height = 0.2) +  # Add horizontal error bars
  labs(x = oxydeltwater, y = element_blank()) +  # Label x and y axes
  theme_minimal() +  # Set minimal theme
  scale_x_continuous(breaks = seq(0, -10, by = -1)) +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),  # Remove background
        axis.text.x = element_text(color= "black", size=8),
        axis.text.y = element_text(color= "black", size=8, face = "italic"),
        axis.ticks = element_line())



#using tiff() and dev.off

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Exports")

tiff("gg_d18Ow.tiff", units="in", width= 6.2361, height= 2.787, res=500)

ggsave("gg_d18Ow.tiff", units="in", width= 6.2361, height= 2.787, dpi=500, compression = 'lzw')

dev.off()


# all water by taxon ------------------------------------------------------

V1075_BySpec <- read.csv("V1075_BySpec_water.csv")

# Define the conditions for assigning values to the "Habitat" column
V1075_BySpec$Habitat <- ifelse(V1075_BySpec$Taxon %in% c("Glyptops sp.", "Neosuchian G", "Neosuchian B"), 
                                "Amphibious", 
                                ifelse(V1075_BySpec$Taxon %in% c("Naomichelys sp.", "Maniraptorans", "Sauropods", "Ornithischians", "Neosuchian A"), 
                                       "Terrestrial", 
                                       ifelse(V1075_BySpec$Taxon %in% c("Lepisosteids", "Hybodonts"), 
                                              "Aquatic", 
                                              NA)))

  
  
  # Reorder Taxon based on Habitat
  V1075_BySpec$Taxon <- factor(V1075_BySpec$Taxon, levels = unique(V1075_BySpec$Taxon[order(V1075_BySpec$Habitat)]))

  # Define custom levels for Habitat
  custom_levels <- c("Aquatic", "Amphibious", "Terrestrial")
  
  # Reorder the Taxon variable based on Habitat with custom levels
  V1075_BySpec$Taxon <- factor(V1075_BySpec$Taxon, levels = unique(V1075_BySpec$Taxon[order(factor(V1075_BySpec$Habitat, levels = custom_levels))]))
  


# Create the plot
ggplot(V1075_BySpec, aes(x = d18Owater, y = Taxon)) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.7, shape = 1) +  # Use shape = 1 for open circles
  theme_minimal() +
  ggtitle("V1075 d18Owater by fossil specimen") +
  labs(x = oxydeltwater, y = "") +
  theme(strip.placement = "outside",
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA),  # Set border color and remove fill
        axis.ticks.x = element_line(color = "black"),  # Set x-axis ticks color
        axis.ticks.y = element_line(color = "black"),  # Set y-axis ticks color
        axis.text.x = element_text(size = 8, hjust = 1, face = "bold"),  # Set x-axis tick label size, orientation, and bold font
        axis.text.y = element_text(size = 8, hjust = 1, face = "italic")) +  # Set y-axis tick label size, orientation, and bold font
  coord_cartesian(expand = FALSE, xlim = c(-14, 0), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(-14, 0, by = 1))

#using tiff() and dev.off

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Exports")

tiff("gg_d18Ow.tiff", units="in", width= 6.2361, height= 2.787, res=500)

ggsave("gg_d18Ow.tiff", units="in", width= 6.2361, height= 2.787, dpi=500, compression = 'lzw')

dev.off()
# d18O by Specimen --------------------------------------------------------


library(ggplot2)

# Create the plot
ggplot(V1075_BySpec, aes(x = d18O, y = Taxon)) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(x = "d18O", y = "Taxon") +
  theme(strip.placement = "outside")


# Calculate mean d18O by eco_type
mean_d18O_by_Taxon <- V1075_BySpec %>%
  group_by(Taxon) %>%
  summarise(mean_d18O = mean(d18O..VSMOW., na.rm = TRUE)) %>%
  arrange(mean_d18O)

# Reorder eco_type levels based on mean d18O
V1075_BySpec$Taxon <- factor(V1075_BySpec$Taxon, levels = mean_d18O_by_Taxon$Taxon)

# Create the Plot
ggplot(V1075_BySpec, aes(x = d18O..VSMOW., y = Taxon)) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.7, shape = 1) +  # Use shape = 1 for open circles
  theme_minimal() +
  ggtitle("V1075 d18Op by fossil specimen") +
  labs(x = oxydeltphosphate, y = "") +
  theme(strip.placement = "outside",
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA),  # Set border color and remove fill
        axis.ticks.x = element_line(color = "black"),  # Set x-axis ticks color
        axis.ticks.y = element_line(color = "black"),  # Set y-axis ticks color
        axis.text.x = element_text(size = 8, hjust = 1, face = "bold"),  # Set x-axis tick label size, orientation, and bold font
        axis.text.y = element_text(size = 8, hjust = 1, face = "italic")) +  # Set y-axis tick label size, orientation, and bold font
  coord_cartesian(expand = FALSE, xlim = c(8, 24), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(8, 24, by = 2))

# Print and export using tiff() and dev.off

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Exports")

tiff("gg_BySpec.tiff", units="in", width= 6.2, height= 2.7, res=500)

ggsave("gg_BySpec.tiff", units="in", width= 6.2, height= 2.7, dpi=500, compression = 'lzw')

dev.off()

# all measurements --------------------------------------------------------


# Calculate mean d18O by Taxon
  #mean_d18O_by_Taxon <- V1075_alldata %>%
    #group_by(Taxon) %>%
    #summarise(mean_d18O = mean(d18O..VSMOW., na.rm = TRUE)) %>%
    #arrange(mean_d18O)

# Reorder Taxon levels based on mean d18O
V1075_alldata$Taxon <- factor(V1075_alldata$Taxon, levels = mean_d18O_by_Taxon$Taxon)

# Create the plot with "Taxon" and open circles
ggplot(V1075_alldata, aes(x = d18O..VSMOW., y = Taxon)) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.7, shape = 1) +  # Use shape = 1 for open circles
  theme_minimal() +
  ggtitle("V1075 individual d18Op measurements") +  # Add main title here
  labs(x = oxydeltphosphate, y = "") +
  theme(strip.placement = "outside",
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA),  # Set border color and remove fill
        axis.ticks.x = element_line(color = "black"),  # Set x-axis ticks color
        axis.ticks.y = element_line(color = "black"),  # Set y-axis ticks color
        axis.text.x = element_text(size = 8, hjust = 1, face = "bold"),  # Set x-axis tick label size, orientation, and bold font
        axis.text.y = element_text(size = 8, hjust = 1, face = "italic")) +  # Set y-axis tick label size, orientation, and bold font
  coord_cartesian(expand = FALSE, xlim = c(8, 24), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(8, 24, by = 2))



#using tiff() and dev.off

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Exports")

tiff("gg_alldata.tiff", units="in", width= 6.2, height= 2.7, res=500)

ggsave("gg_alldata.tiff", units="in", width= 6.2, height= 2.7, dpi=500, compression = 'lzw')

dev.off()


