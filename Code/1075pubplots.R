
# Packages, Data ----------------------------------------------------------


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

# Here is a custom delta notation object
oxydeltphosphate <- expression("δ"^18 * "O"[phosphate] * "(‰ V-SMOW)")

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
V1075_BySpec <- read.csv("V1075_BySpec.csv")
NIST120c <- read.csv("V1075_NIST120c.csv")
V1075_MCwater <- read.csv("V1075_MCwater.csv")
V1075_alldata <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")


# Water by Taxon -----------------------------------------------------------------------

# Remove rows corresponding to "Shark" and "Fish"
V1075_MCwater <- subset(V1075_MCwater, Taxon != "Shark" & Taxon != "Fish")

# Define the conditions for assigning values to the "Habitat" column
V1075_MCwater$Habitat <- ifelse(V1075_MCwater$Taxon %in% c("Aquatic Turtle", "Croc G", "Croc B"), "Aquatic or Semi-Aquatic", 
                                ifelse(V1075_MCwater$Taxon %in% c("Terrestrial Turtle", "Small Theropod", "Sauropoda", "Ornithischian", "Croc A"), "Terrestrial", NA))

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
        axis.text.y = element_text(color= "black", size=8),
        axis.ticks = element_line())




# d18O by Specimen --------------------------------------------------------


library(ggplot2)

# Create the plot
ggplot(V1075_BySpec, aes(x = d18O, y = Taxon)) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(x = "d18O", y = "Taxon") +
  theme(strip.placement = "outside")


# Calculate mean d18O by eco_type
mean_d18O_by_eco_type <- V1075_BySpec %>%
  group_by(eco_type) %>%
  summarise(mean_d18O = mean(d18O, na.rm = TRUE)) %>%
  arrange(mean_d18O)

# Reorder eco_type levels based on mean d18O
V1075_BySpec$eco_type <- factor(V1075_BySpec$eco_type, levels = mean_d18O_by_eco_type$eco_type)

# Create the plot
ggplot(V1075_BySpec, aes(x = d18O, y = eco_type)) +
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(x = oxydeltphosphate, y = "Taxon") +
  theme(strip.placement = "outside")


# all measurements --------------------------------------------------------


# Calculate mean d18O by Eco
mean_d18O_by_Eco <- V1075_alldata %>%
  group_by(Eco) %>%
  summarise(mean_d18O = mean(d18O..VSMOW., na.rm = TRUE)) %>%
  arrange(mean_d18O)

# Reorder Eco levels based on mean d18O
V1075_alldata$Eco <- factor(V1075_alldata$Eco, levels = mean_d18O_by_Eco$Eco)

# Create the plot with "Eco" instead of "Taxon" and open circles
gg_alldata <- ggplot(V1075_alldata, aes(x = d18O..VSMOW., y = Eco)) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.7, shape = 1) +  # Use shape = 1 for open circles
  theme_minimal() +
  labs(x = oxydeltphosphate, y = "Taxon") +
  theme(strip.placement = "outside",
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", fill = NA),  # Set border color and remove fill
        axis.ticks.x = element_line(color = "black"),  # Set x-axis ticks color
        axis.ticks.y = element_line(color = "black"),  # Set y-axis ticks color
        axis.text.x = element_text(size = 10, hjust = 1, face = "bold"),  # Set x-axis tick label size, orientation, and bold font
        axis.text.y = element_text(size = 10, hjust = 1, face = "italic")) +  # Set y-axis tick label size, orientation, and bold font
  coord_cartesian(expand = FALSE, xlim = c(8, 24), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(8, 24, by = 2))

print(gg_alldata)


#using tiff() and dev.off

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Exports")

tiff("gg_alldata.tiff", units="in", width= 6.2361, height= 2.787, res=500)

ggsave("gg_alldata.tiff", units="in", width= 6.2361, height= 2.787, dpi=500, compression = 'lzw')

dev.off()


