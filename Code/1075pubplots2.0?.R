
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
oxydelt <- expression("δ"^18 * "O‰ (VSMOW)")

# Read in raw data
V1075raw_githubURL <- "https://github.com/mattgeo1990/1075_Vertebrate_d18Op/blob/main/Data/V1075_PhosphateData_8-18-23_copy.csv"
V107_raw <- read.csv(V1075raw_githubURL)

# Read in cleaned data (include link to script that cleans the data?)
# Call V1075_cl from GitHub 
# cleaning script is here: https://github.com/mattgeo1990/1075_Vertebrate_d18Op/blob/main/Code/V1075_d18O.R
# V1075cl_githubURL <- "https://github.com/mattgeo1990/1075_Vertebrate_d18Op/blob/main/Data/V1075_cl.csv"
# V1075_cl <- read.csv(V1075cl_githubURL)

# For now, just source from local 
setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
V1075_BySpec <- read.csv("V1075_BySpec.csv")
NIST120c <- read.csv("V1075_NIST120c.csv")

oxydelt <- expression("δ"^18 * "O‰ (V-SMOW)")


# Stat Plots -----------------------------------------------------------------------


# d18O by Eco_type
ggplot(V1075_BySpec, aes(x=reorder(Eco, d18O..VSMOW., na.rm = TRUE), y=d18O..VSMOW.))+
  theme(panel.background = element_rect(color = "black", size = 2, fill = NA)) +
  geom_jitter(position=position_jitter(0), shape = 1, size = 3, stroke = 1) +
  theme(panel.grid.major.x = element_line(color = "black", size = 0.1)) +
  theme(axis.text.x = element_text(size = 15, angle = -45, hjust = 0, face = "bold")) +
  theme(axis.text.y = element_text(size = 15, hjust = 0, color = "black")) +
  theme(axis.ticks.x = element_line())+
  theme(panel.grid.major.y = element_blank())+
  theme(panel.grid.minor.y = element_blank())+
  theme(plot.margin = margin(20, 40, 20, 20, "pt")) +
  theme(axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(breaks = seq(10, 25, by = 2)) +
  labs(x="", y = oxydelt)

# d18O by Taxon
ggplot(V1075_cl, aes(x=reorder(Taxon, d18O..VSMOW., na.rm = TRUE), y=d18O..VSMOW.))+
  theme(panel.background = element_rect(color = "black", size = 2, fill = NA)) +
  geom_jitter(position=position_jitter(0), shape = 1, size = 3, stroke = 1) +
  theme(panel.grid.major.x = element_line(color = "black", size = 0.1)) +
  theme(axis.text.x = element_text(size = 15, angle = -45, hjust = 0, face = "bold")) +
  theme(axis.text.y = element_text(size = 15, hjust = 0, color = "black")) +
  theme(axis.ticks.x = element_line())+
  theme(panel.grid.major.y = element_blank())+
  theme(panel.grid.minor.y = element_blank())+
  theme(plot.margin = margin(20, 40, 20, 20, "pt")) +
  theme(axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(breaks = seq(10, 25, by = 2)) +
  labs(x="", y = oxydelt)


