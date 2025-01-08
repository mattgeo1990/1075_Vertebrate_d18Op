
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
library(tidyverse)
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
V1075_MCwater <- read.csv("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/d18Owater_MCsim_summary.csv")
#V1075_MCwater <- read.csv("V1075_MCwater.csv")
V1075_alldata <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")

# Models
LatTempModern <- -0.003* (lat^2) - 0.2514 * lat+30.113 #Rozanski et al., 1993


# Water by Taxon -----------------------------------------------------------------------

# Remove gar, sharks, and dinosaurs
# The fish can't inform water estimates because body temp is variable
# dinosaur d18Owater can't be reliably reconstructed because there is no framework (Kohn's mammal equations aren't a good fit, and we have to assume humidity if we use those anyways)

V1075_MCwater$Taxon[which(V1075_MCwater$Taxon == "crocA")] <- "Neosuchian A"
V1075_MCwater$Taxon[which(V1075_MCwater$Taxon == "crocB")] <- "Neosuchian B"
V1075_MCwater$Taxon[which(V1075_MCwater$Taxon == "crocG")] <- "Neosuchian G"


# Define the conditions for assigning values to the "Habitat" column
V1075_MCwater$Habitat <- ifelse(V1075_MCwater$Taxon %in% c("Glyptops", "crocG", "crocB"), "Amphibious", 
                                ifelse(V1075_MCwater$Taxon %in% c("Naomichelys", "crocA"), "Terrestrial", NA))

# Display the modified data frame
#V1075_MCwater
#V1075_MCwater$PlotOrder <- 
  
  
  # Reorder Taxon based on Habitat
  V1075_MCwater$Taxon <- factor(V1075_MCwater$Taxon, levels = unique(V1075_MCwater$Taxon[order(V1075_MCwater$Habitat)]))

# Plotting
ggplot(V1075_MCwater, aes(x = Mean_d18Owater, y = Taxon)) +
  geom_point() +  # Add points for mean
  geom_errorbarh(aes(xmin = Lower_95_CI, xmax = Upper_95_CI), height = 0.2) +  # Add horizontal error bars
  labs(x = "d18Owater", y = "Taxon")  # Label x and y axes

# create delta notation for label
oxydeltphosphate <- expression(paste(delta^{18}, "O"[p], " (‰ V-SMOW)"))
oxydeltwater <- expression(paste(delta^{18}, "O"[sw], " (‰ V-SMOW)"))
# Plotting
ggplot(V1075_MCwater, aes(x = Mean_d18Owater, y = Taxon)) +
  geom_point() +  # Add points for mean
  geom_errorbarh(aes(xmin = Lower_95_CI, xmax = Upper_95_CI), height = 0.2) +  # Add horizontal error bars
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

tiff("gg_d18Ow_nodinos_small.tiff", units="in", width= 3.5, height= 3, res=500)

ggsave("gg_d18Ow_nodinos_small.tiff", units="in", width= 3.5, height= 3, dpi=500, compression = 'lzw')

dev.off()


# all water by taxon ------------------------------------------------------

# Looking for the plot with all the taxa water reconstructions?
# I removed it because we are no longer including dinosaurs and fish in the reconstructions
# See first round reviewers comments on 1075 Frontiers manuscript
# Also see comments above in "Water by Taxon" section

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
  # ggtitle("V1075 d18Op by fossil specimen") +
  labs(x = oxydeltphosphate, y = "") +
  theme(strip.placement = "outside",
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_rect(color = "black", size = 1, fill = NA),  # Set border color and remove fill
        axis.ticks.x = element_line(color = "black"),  # Set x-axis ticks color
        axis.ticks.y = element_line(color = "black"),  # Set y-axis ticks color
        axis.text.x = element_text(size = 8, color = "black", hjust = 1),  # Set x-axis tick label size, orientation, and bold font
        axis.text.y = element_text(size = 8, color = "black", hjust = 1, face = "italic")) +  # Set y-axis tick label size, orientation, and bold font
  coord_cartesian(expand = FALSE, xlim = c(8, 24), ylim = c(0, 12)) +
  scale_x_continuous(breaks = seq(8, 24, by = 2))

# Print and export using tiff() and dev.off

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Exports")

tiff("gg_BySpec_small.tiff", units="in", width= 3.5, height= 3, res=500)

ggsave("gg_BySpec_small.tiff", units="in", width= 3.5, height= 3, dpi=500, compression = 'lzw')

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


# Temp Grad ---------------------------------------------------------------


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
OKTXtemp_upper <- OKTX_TempLat$temp+3
OKTXtemp_lower <- OKTX_TempLat$temp-3
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
  geom_line(data = Judd24_Albian, aes(x = lat, y = temp_mean), size = 1, color = "#E66101") +
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
      panel.grid = element_blank(),                      # Remove grid lines
      panel.border = element_rect(color = "black", fill = NA), # Add border
      axis.ticks = element_line(color = "black"),        # Keep axis tick marks
      axis.text = element_text(size = 12),               # Customize axis text size
      axis.title = element_text(size = 14)               # Customize axis title size
    )

# Print and export using tiff() and dev.off

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Exports")

tiff("temp_gradient.tiff", units="mm", width= 74, height = 70, res=500)

ggsave("temp_gradient.tiff", units="mm", width= 74, height= 70, dpi=500, compression = 'lzw')

dev.off()


# d18Owater Grad ----------------------------------------------------------
# Paleolatitude of Cloverly Formation (Vaes et al., 2023)
Cloverly_Paleolat_mean <- 51.76046
Cloverly_paleolat_CI_upper <- 50.20277
Cloverly_paleolat_CI_lower <- 53.36299

# Ruby Ranch paleolat (Vaes et al., 2023 reconstruction based on lat: 38.81°N, long: 104.54°W of nearby Colorado Springs, CO)
RRlat <- 45.34
RRlat_upper <- 46.94
RRlat_lower <- 43.79

# Trinity paleolat based on lat: 33.73°N, long: -97.16 of the I-35 bridge over the Red River, in vicinity of Kate's sampling sites (https://doi.org/10.1016/j.palaeo.2019.109491)
OKTXlat <- 38.72
OKTXlat_upper <- 40.3
OKTXlat_lower <- 37.2

# load Celina's turtle data
CelinaTurtles <- read.csv("/Users/allen/Documents/Data Analysis/Data/Geochem/SuarezEtAl2020_AptianAlbianTurtleDATA.csv")

# set the paleolatitudes
CelinaTurtles$Palaeolatitude[which(CelinaTurtles$Formation == "Holly Creek")] <- OKTXlat # Antlers and Holly Creek are at nearly same coordinates
CelinaTurtles$Palaeolatitude[which(CelinaTurtles$Formation == "Cedar Mountain")] <- RRlat
CelinaTurtles$Palaeolatitude[which(CelinaTurtles$Formation == "Cloverly")] <- Cloverly_Paleolat_mean

hf <- mean(CelinaTurtles$d18Ow[which(CelinaTurtles$Formation == "Holly Creek")])
cl <- mean(CelinaTurtles$d18Ow[which(CelinaTurtles$Formation == "Cloverly")])
cmf <- mean(CelinaTurtles$d18Ow[which(CelinaTurtles$Formation == "Cedar Mountain")])

# Estimate 95% CI of d18Ow

# Subset Cedar Mountain data
filtered_d18Ow <- CelinaTurtles$d18Ow[which(CelinaTurtles$Formation == "Cedar Mountain")]
# Check if there are enough data points
if (length(filtered_d18Ow) > 1) {
  # Calculate the mean and standard error
  mean_value <- mean(filtered_d18Ow, na.rm = TRUE)
  std_error <- sd(filtered_d18Ow, na.rm = TRUE) / sqrt(length(filtered_d18Ow))
  
  # Calculate the 95% CI limits
  CMF_d18Ow_ci_lower <- mean_value - 1.96 * std_error
  CMF_d18Ow_ci_upper <- mean_value + 1.96 * std_error
  
  # Print the results
  cat("95% Confidence Interval:\n")
  cat("Lower Limit:", CMF_d18Ow_ci_lower, "\n")
  cat("Upper Limit:", CMF_d18Ow_ci_upper, "\n")
} else {
  cat("Not enough data points to calculate a 95% CI.\n")
}
sd(hc_filtered_d18Ow)
# Subset Holly Creek data
hc_filtered_d18Ow <- CelinaTurtles$d18Ow[which(CelinaTurtles$Formation == "Holly Creek")]
# Check if there are enough data points
if (length(hc_filtered_d18Ow) > 1) {
  # Calculate the mean and standard error
  mean_value <- mean(hc_filtered_d18Ow, na.rm = TRUE)
  std_error <- sd(hc_filtered_d18Ow, na.rm = TRUE) / sqrt(length(hc_filtered_d18Ow))
  
  # Calculate the 95% CI limits
  HC_d18Ow_ci_lower <- mean_value - 1.96 * std_error
  HC_d18Ow_ci_upper <- mean_value + 1.96 * std_error
  
  # Print the results
  cat("95% Confidence Interval:\n")
  cat("Lower Limit:", HC_d18Ow_ci_lower, "\n")
  cat("Upper Limit:", HC_d18Ow_ci_upper, "\n")
} else {
  cat("Not enough data points to calculate a 95% CI.\n")
}


# Subset Cloverly data
cl_filtered_d18Ow <- CelinaTurtles$d18Ow[which(CelinaTurtles$Formation == "Cloverly")]
# Check if there are enough data points
if (length(cl_filtered_d18Ow) > 1) {
  # Calculate the mean and standard error
  mean_value <- mean(cl_filtered_d18Ow, na.rm = TRUE)
  std_error <- sd(cl_filtered_d18Ow, na.rm = TRUE) / sqrt(length(cl_filtered_d18Ow))
  
  # Calculate the 95% CI limits
  cl_d18Ow_ci_lower <- mean_value - 1.96 * std_error
  cl_d18Ow_ci_upper <- mean_value + 1.96 * std_error
  
  # Print the results
  cat("95% Confidence Interval:\n")
  cat("Lower Limit:", cl_d18Ow_ci_lower, "\n")
  cat("Upper Limit:", cl_d18Ow_ci_upper, "\n")
} else {
  cat("Not enough data points to calculate a 95% CI.\n")
}

# Create the data frame
celina_turtles <- data.frame(
  mean_d18Ow = c(hf, cl, cmf),
  lat = c(OKTXlat, Cloverly_Paleolat_mean, RRlat),
  lat_CI_upper <- c(OKTXlat_upper, Cloverly_paleolat_CI_upper, RRlat_upper),
  lat_CI_lower <- c(OKTXlat_lower, Cloverly_paleolat_CI_lower, RRlat_lower),
  d18Ow_upper <- c(HC_d18Ow_ci_upper, cl_d18Ow_ci_upper,CMF_d18Ow_ci_upper),
  d18Ow_lower <- c(HC_d18Ow_ci_lower, cl_d18Ow_ci_lower,CMF_d18Ow_ci_lower)
)

lat <- seq(0,90,by=1)

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
V1075_MC_output_summary <- read.csv("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/V1075_MCoutput_summary.csv")
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
 # Add Celina's data
  geom_point(data = celina_turtles, aes(x = lat, y = mean_d18Ow), size = 2, color = "black", shape = 5) +
  # Add latitude 95% CI error bars for Celina's data
  geom_errorbarh(data = celina_turtles, aes(y = mean_d18Ow, xmin = lat_CI_upper....c.OKTXlat_upper..Cloverly_paleolat_CI_upper.., xmax = lat_CI_lower....c.OKTXlat_lower..Cloverly_paleolat_CI_lower..), 
                 height = 0.5, color = "black") +
  # add V1075 data
  geom_point(data = V1075_dual_d18Osw, aes(x = lat, y = Mean), size = 2, color = "black") +
  # Add latitude 95% CI error bars for V1075
  geom_errorbarh(data = V1075_dual_d18Osw, aes(y = Mean, xmin = lat_lower, xmax = lat_upper), 
                 height = 0.5, color = "black") +
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

# Print and export using tiff() and dev.off

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Exports")

tiff("d18Owater_gradient.tiff", units="mm", width= 74, height = 70, res=500)

ggsave("d18Owater_gradient.tiff", units="mm", width= 74, height= 70, dpi=500, compression = 'lzw')

dev.off()

