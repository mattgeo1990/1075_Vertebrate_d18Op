library(dplyr)
# For now, just source from local 
setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
V1075_BySpec <- read.csv("V1075_BySpec.csv")
raw <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")
NIST120c <- read.csv("V1075_NIST120c.csv")

# Remove d18OVSMOW column from V1075_BySpec, this column is an artifact
V1075_BySpec <- V1075_BySpec %>% select(-d18O..VSMOW.)
# Remove Allosauroid data, it can't be used for stats because n = 1
V1075_BySpec_forstats <- V1075_BySpec[which(V1075_BySpec$Taxon != "Allosauroid"),]
table(V1075_BySpec_forstats$Taxon)
# subset the Glyptops, Neosuchian G, and gar
  
  # gar first
    gar <- V1075_BySpec[which(V1075_BySpec$Taxon == "Lepisosteids"),]
    # test it (should be 17 rows)
      nrow(gar)
    
    #subset gar teeth vs gar scales
    gar_teeth <- gar[which(gar$Element.type == "tooth"),]
    # test it (should be 7 rows)
    nrow(gar_teeth)
    # now scales
    gar_scales <- gar[which(gar$Element.type == "ganoid scale"),]
    # test it (should be 10)
    nrow(gar_scales)
    
  # now Glyptops
    turtle <- V1075_BySpec[which((V1075_BySpec$Taxon == "Glyptops sp.")),]
    # test it (should be 7 rows)
    nrow(turtle)
    
  # now Neosuchian G
    croc_g <- V1075_BySpec[which((V1075_BySpec$Taxon == "Neosuchian G")),]
    # test it (should be 12 rows)
    nrow(croc_g)
    
# plotting gar data to look at distributions
# need to decide whether to include teeth or not
    # Combine the data into a list
data <- list(Teeth = gar_teeth$d18O, Scales = gar_scales$d18O)

# Create a strip chart
stripchart(data,
           vertical = TRUE,            # Make the plot vertical
           method = "jitter",          # Add jitter to avoid overlap
           col = c("blue", "red"),     # Colors for teeth and scales
           pch = 16,                   # Use filled circles for points
           xlab = "Sample Type",       # X-axis label
           ylab = "δ18O (per mil)")    # Y-axis label


# Inspect the Data --------------------------------------------------------

# Load necessary library
library(dplyr)

# Group data by Taxon and perform Shapiro-Wilk test on d18O
normality_results <- V1075_BySpec_forstats %>%
  group_by(Taxon) %>%
  summarise(
    Shapiro_W_p = shapiro.test(d18O)$p.value,  # p-value of Shapiro-Wilk test
    Shapiro_W_statistic = shapiro.test(d18O)$statistic  # W statistic of Shapiro-Wilk test
  )

# Print the results
print(normality_results)

# Optional: Highlight taxa that deviate from normality
normality_results %>%
  mutate(Normality = ifelse(Shapiro_W_p > 0.05, "Normal", "Not Normal")) %>%
  print()

# Visualize distributions for each Taxon
library(ggplot2)

ggplot(V1075_BySpec_forstats, aes(x = d18O)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  facet_wrap(~Taxon, scales = "free_y") +
  labs(title = "Distribution of d18O by Taxon",
       x = "d18O",
       y = "Frequency") +
  theme_minimal()

# Test to see if whole distribution is normal and unimodal
hist(V1075_BySpec$d18O, breaks = 20)
shapiro.test(V1075_BySpec$d18O)
summary(V1075_BySpec$d18O)
summary(turtle$d18O)

# simple models of diageneitc influence
d18Ow <- 0
d18Op <- 15
118.7 - 4.22*(d18Op - d18Ow) 

T <- c(50,100,150)
d18Op + (118.7 - T) / 4.22
# water reconstruction ----------------------------------------------------

# from turtle
    # d18Owater <- 1.01 *(d18O..VSMOW.) - 22.3 #Barrick et al. (1999)
    
    mean_turtle_d18Op <- mean(turtle$d18O)
    turtle_water <- 1.01 *(mean_turtle_d18Op) - 22.3
     # I am satisfied with this value, it is consistent

#from croc
    # d18Owater <- 0.82 * V1075_BySpec$d18O - 19.93

    mean_crocg_d18Op <- mean(croc_g$d18O)
    croc_water <- 0.82 * mean_crocg_d18Op - 19.93
    
    
    
    # I am happy with this value, it is consistent
    
    t.test(turtle$d18O, croc_g$d18O, var.equal = FALSE)
    
    cw <- Amiot_slope * croc_g$d18O - (-Amiot_intercept)
    tw <- Barrick_slope *(turtle$d18O) - (-Barrick_intercept)
    mean(cw)
    sd(cw)/sqrt(length(cw))
    mean(tw)
    sd(tw)/sqrt(length(tw))
    t.test(cw, tw, var.equal = FALSE)
    
# dual-taxon temperature --------------------------------------------------
# first NIST120c
    NIST120c_mean <- mean(NIST120c$d.18O.16O)
# gar scale temp
    mean_d18Op_gar_scales <- mean(gar_scales$d18O)
    temp_TurtleFishscales <- 118.7 - 4.22*((mean_d18Op_gar_scales  +(22.6 - NIST120c_mean)) - turtle_water) 
    
# gar teeth temp
    mean_d18Op_gar_teeth <- mean(gar_teeth$d18O)
    temp_TurtleFishteeth <- 118.7 - 4.22*((mean_d18Op_gar_teeth  +(22.6 - NIST120c_mean)) - turtle_water) 
    
# gar all
    mean_d18Op_gar_all <- mean(gar$d18O)
    temp_TurtleFishall <- 118.7 - 4.22*((mean_d18Op_gar_all  +(22.6 - NIST120c_mean)) - turtle_water) 

# gar all AND turtlecroc all
    allwater <- mean(turtle_water,croc_water)
    temp_multitaxa <- 118.7 - 4.22*((mean_d18Op_gar_all  +(22.6 - NIST120c_mean)) - allwater) 
   
   
# Transfer Function -------------------------------------------------------
    # water temp
   # Tw <- temp_TurtleFishall
    Tw <- 25
    # transfer function for growing season water temp to mean annual air temp from Hren and Sheldon (2012):
    MAAT <- -0.0146 * (Tw^2) +
      1.753 * Tw - 
      16.079
  

    # Define coefficients
    intercept <- 3.72984
    slope <- 0.78520
    
    # Input Tw_AMJJAS value
    Tw_AMJJAS <- 26  # Replace with your value
    
    # Calculate Ta_AMJJAS
    Ta_AMJJAS <- intercept + slope * Tw_AMJJAS
    print(Ta_AMJJAS)
