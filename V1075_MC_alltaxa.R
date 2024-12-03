

# Setup -------------------------------------------------------------------

# !!! V1075_MCdata is sourced from script "V1075_d18O_wrangle.R"

# Packages

library(dplyr)
library(purrr)
library(ggplot2)
library(RCurl)

# Data
# Call d18Op data from GitHub, cleaned and averaged per specimen (V1075_MCdata.csv)
#samples_githubURL <- getURL("https://raw.githubusercontent.com/mattgeo1990/1075_Vertebrate_d18Op/main/Data/V1075_cl")
#V1075_cl <- read.csv(text = samples_githubURL)

# READ IN NIST120c STD d18Op VALUES FROM RUN 1 and RUN 2
# NEED NIST120C STD d18Op from Run 3!!!!!
#standards_githubURL <- getURL("https://raw.githubusercontent.com/mattgeo1990/1075_Vertebrate_d18Op/main/Data/V1075_NIST120c.csv")
#NIST120c <- read.csv(text = standards_githubURL)

# local, if needed
  setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
  V1075_MCdata <- read.csv("V1075MC_data.csv")
  NIST120c <- read.csv("V1075_NIST120c.csv")

# Set number of Monte Carlo repetitions 
nMCrepetitions <- 1e5

# Subset V1075_cl by biological group
gar <- subset(V1075_MCdata, Taxon == "Lepisosteids")
shark <- subset(V1075_MCdata, Taxon == "Hybodonts")
glyptops <- subset(V1075_MCdata, Taxon == "Glyptops sp.")
naomichelys <- subset(V1075_MCdata, Taxon == "Naomichelys sp.")
crocG <- subset(V1075_MCdata, Taxon == "Neosuchian G")
crocA <- subset(V1075_MCdata, Taxon == "Neosuchian A")
crocB <- subset(V1075_MCdata, Taxon == "Neosuchian B")


# Bootstrapping
# Resample d18Op for each taxon
# Take mean of each resample
# Compile means in data frame

synth_shark <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(shark$d18O, replace = TRUE))) 

synth_gar <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(gar$d18O, replace = TRUE))) 

synth_glyptops <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(glyptops$d18O, replace = TRUE))) 

synth_naomichelys <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(naomichelys$d18O, replace = TRUE))) 

synth_crocG <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(crocG$d18O, replace = TRUE))) 

synth_crocA <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(crocA$d18O, replace = TRUE))) 

synth_crocB <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(crocB$d18O, replace = TRUE))) 

synth_NIST <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(NIST120c$d.18O.16O, replace = TRUE))) 


# Combine data frames into one
combined_data <- rbind(
  data.frame(group = "Gar", values = synth_gar$means),
  data.frame(group = "Sharks", values = synth_shark$means),
  data.frame(group = "Glyptops", values = synth_glyptops$means),
  data.frame(group = "Naomichelys", values = synth_glyptops$means),
  data.frame(group = "Neosuchian G", values = synth_crocG$means),
  data.frame(group = "Neosuchian A", values = synth_crocA$means),
  data.frame(group = "Neosuchian B", values = synth_crocB$means),
  data.frame(group = "NIST", values = synth_NIST$means)
)

# Set order of facets
combined_data$group <- factor(combined_data$group, levels = c("Gar", "Sharks", "Glyptops", "Neosuchian G", "Neosuchian A", "Neosuchian B", "NIST"))

# Plot histogram of resampled means for each taxon
ggplot(combined_data, aes(x = values, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30, color = "black") +
  labs(title = "Histogram of Means",
       x = "Means",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~group, scales = "free")


# CALCULATE WATER

# Create data frame to store MC results
    
    # Determine the number of unique values in V1075_MCdata$Taxon
    num_unique <- length(unique(V1075_MCdata$Taxon))
    
    # Create data frame to store MC results with the correct number of rows
    V1075_MCwater <- data.frame(Taxon = character(num_unique))
    V1075_MCwater$Taxon <- unique(V1075_MCdata$Taxon)    
    V1075_MCwater$MEAN <- NA
    V1075_MCwater$hiCL <- NA
    V1075_MCwater$loCL <- NA

# Glyptops ------------------------------------------------------------------


    # create function
      turtlewater <- function(synthmeans_turtle){
        1.01 *(synthmeans_turtle) - 22.3 #Barrick et al. (1999)
      }
  
    # run turtlewater on synth_turtle
      synth_glyptops <- synth_glyptops %>%
        mutate(d18Owater = turtlewater(means))
  
      # CI, mean of turtle d18Owater
      CIsetup_water <- sort(synth_glyptops$d18Owater)
      str(CIsetup_water)
      
      # Calculate the lower and upper percentiles for the middle 95%
      lower_percentile_water <- quantile(CIsetup_water, 0.025)
      upper_percentile_water <- quantile(CIsetup_water, 0.975)
      
      # Subset the middle 95% of the data
      subset_CIsetup_water <- CIsetup_water[CIsetup_water >= lower_percentile_water & CIsetup_water <= upper_percentile_water]
      
      # Take mean
      mean_turtlewater <- round(mean(subset_CIsetup_water), 1)
      lowCI_water <- round(abs(mean_turtlewater - lower_percentile_water), 1)
      highCI_water <- round(abs(mean_turtlewater - upper_percentile_water), 1)
      cat(mean_turtlewater, "+", highCI_water, "/", "-", lowCI_water)
    
      # store results
      V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Glyptops sp.")] <- mean_turtlewater
      V1075_MCwater$hiCL[which(V1075_MCwater$Taxon == "Glyptops sp.")] <- upper_percentile_water
      V1075_MCwater$loCL[which(V1075_MCwater$Taxon == "Glyptops sp.")] <- lower_percentile_water
      
# Naomichelys ------------------------------------------------------------------
      
      
      # create function
      turtlewater <- function(synthmeans_turtle){
        1.01 *(synthmeans_turtle) - 22.3 #Barrick et al. (1999)
      }
      
      # run turtlewater on synth_turtle
      synth_naomichelys <- synth_naomichelys %>%
        mutate(d18Owater = turtlewater(means))
      
      # CI, mean of turtle d18Owater
      CIsetup_water <- sort(synth_naomichelys$d18Owater)
      str(CIsetup_water)
      
      # Calculate the lower and upper percentiles for the middle 95%
      lower_percentile_water <- quantile(CIsetup_water, 0.025)
      upper_percentile_water <- quantile(CIsetup_water, 0.975)
      
      # Subset the middle 95% of the data
      subset_CIsetup_water <- CIsetup_water[CIsetup_water >= lower_percentile_water & CIsetup_water <= upper_percentile_water]
      
      # Take mean
      mean_naomichelys <- round(mean(subset_CIsetup_water), 1)
      lowCI_water <- round(abs(mean_naomichelys - lower_percentile_water), 1)
      highCI_water <- round(abs(mean_naomichelys - upper_percentile_water), 1)
      cat(mean_naomichelys, "+", highCI_water, "/", "-", lowCI_water)
      
      # store results
      V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Naomichelys sp.")] <- mean_naomichelys
      V1075_MCwater$hiCL[which(V1075_MCwater$Taxon == "Naomichelys sp.")] <- upper_percentile_water
      V1075_MCwater$loCL[which(V1075_MCwater$Taxon == "Naomichelys sp.")] <- lower_percentile_water
      
  
# Neosuchian G ------------------------------------------------------------------


      # create function
      crocwater <- function(synthmeans_croc){
        0.82*(synthmeans_croc) - 19.93 #Amiot et al.(2007)
      }
      
      # run crocwater on synth_croc
      synth_crocG <- synth_crocG %>%
        mutate(d18Owater = crocwater(means))
      
      # CI, mean of croc d18Owater
      CIsetup_water <- sort(synth_crocG$d18Owater)
      str(CIsetup_water)
      
      # Calculate the lower and upper percentiles for the middle 95%
      lower_percentile_water <- quantile(CIsetup_water, 0.025)
      upper_percentile_water <- quantile(CIsetup_water, 0.975)
      
      # Subset the middle 95% of the data
      subset_CIsetup_water <- CIsetup_water[CIsetup_water >= lower_percentile_water & CIsetup_water <= upper_percentile_water]
      
      # Take mean
      mean_crocGwater <- round(mean(subset_CIsetup_water), 1)
      lowCI_water <- round(abs(mean_crocGwater - lower_percentile_water), 1)
      highCI_water <- round(abs(mean_crocGwater - upper_percentile_water), 1)
      cat(mean_crocGwater, "+", highCI_water, "/", "-", lowCI_water)
   
    
      # store results
      V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Neosuchian G")] <- mean_crocGwater
      V1075_MCwater$hiCL[which(V1075_MCwater$Taxon == "Neosuchian G")] <- upper_percentile_water
      V1075_MCwater$loCL[which(V1075_MCwater$Taxon == "Neosuchian G")] <- lower_percentile_water
      
      

# Neosuchian B ------------------------------------------------------------------

      
      # run crocwater on synth_croc
      synth_crocB <- synth_crocB %>%
        mutate(d18Owater = crocwater(means))
      
      # CI, mean of croc d18Owater
      CIsetup_water <- sort(synth_crocB$d18Owater)
      str(CIsetup_water)
      
      # Calculate the lower and upper percentiles for the middle 95%
      lower_percentile_water <- quantile(CIsetup_water, 0.025)
      upper_percentile_water <- quantile(CIsetup_water, 0.975)
      
      # Subset the middle 95% of the data
      subset_CIsetup_water <- CIsetup_water[CIsetup_water >= lower_percentile_water & CIsetup_water <= upper_percentile_water]
      
      # Take mean
      mean_crocBwater <- round(mean(subset_CIsetup_water), 1)
      lowCI_water <- round(abs(mean_crocBwater - lower_percentile_water), 1)
      highCI_water <- round(abs(mean_crocBwater - upper_percentile_water), 1)
      cat(mean_crocBwater, "+", highCI_water, "/", "-", lowCI_water)
      
      # store results
      V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Neosuchian B")] <- mean_crocBwater
      V1075_MCwater$hiCL[which(V1075_MCwater$Taxon == "Neosuchian B")] <- upper_percentile_water
      V1075_MCwater$loCL[which(V1075_MCwater$Taxon == "Neosuchian B")] <- lower_percentile_water
      
      
# Neosuchian "A" ------------------------------------------------------------------
      
      # run crocwater on synth_croc
      synth_crocA <- synth_crocA %>%
        mutate(d18Owater = crocwater(means))
      
      # CI, mean of croc d18Owater
      CIsetup_water <- sort(synth_crocA$d18Owater)
      str(CIsetup_water)
      
      # Calculate the lower and upper percentiles for the middle 95%
      lower_percentile_water <- quantile(CIsetup_water, 0.025)
      upper_percentile_water <- quantile(CIsetup_water, 0.975)
      
      # Subset the middle 95% of the data
      subset_CIsetup_water <- CIsetup_water[CIsetup_water >= lower_percentile_water & CIsetup_water <= upper_percentile_water]
      
      # Take mean
      mean_crocAwater <- round(mean(subset_CIsetup_water), 1)
      lowCI_water <- round(abs(mean_crocAwater - lower_percentile_water), 1)
      highCI_water <- round(abs(mean_crocAwater - upper_percentile_water), 1)
      cat(mean_crocAwater, "+", highCI_water, "/", "-", lowCI_water)
      
      # store results
      V1075_MCwater$MEAN[which(V1075_MCwater$Taxon == "Neosuchian A")] <- mean_crocAwater
      V1075_MCwater$hiCL[which(V1075_MCwater$Taxon == "Neosuchian A")] <- upper_percentile_water
      V1075_MCwater$loCL[which(V1075_MCwater$Taxon == "Neosuchian A")] <- lower_percentile_water
      
      
  
# EXPORT d18Owater estimates
    setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
    write.csv(V1075_MCwater, "V1075_MCwater.csv", row.names = FALSE)

    
    
#  Calculate temps -------------------------------------------------------


    # define temp function
    TempFun <- function(d18Ofish, NISTmean, d18Owater) {
      temp <- 118.7 - 4.22*((d18Ofish  +(22.6 - NISTmean)) - d18Owater) 
    }

  # turtlewater
    # run TempFun over all the synthetic means using turtlewater
    synth_temps <- TempFun(d18Ofish = synth_gar$means, 
                           NISTmean = synth_NIST$means,
                           d18Owater = synth_glyptops$d18Owater)
    
    # plot temps
    ggplot(data = data.frame(temperature = synth_temps), aes(x = temperature)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = "Histogram of Temperatures",
           x = "Temperature",
           y = "Frequency")

  # confidence intervals
    
    CIsetup <- sort(synth_temps)
    str(CIsetup)
    
    # Calculate the lower and upper percentiles for the middle 95%
    lower_percentile <- quantile(CIsetup, 0.025)
    upper_percentile <- quantile(CIsetup, 0.975)
    
    # Subset the middle 95% of the data
    subset_CIsetup <- CIsetup[CIsetup >= lower_percentile & CIsetup <= upper_percentile]
    
    # Take mean
    mean_temp <- round(mean(subset_CIsetup), 1)
    lowCI <- round(abs(mean_temp - lower_percentile), 1)
    highCI <- round(abs(mean_temp - upper_percentile), 1)
    cat(mean_temp, "+", highCI, "/", "-", lowCI)

    
    CIfun <- function(data) {
      sample.mean <- mean(data)
      sample.n <- length(data)
      sample.sd <- sd(data)
      sample.se <- sample.sd/sqrt(sample.n)
      alpha = 0.05
      degrees.freedom = sample.n - 1
      t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
      margin.error <- t.score * sample.se
      lower.bound <- sample.mean - margin.error
      upper.bound <- sample.mean + margin.error
      cat("Mean:", mean(data), "\n")
      cat("95% Confidence Interval:", lower.bound,",", upper.bound,"\n")
    }
    
    CIfun(synth_temps)
  
 # should I run temps based on crocwater?

    


