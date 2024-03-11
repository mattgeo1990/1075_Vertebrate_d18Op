
# Packages


library(dplyr)
library(purrr)
library(ggplot2)
library(RCurl)

# Data
# Call d18Op data from GitHub, cleaned and averaged per specimen (V1075_BySpec.csv)
samples_githubURL <- getURL("https://raw.githubusercontent.com/mattgeo1990/1075_Vertebrate_d18Op/main/Data/V1075_GarTurtle")
V1075_GarTurtle <- read.csv(text = samples_githubURL)

# READ IN NIST120c STD d18Op VALUES FROM RUN 1 and RUN 2
# NEED NIST120C STD d18Op from Run 3!!!!!
standards_githubURL <- getURL("https://raw.githubusercontent.com/mattgeo1990/1075_Vertebrate_d18Op/main/Data/V1075_NIST120c.csv")
NIST120c <- read.csv(text = standards_githubURL)


setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
V1075_GarTurtleCroc <- read.csv("V1075_GarTurtleCroc.csv")
NIST120c <- read.csv("V1075_NIST120c.csv")

# Set number of Monte Carlo repetitions 
nMCrepetitions <- 1e5

# Subset V1075_GarTurtle into gar, turtle, and croc matrices
gar <- subset(V1075_GarTurtleCroc, eco_type == "Fish")
turtle <- subset(V1075_GarTurtleCroc, eco_type == "Aquatic Turtle")
croc <- subset(V1075_GarTurtleCroc, eco_type == "Croc G")

# resample

synth_gar <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(gar$d18O, replace = TRUE))) 

synth_turtle <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(turtle$d18O, replace = TRUE))) 

synth_croc <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(croc$d18O, replace = TRUE))) 

synth_NIST <- data_frame(num = 1:nMCrepetitions) %>% 
  group_by(num) %>% 
  mutate(means = mean(sample(NIST120c$d.18O.16O, replace = TRUE))) 


# Combine data frames into one
combined_data <- rbind(
  data.frame(group = "Gar", values = synth_gar$means),
  data.frame(group = "Turtle", values = synth_turtle$means),
  data.frame(group = "Croc G", values = synth_turtle$means),
  data.frame(group = "NIST", values = synth_NIST$means)
)

# Set order of facets
combined_data$group <- factor(combined_data$group, levels = c("Gar", "Turtle", "Croc G", "NIST"))

# Plotting with ggplot2 using facet_wrap
ggplot(combined_data, aes(x = values, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30, color = "black") +
  labs(title = "Histogram of Means",
       x = "Means",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~group, scales = "free")



# calculate water

  # turtle 
    # create function
      turtlewater <- function(synthmeans_turtle){
        1.01 *(synthmeans_turtle) - 22.3 #Barrick et al. (1999)
      }
  
    # run turtlewater on synth_turtle
      synth_turtle <- synth_turtle %>%
        mutate(d18Owater = turtlewater(means))
  
      # CI, mean of turtle d18Owater
      CIsetup_water <- sort(synth_turtle$d18Owater)
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
    
  # Croc
      # create function
      crocwater <- function(synthmeans_croc){
        0.82*(synthmeans_croc) - 19.93 #Amiot et al.(2007)
      }
      
      # run crocwater on synth_croc
      synth_croc <- synth_croc %>%
        mutate(d18Owater = crocwater(means))
      
      # CI, mean of croc d18Owater
      CIsetup_water <- sort(synth_croc$d18Owater)
      str(CIsetup_water)
      
      # Calculate the lower and upper percentiles for the middle 95%
      lower_percentile_water <- quantile(CIsetup_water, 0.025)
      upper_percentile_water <- quantile(CIsetup_water, 0.975)
      
      # Subset the middle 95% of the data
      subset_CIsetup_water <- CIsetup_water[CIsetup_water >= lower_percentile_water & CIsetup_water <= upper_percentile_water]
      
      # Take mean
      mean_crocwater <- round(mean(subset_CIsetup_water), 1)
      lowCI_water <- round(abs(mean_crocwater - lower_percentile_water), 1)
      highCI_water <- round(abs(mean_crocwater - upper_percentile_water), 1)
      cat(mean_crocwater, "+", highCI_water, "/", "-", lowCI_water)
   
  # t-test on water distributions from turtle and croc
      # Perform t-test
      t_test_result <- t.test(synth_turtle$d18Owater, synth_croc$d18Owater)
      
      # Print the result
      print(t_test_result)
    
    
# calculate temps
    # define temp function
    TempFun <- function(d18Ofish, NISTmean, d18Owater) {
      temp <- 118.7 - 4.22*((d18Ofish  +(22.6 - NISTmean)) - d18Owater) 
    }

  # turtlewater
    # run TempFun over all the synthetic means using turtlewater
    synth_temps <- TempFun(d18Ofish = synth_gar$means, 
                           NISTmean = synth_NIST$means,
                           d18Owater = synth_turtle$d18Owater)
    
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

    