
# Packages and Data -------------------------------------------------------

# Install and load necessary packages
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Code")
source("V1075_d18O.R")

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op")

# Actual data ------------------------------------------------------------------

# Set a seed for reproducibility
set.seed(123)

# Number of subsamples
num_subsamples <- 1000

# Original sample size
n <- length(scales$d18O..VSMOW.)

# Initialize vectors to store results
subsample_sizes <- numeric(num_subsamples)
variances <- numeric(num_subsamples)
standard_errors <- numeric(num_subsamples)
standard_deviations <- numeric(num_subsamples)

# Generate random subsamples, calculate mean, variance, standard error, and standard deviation for each
for (i in 1:num_subsamples) {
  subsample_size <- sample(3:n, 1) # calls sample function to generate one subsample size anywhere from 3 to n
  subsample <- sample(scales$d18O..VSMOW., subsample_size, replace = FALSE) #  creates a random subsample from the vector scales$d18O..VSMOW. with a specified subsample size
  
  mean_value <- mean(subsample)
  variance_value <- var(subsample)
  standard_error_value <- sd(subsample) / sqrt(subsample_size)
  standard_deviation_value <- sd(subsample)
  
  subsample_sizes[i] <- subsample_size
  variances[i] <- variance_value
  standard_errors[i] <- standard_error_value
  standard_deviations[i] <- standard_deviation_value
}

# Create data frames with the results
variance_df <- data.frame(Subsample_Size = subsample_sizes, Variance = variances)
se_df <- data.frame(Subsample_Size = subsample_sizes, Standard_Error = standard_errors)
sd_df <- data.frame(Subsample_Size = subsample_sizes, Standard_Deviation = standard_deviations)

# Plot variance using ggplot with x-axis ticks every 5 units
ggplot(variance_df, aes(x = Subsample_Size, y = Variance)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Variance vs Subsample Size",
       x = "Subsample Size",
       y = "Variance") +
  scale_x_continuous(breaks = seq(0, n, by = 5)) +  # Set x-axis ticks every 5 units
  theme_minimal()

# Plot standard error using ggplot with x-axis ticks every 5 units
ggplot(se_df, aes(x = Subsample_Size, y = Standard_Error)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Standard Error vs Subsample Size",
       x = "Subsample Size",
       y = "Standard Error") +
  scale_x_continuous(breaks = seq(0, n, by = 5)) +  # Set x-axis ticks every 5 units
  theme_minimal()

# Plot standard deviation using ggplot with x-axis ticks every 5 units
ggplot(sd_df, aes(x = Subsample_Size, y = Standard_Deviation)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Standard Deviation vs Subsample Size",
       x = "Subsample Size",
       y = "Standard Deviation") +
  scale_x_continuous(breaks = seq(0, n, by = 5)) +  # Set x-axis ticks every 5 units
  theme_minimal()





# simulated data ----------------------------------------------------------


# Set a seed for reproducibility
set.seed(123)

# Simulate a dataset of 50 values that is normally distributed
simulated_data <- rnorm(50, mean = mean(scales$d18O..VSMOW.), sd = sd(scales$d18O..VSMOW.))

# Number of subsamples
num_subsamples <- 1000

# Initialize vectors to store results
subsample_sizes <- numeric(num_subsamples)
means <- numeric(num_subsamples)
variances <- numeric(num_subsamples)
standard_errors <- numeric(num_subsamples)
standard_deviations <- numeric(num_subsamples)

# Generate random subsamples from the simulated dataset, calculate mean, variance, standard error, and standard deviation for each
for (i in 1:num_subsamples) {
  subsample_size <- sample(3:50, 1)
  subsample <- sample(simulated_data, subsample_size, replace = FALSE)
  
  mean_value <- mean(subsample)
  variance_value <- var(subsample)
  standard_error_value <- sd(subsample) / sqrt(subsample_size)
  standard_deviation_value <- sd(subsample)
  
  subsample_sizes[i] <- subsample_size
  means[i] <- mean_value
  variances[i] <- variance_value
  standard_errors[i] <- standard_error_value
  standard_deviations[i] <- standard_deviation_value
}

# Create data frames with the results
mean_df <- data.frame(Subsample_Size = subsample_sizes, Mean = means)
variance_df <- data.frame(Subsample_Size = subsample_sizes, Variance = variances)
se_df <- data.frame(Subsample_Size = subsample_sizes, Standard_Error = standard_errors)
sd_df <- data.frame(Subsample_Size = subsample_sizes, Standard_Deviation = standard_deviations)

hist(mean_df$Mean, breaks = seq(12, 17, by = 0.1))
# Plot variance using ggplot with x-axis ticks every 5 units
ggplot(variance_df, aes(x = Subsample_Size, y = Variance)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Variance vs Subsample Size (Simulated Data)",
       x = "Subsample Size",
       y = "Variance") +
  scale_x_continuous(breaks = seq(0, 50, by = 5)) +  # Set x-axis ticks every 5 units
  theme_minimal()

# Plot standard error using ggplot with x-axis ticks every 5 units
ggplot(se_df, aes(x = Subsample_Size, y = Standard_Error)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Standard Error vs Subsample Size (Simulated Data)",
       x = "Subsample Size",
       y = "Standard Error") +
  scale_x_continuous(breaks = seq(0, 50, by = 5)) +  # Set x-axis ticks every 5 units
  theme_minimal()

# Plot standard deviation using ggplot with x-axis ticks every 5 units
ggplot(sd_df, aes(x = Subsample_Size, y = Standard_Deviation)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Standard Deviation vs Subsample Size (Simulated Data)",
       x = "Subsample Size",
       y = "Standard Deviation") +
  scale_x_continuous(breaks = seq(0, 50, by = 5)) +  # Set x-axis ticks every 5 units
  theme_minimal()


hist(variance_df$Variance)
hist(se_df$Standard_Error)
hist(sd_df$Standard_Deviation)


#Procedure sensu Pearson and Grove 2013 --------------------------------------

# Ovis Simulation 1

# Data are loaded as a two-column matrix labelled 'data', with column 1 being carbon and column 2 being nitrogen
# No headers

# Setup
x <- scales$d18O..VSMOW. # extract the isotope data from the dataframe

output <- matrix(0, nrow = 50, ncol = 1000)  # create empty output matrices
output1 <- matrix(0, nrow = 50, ncol = 1000)
output2 <- matrix(0, nrow = 48, ncol = 1)
output3 <- matrix(0, nrow = 48, ncol = 1)

# Sampling Loop
for (n in 2:49) {  # initiate main loop. Should be generating sample sizes from 3-50
  for (m in 1:1000) {  # initiate sub loop. This is telling R to run the enclosed simulation 1000 times (bootstrapping n = 1000)
    y <- sample(x, 50, replace = TRUE)  # draw m samples at random from the data with replacement (SHOULD I DISABLE REPLACEMENT?)
    output[n, m] <- mean(y[1:(n + 1)])  # average samples of sizes 3 to 50 from the random sample in y
  }
}

# Test Loop
for (n in 1:49) {  # initiate main loop
  for (m in 1:1000) {  # initiate sub loop
    output1[n, m] <- abs(output[n, m] - output[n + 1, m])  # calculate absolute differences between successive sample sizes
  }
}

# Gather Statistics
for (n in 1:48) {  # initiate loop. Why 48?
  output2[n, 1] <- mean(output1[n, 1:1000])  # calculate mean improvement at each sample size
  output3[n, 1] <- sd(output1[n, 1:1000])  # calculate SD on improvement at each sample size
}

# Assuming output2 and output3 have been computed as per the previous code

# Create a data frame for plotting
plot_data <- data.frame(n = 1:48, mean_improvement = output2[, 1], sd_improvement = output3[, 1])

# Plot with ggplot2
ggplot(plot_data, aes(x = n, y = mean_improvement)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_improvement - sd_improvement, ymax = mean_improvement + sd_improvement), alpha = 0.2) +
  labs(title = "Mean Improvement vs. Sample Size",
       x = "Sample Size",
       y = "Mean Improvement") +
  theme_minimal()

# Ovis simulation 2

# Setup

output1 <- matrix(0, nrow = 1000, ncol = 49)  # create empty output matrices
output2 <- matrix(0, nrow = 1000, ncol = 49)
output3 <- matrix(0, nrow = 1000, ncol = 49)

# Sampling Loop
for (m in 2:50) {  # initiate main loop
  for (n in 1:1000) {  # initiate sub loop
    y <- sample(x, m, replace = TRUE)  # draw m samples at random from the carbon data with replacement
    a <- mean(y)  # calculate the mean of the m samples
    b <- 1.96 * (sd(y) / sqrt(m))  # calculate the 95%CI for the m samples
    output1[n, m - 1] <- a + b  # calculate mean plus 95%CI...
    output2[n, m - 1] <- a - b  # ...and mean minus 95%CI
  }
}

truemean <- median(boot::boot(x, R = 1000, statistic = function(x, i) mean(x[i]))$t)  # calculate 'true' mean via bootstrap

# Test Loop
output3 <- matrix(0, nrow = 1000, ncol = 49)  # recreate empty output matrix
for (m in 1:1000) {  # initiate main loop
  for (n in 1:49) {  # initiate sub loop
    if (truemean <= output1[m, n] & truemean >= output2[m, n]) {  # initiate conditional loop
      output3[m, n] <- 1  # output 1 if true...
    } else {
      output3[m, n] <- 0  # ...output 0 if false
    }
  }
}

output4 <- matrix(0, nrow = 1, ncol = 49)  # seed empty vector
for (m in 1:49) {  # initiate loop
  output4[1, m] <- sum(output3[, m]) / 1000  # calculate proportion of true values in each column of output3
}

# Create a data frame for plotting
plot_data <- data.frame(n = 1:49, proportion = output4[1,])

# Plot with ggplot2
ggplot(plot_data, aes(x = n, y = proportion)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # "lm" stands for linear model
  labs(title = "Proportion of Estimates Containing True Mean vs. Sample Size",
       x = "Sample Size",
       y = "Proportion") +
  theme_minimal()


# Multi-taxon Models --------------------------------------------------------

# Need to source V1075_BySpec from AllOxyCompare.R
# First, omit Large Theropod (only one specimen)
V1075_BySpec <- subset(V1075_BySpec, eco_type != "Large Theropod")

# Assuming 'x' is your d18O values and 'Eco' is your grouping variable
x <- V1075_BySpec$d18O
groups <- V1075_BySpec$eco_type

# Create an empty list to store results for each group
results_list <- list()

# Loop over each Eco group
for (group in unique(groups)) {
  # Subset the data for the current group
  group_data <- x[groups == group]
  
  # Ovis Simulation 1
  output <- matrix(0, nrow = 50, ncol = 1000)
  output1 <- matrix(0, nrow = 50, ncol = 1000)
  output2 <- matrix(0, nrow = 48, ncol = 1)
  output3 <- matrix(0, nrow = 48, ncol = 1)
  
  for (n in 1:49) {
    for (m in 1:1000) {
      y <- sample(group_data, 50, replace = TRUE)
      output[n, m] <- mean(y[1:(n + 1)])
    }
  }
  
  for (n in 1:49) {
    for (m in 1:1000) {
      output1[n, m] <- abs(output[n, m] - output[n + 1, m])
    }
  }
  
  for (n in 1:48) {
    output2[n, 1] <- mean(output1[n, 1:1000])
    output3[n, 1] <- sd(output1[n, 1:1000])
  }
  
  # Store results in a data frame
  plot_data <- data.frame(
    Group = rep(group, 48),
    n = 1:48,
    mean_improvement = output2[, 1],
    sd_improvement = output3[, 1]
  )
  
  # Add results to the list
  results_list[[group]] <- plot_data
}

# Combine results for all groups into a single data frame
combined_results <- do.call(rbind, results_list)

# Plot with ggplot2
library(ggplot2)
ggplot(combined_results, aes(x = n, y = mean_improvement, color = Group, linetype = Group)) +
  geom_line() +
  labs(title = "Mean Improvement vs. Sample Size by Eco Group",
       x = "Sample Size",
       y = "Mean Improvement (‰)",
       color = "Eco Group",
       linetype = "Eco Group") +
  theme_minimal()



# OVIS 2
  # Load necessary libraries
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  library(dplyr)

  # Create an empty list to store results for each group
  results_list <- list()
  
  # Loop over each Eco group
  for (group in unique(groups)) {
    # Subset the data for the current group
    group_data <- x[groups == group]
    
    # Ovis Simulation 2
    output1 <- matrix(0, nrow = 1000, ncol = 49)
    output2 <- matrix(0, nrow = 1000, ncol = 49)
    output3 <- matrix(0, nrow = 1000, ncol = 49)
    
    for (m in 2:50) {
      for (n in 1:1000) {
        y <- sample(group_data, m, replace = TRUE)
        a <- mean(y)
        b <- 1.96 * (sd(y) / sqrt(m))
        output1[n, m - 1] <- a + b
        output2[n, m - 1] <- a - b
      }
    }
    
    truemean <- median(boot::boot(group_data, R = 1000, statistic = function(x, i) mean(x[i]))$t)
    
    output3 <- matrix(0, nrow = 1000, ncol = 49)
    for (m in 1:1000) {
      for (n in 1:49) {
        if (truemean <= output1[m, n] & truemean >= output2[m, n]) {
          output3[m, n] <- 1
        } else {
          output3[m, n] <- 0
        }
      }
    }
    
    output4 <- matrix(0, nrow = 1, ncol = 49)
    for (m in 1:49) {
      output4[1, m] <- sum(output3[, m]) / 1000
    }
    
    # Store results in a data frame
    plot_data <- data.frame(
      Group = rep(group, 49),
      n = 1:49,
      proportion = output4[1, ]
    )
    
    # Add results to the list
    results_list[[group]] <- plot_data
  }

  # Combine results for all groups into a single data frame
    combined_results <- bind_rows(results_list)
  
  # Plot with ggplot2
    library(ggplot2)
    ggplot(combined_results, aes(x = n, y = proportion, color = Group, linetype = Group)) +
      geom_line() +
      labs(title = "Proportion of Estimates Containing True Mean vs. Sample Size by Eco Group",
           x = "Sample Size",
           y = "Proportion",
           color = "Eco Group",
           linetype = "Eco Group") +
      theme_minimal()


# Histograms --------------------------------------------------------------

# Assuming 'x' is your d18O values and 'Eco' is your grouping variable
x <- V1075_cl$d18O..VSMOW.
groups <- V1075_cl$Eco

# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
library(ggplot2)

# Create an empty list to store results for each group
results_list <- list()

# Loop over each Eco group
for (group in unique(groups)) {
  # Subset the data for the current group
  group_data <- x[groups == group]
  
  # Ovis Simulation 1
  output <- matrix(0, nrow = 50, ncol = 1000)
  
  for (n in 1:49) {
    for (m in 1:1000) {
      y <- sample(group_data, 50, replace = TRUE)
      output[n, m] <- mean(y[1:(n + 1)])
    }
  }
  
  # Store results in a data frame
  plot_data <- data.frame(
    Group = rep(group, each = 49 * 1000),
    n = rep(1:49, times = 1000),
    mean_improvement = as.vector(output)
  )
  
  # Add results to the list
  results_list[[group]] <- plot_data
}

# Combine results for all groups into a single data frame
combined_results <- bind_rows(results_list)

# Plot histograms with ggplot2
ggplot(combined_results, aes(x = mean_improvement)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~Group, scales = "free") +
  labs(title = "Histograms of Simulated Data by Eco Group",
       x = "Mean Improvement",
       y = "Frequency") +
  theme_minimal()


# Proxy Sensitivity analyses ----------------------------------------------

#calculate d18Omw estimates from random subsamples (n=3 to n=n) of d18Oglyptops, d18OcrocG

# Assuming 'V1075_BySpec' is your data frame and 'd18O' is the variable to be subsampled
# Assuming 'eco_type' is the grouping variable

# Assuming 'V1075_BySpec' is your data frame and 'd18O' is the variable to be subsampled
# Assuming 'eco_type' is the grouping variable


# Assuming 'V1075_BySpec' is your data frame and 'd18O' is the variable to be subsampled
# Assuming 'eco_type' is the grouping variable

# Assuming 'V1075_BySpec' is your data frame and 'd18O' is the variable to be subsampled
# Assuming 'eco_type' is the grouping variable

# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Create an empty list to store results for each eco_type group
results_list <- list()

# Loop over each eco_type group
for (eco_type in unique(V1075_BySpec$eco_type)) {
  # Subset the data for the current eco_type group
  eco_type_data <- V1075_BySpec$d18O[V1075_BySpec$eco_type == eco_type]
  
  # Determine the actual sample size of the current eco_type group
  actual_sample_size <- length(eco_type_data)
  
  # Ovis Simulation (subsample from 3 to actual sample size)
  output <- matrix(0, nrow = actual_sample_size - 2, ncol = 1000)
  
  for (n in 3:actual_sample_size) {
    for (m in 1:1000) {
      y <- sample(eco_type_data, n, replace = TRUE)
      output[n - 2, m] <- mean(y)
    }
  }
  
  # Store results in a data frame
  plot_data <- data.frame(
    Eco_Type = rep(eco_type, each = actual_sample_size - 2),
    Sample_Size = rep(3:(actual_sample_size), each = 1000),
    Mean = as.vector(output)
  )
  
  # Add results to the list
  results_list[[eco_type]] <- plot_data
}

# Combine results for all eco_type groups into a single data frame
combined_results <- bind_rows(results_list)

# Assuming 'combined_results' is your data frame

# Print the number of rows for each Eco_Type group
combined_results %>%
  group_by(Eco_Type) %>%
  summarise(NumRows = n()) %>%
  print()

V1075_BySpec %>%
  group_by(eco_type) %>%
  summarise(NumRows = n()) %>%
  print()

# JUST FISH

# Assuming 'V1075_BySpec' is your data frame and 'd18O' is the variable to be subsampled
# Assuming 'eco_type' is the grouping variable

# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Subset the data for 'eco_type' "Fish"
fish_data <- V1075_BySpec$d18O[V1075_BySpec$eco_type == "Fish"]

# Number of desired subsamples
num_subsamples <- 1000

# Ovis Simulation (subsample from 3 to 17)
output_fish <- matrix(0, nrow = num_subsamples, ncol = 15)

for (n in 3:17) {
  for (m in 1:(num_subsamples/15)) {
    y_fish <- sample(fish_data, n, replace = TRUE)
    output_fish[(m - 1) * 15 + 1:m * 15, n - 2] <- mean(y_fish)
  }
}

# Store results in a data frame
plot_data_fish <- data.frame(
  Eco_Type = rep("Fish", each = num_subsamples),
  Sample_Size = rep(3:17, times = num_subsamples/15),
  Mean = as.vector(output_fish)
)

# Print or use 'plot_data_fish' as needed




# Subset the data for the "Aquatic Turtle" eco_type
aquatic_turtle_data <- combined_results %>%
  filter(Eco_Type == "Aquatic Turtle")

# Apply the function to the subsample means
aquatic_turtle_data$d18Omw_turt <- 1.01 * aquatic_turtle_data$Mean - 22.3

# Print or use 'aquatic_turtle_data' as needed


#calculate temps from d18Oglyptops/d18OcrocG subsamples and d18Oscales subsamples 

t <- subset(combined_results, Eco_Type == "Aquatic Turtle")
nrow(t)

nrow(combined_results)
#calculate 








# calculate ingested/living water d18O from turtle shell
d18Op_turt <- mean(Glyp$d18O..VSMOW, na.rm = TRUE)
se_d18OGlyp <- sd(Glyp$d18O..VSMOW)/sqrt(length(Glyp$d18O..VSMOW))
d18Omw_turt <- 1.01 *(d18Op_turt) - 22.3 #Barrick et al. (1999)
turtwaters <- 1.01 *(Glyp$d18O..VSMOW) - 22.3 #Barrick et al. (1999)
#se_d18Omw <- 1.01 *(se_d18OGlyp)
# turtleTemp <- 111.37 - (4.3 * ( turtled18Op - turtleWater_d18O)) #Barrick et al. (1999) (I don't think this is used correctly. Temp estimate is erroneous)

summary(turtwaters)

# calculate living water temperature of ganoid scales from turtled18Omw:
scales <- Gar[which(Gar$Element.type %in% "ganoid scale"), ]
d18Op_fish <- mean(scales$d18O..VSMOW., na.rm = TRUE)
#fishTemp <- 111.37 - 4.3*(fishd18Op - turtleWater_d18O )#Longinelli and Nuti (1973), Kolodny et al. (1983)
TempFishTurt <- 118.7 - 4.22*((d18Op_fish  +(22.6 - NIST120c_mean))- d18Omw_turt ) #Puceat et al. (2010)
# SEtemp <- sqrt((GarSE^2)+(se_d18Omw^2))

# calculate ingested/living water d18O from croc enamel
d18Op_crocA <- mean(CrocA$d18O..VSMOW., na.rm = TRUE)
d18Ow_crocA <- 0.82*(d18Op_crocA) - 19.93 #Amiot et al.(2007)
#TempFishCrocA <- 118.7 - 4.22*((fishd18Op  +(22.6 - NIST120c_mean)) - d18Ow_crocA ) #Puceat et al. (2010)

d18Op_crocB <- mean(Berni$d18O..VSMOW., na.rm = TRUE)
d18Omw_crocB <- 0.82*(d18Op_crocB) - 19.93 #Amiot et al.(2007)
#TempFishCrocB <- 118.7 - 4.22*((fishd18Op  +(22.6 - NIST120c_mean)) - d18Ow_crocB ) #Puceat et al. (2010)

d18Op_crocG <- mean(Goni$d18O..VSMOW., na.rm = TRUE)
d18Omw_crocG <- 0.82*(d18Op_crocG) - 19.93 #Amiot et al.(2007)
#TempFishCrocG <- 118.7 - 4.22*((fishd18Op  +(22.6 - NIST120c_mean)) - d18Omw_crocG ) #Puceat et al. (2010)

# calculate ingested/living water temperature of ganoid scales from turtled18Omw:
TempFishCrocG <- 118.7 - 4.22*((d18Op_fish  +(22.6 - NIST120c_mean))- d18Omw_crocG ) #Puceat et al. (2010)

# use Tim Cullen's ectotherm-endotherm combined mean approach to estimate temp (modified from Fricke and Wing, 2005)

# calc d18Omw from each endothermic taxon
# d18Op <- (0.76) * (d18Omw) + 19.94
# d18Omw <- (d18Op - 19.94)/(0.76)

# Compute mean d18Omw for endotherms 
mean_d18Omw <- V1075_cl %>%
  group_by(thermophysiology) %>%
  summarize(mean_d18Omw = mean((d18O..VSMOW. - 19.94) / 0.76, na.rm = TRUE))

d18Ow_endo <- mean_d18Omw[mean_d18Omw$thermophysiology == "endotherm", ]
d18Ow_endo <- d18Ow_endo$mean_d18Omw

# Calculate mean d18Op of ectotherms
d18Op_ecto <- V1075_cl %>%
  filter(thermophysiology == "ectotherm") %>%
  summarize(mean_d18Op = mean(d18O..VSMOW., na.rm = TRUE))
d18Op_ecto <- d18Op_ecto$mean_d18Op

# Compute Endotherm-Ectotherm Combined Mean temperature estimate
EECM <- 111.4 - ( 4.3 * ( (d18Op_ecto) - (d18Ow_endo) ))
paste("EECM Temp estimate:", sprintf("%.2f", EECM), "degrees C")

