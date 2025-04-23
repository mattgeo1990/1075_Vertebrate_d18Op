
# Data, Packages ----------------------------------------------------------


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

# Load Barrick regression model (from ~/Documents/GitHub/1075_Vertebrate_d18Op/Code/d18Ow_Proxy_Regressions.R)
Barrick_lm_model <- readRDS("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/Barrick_reg_lm.rds")

# Load Amiot regression model (from ~/Documents/GitHub/1075_Vertebrate_d18Op/Code/d18Ow_Proxy_Regressions.R)
Amiot_lm_model <- readRDS("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/Amiot_reg_lm.rds")

# Load Puceat, Longinelli, Nuti (PLN) regression model (from ~/Documents/GitHub/1075_Vertebrate_d18Op/Code/d18Ow_Proxy_Regressions.R)
PLN_lm_model <- readRDS("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/PLNd18Op_reg_lm.rds")

# Load Tw~Ta transform model (from )
TwTa_lm_model <- readRDS("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/TwTa_reg_lm.rds")





summary(Amiot_lm_model)
summary(PLN_lm_model)
summary(TwTa_lm_model)
coef(PLN_lm_model)[1]


# Monte Carlo Parameters --------------------------------------------------

# Set number of Monte Carlo repetitions 
nMCrepetitions <- 1e3

# d18Op bootstrapping -----------------------------------------------------

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
  data.frame(group = "Naomichelys", values = synth_naomichelys$means),
  data.frame(group = "Neosuchian G", values = synth_crocG$means),
  data.frame(group = "Neosuchian A", values = synth_crocA$means),
  data.frame(group = "Neosuchian B", values = synth_crocB$means),
  data.frame(group = "NIST", values = synth_NIST$means)
)


# Set order of facets
combined_data$group <- factor(combined_data$group, levels = c("Gar", "Sharks", "Glyptops", "Neosuchian G", "Neosuchian A", "Neosuchian B", "Naomichelys", "NIST"))

# Plot histogram of resampled means for each taxon
ggplot(combined_data, aes(x = values, fill = group)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 30, color = "black") +
  labs(title = "Histogram of Means",
       x = "Means",
       y = "Frequency") +
  theme_minimal() +
  facet_wrap(~group, scales = "free")


# Export simulated d18Op distributions

write.csv(combined_data, file = "/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/V1075_MC_d18Op_dist.csv", row.names = FALSE)

#Evaluate effect of n_nMCrepetitions
# Compute 95% CI
nrow(synth_gar)
meangar <- mean(synth_gar$means)
lower_95_CI <- quantile(synth_gar$means, probs = 0.025)  # Lower limit
upper_95_CI <- quantile(synth_gar$means, probs = 0.975)  # Upper limit

# Print results
cat("Mean simulated gar d18Op:" , meangar, "]\n")
cat("95% CI for sim gar d18Op: [", lower_95_CI, ",", upper_95_CI, "]\n")

# Neosuchian G --------------------------------------------------------------

# Extract the regression coefficients and standard errors
Amiot_model_summary <- summary(Amiot_lm_model)
Amiot_intercept <- coef(Amiot_lm_model)[1]
Amiot_slope <- coef(Amiot_lm_model)[2]
Amiot_intercept_se <- coef(Amiot_model_summary)[1, "Std. Error"]  # Standard error for intercept
Amiot_slope_se <- coef(Amiot_model_summary)[2, "Std. Error"]      # Standard error for slope

cat("Standard Error for Intercept:", Amiot_intercept_se, "\n")
cat("Standard Error for Slope:", Amiot_slope_se, "\n")

# Simulate regression coefficients for the regression model
set.seed(123)  # For reproducibility
n_iterations <- 1e3  # Number of Monte Carlo simulations

# Define 
Amiot_residual_sd <- summary(Amiot_lm_model)$sigma

# Generate simulated slope and intercept values
Amiot_simulated_slope <- rnorm(n_iterations, mean = Amiot_slope, sd = Amiot_slope_se)
Amiot_simulated_intercept <- rnorm(n_iterations, mean = Amiot_intercept, sd = Amiot_intercept_se)

# Monte Carlo simulation for the regression
# simulate residual error
Amiot_residual_error <- rnorm(n_iterations, mean = 0, sd = Amiot_residual_sd)

# Store Croc G d18Op synth means in vector
crocG_synthmeans_d18Op <- synth_crocG$means
mean(crocG_synthmeans_d18Op)

# Expand d18Op values to interact with all simulated parameters
expanded_d18Op <- rep(crocG_synthmeans_d18Op, each = n_iterations)
expanded_slopes <- rep(Amiot_simulated_slope, times = length(crocG_synthmeans_d18Op))
expanded_intercepts <- rep(Amiot_simulated_intercept, times = length(crocG_synthmeans_d18Op))
expanded_residuals <- rep(Amiot_residual_error, times = length(crocG_synthmeans_d18Op))

# Calculate water simulations
crocG_water_simulations <- expanded_slopes * expanded_d18Op + expanded_intercepts + expanded_residuals
mean(crocG_water_simulations)

# Sort the simulated water values
sorted_crocG_synthd18Owater <- sort(crocG_water_simulations)

# Calculate 95% CI percentiles
crocGwatersynth_lower_95_CI <- quantile(sorted_crocG_synthd18Owater, probs = 0.025)
crocGwatersynth_upper_95_CI <- quantile(sorted_crocG_synthd18Owater, probs = 0.975)

# Compute the mean of the data
mean_crocGwater_synth <- mean(sorted_crocG_synthd18Owater)

# Print the results
cat("Mean crocG d18Owater:", round(mean_crocGwater_synth, 2), "\n")
cat("95% CI for crocG d18Owater: [", round(crocGwatersynth_lower_95_CI, 2), ",", round(crocGwatersynth_upper_95_CI, 2), "]\n")

# Neosuchian A --------------------------------------------------------------

# Simulate regression coefficients for the regression model
set.seed(123)  # For reproducibility
n_iterations <- 1e3  # Number of Monte Carlo simulations

# Store Croc A d18Op synth means in vector
crocA_synthmeans_d18Op <- synth_crocA$means
mean(crocA_synthmeans_d18Op)

# Expand d18Op values to interact with all simulated parameters
expanded_d18Op <- rep(crocA_synthmeans_d18Op, each = n_iterations)
expanded_slopes <- rep(Amiot_simulated_slope, times = length(crocA_synthmeans_d18Op))
expanded_intercepts <- rep(Amiot_simulated_intercept, times = length(crocA_synthmeans_d18Op))
expanded_residuals <- rep(Amiot_residual_error, times = length(crocA_synthmeans_d18Op))

# Calculate water simulations
crocA_water_simulations <- expanded_slopes * expanded_d18Op + expanded_intercepts + expanded_residuals
mean(crocA_water_simulations)

# Sort the simulated water values
sorted_crocA_synthd18Owater <- sort(crocA_water_simulations)

# Calculate 95% CI percentiles
crocAwatersynth_lower_95_CI <- quantile(sorted_crocA_synthd18Owater, probs = 0.025)
crocAwatersynth_upper_95_CI <- quantile(sorted_crocA_synthd18Owater, probs = 0.975)

# Compute the mean of the data
mean_crocAwater_synth <- mean(sorted_crocA_synthd18Owater)

# Print the results
cat("Mean crocA d18Owater:", round(mean_crocAwater_synth, 2), "\n")
cat("95% CI for crocA d18Owater: [", round(crocAwatersynth_lower_95_CI, 2), ",", round(crocAwatersynth_upper_95_CI, 2), "]\n")

summary(crocA_water_simulations)

# Neosuchian B --------------------------------------------------------------

# Simulate regression coefficients for the regression model
set.seed(123)  # For reproducibility
n_iterations <- 1e3  # Number of Monte Carlo simulations

# Store Croc B d18Op synth means in vector
crocB_synthmeans_d18Op <- synth_crocB$means
mean(crocB_synthmeans_d18Op)

# Expand d18Op values to interact with all simulated parameters
expanded_d18Op <- rep(crocB_synthmeans_d18Op, each = n_iterations)
expanded_slopes <- rep(Amiot_simulated_slope, times = length(crocB_synthmeans_d18Op))
expanded_intercepts <- rep(Amiot_simulated_intercept, times = length(crocB_synthmeans_d18Op))
expanded_residuals <- rep(Amiot_residual_error, times = length(crocB_synthmeans_d18Op))

# Calculate water simulations
crocB_water_simulations <- expanded_slopes * expanded_d18Op + expanded_intercepts + expanded_residuals
mean(crocB_water_simulations)

# Sort the simulated water values
sorted_crocB_synthd18Owater <- sort(crocB_water_simulations)

# Calculate 95% CI percentiles
crocBwatersynth_lower_95_CI <- quantile(sorted_crocB_synthd18Owater, probs = 0.025)
crocBwatersynth_upper_95_CI <- quantile(sorted_crocB_synthd18Owater, probs = 0.975)

# Compute the mean of the data
mean_crocBwater_synth <- mean(sorted_crocB_synthd18Owater)

# Print the results
cat("Mean crocB d18Owater:", round(mean_crocBwater_synth, 2), "\n")
cat("95% CI for crocB d18Owater: [", round(crocBwatersynth_lower_95_CI, 2), ",", round(crocBwatersynth_upper_95_CI, 2), "]\n")

summary(crocB_water_simulations)

## Error Analysis for crocwater --------------------------------------------

# Set seed for reproducibility
set.seed(123)

# Number of Monte Carlo iterations
n_iterations <- 1e3

# Input variables
slope <- Amiot_simulated_slope 
intercept <- Amiot_simulated_intercept
residual_error <- Amiot_residual_error
synth_mean <- synth_crocG$means

# Define the model function
crocwater_model <- function(slope, intercept, residual_error, synth_mean) {
  return(slope * synth_mean + intercept + residual_error)
}

# Simulate outputs varying all inputs
output <- crocwater_model(slope, intercept, residual_error, synth_mean)

# Simulate outputs varying one input at a time
mean_slope <- mean(slope)
mean_intercept <- mean(intercept)
mean_residual <- mean(residual_error)
mean_synth <- mean(synth_mean)

output_slope <- crocwater_model(slope, mean_intercept, mean_residual, mean_synth)
output_intercept <- crocwater_model(mean_slope, intercept, mean_residual, mean_synth)
output_residual <- crocwater_model(mean_slope, mean_intercept, residual_error, mean_synth)
output_synth <- crocwater_model(mean_slope, mean_intercept, mean_residual, synth_mean)

# Calculate variances
total_variance <- var(output)
var_slope <- var(output_slope)
var_intercept <- var(output_intercept)
var_residual <- var(output_residual)
var_synth <- var(output_synth)

# Normalize contributions
slope_contribution <- var_slope / total_variance
intercept_contribution <- var_intercept / total_variance
residual_contribution <- var_residual / total_variance
synth_contribution <- var_synth / total_variance

# Display results
cat("Variance Contributions:\n")
cat("Slope:", round(slope_contribution, 3), "\n")
cat("Intercept:", round(intercept_contribution, 3), "\n")
cat("Residual Error:", round(residual_contribution, 3), "\n")
cat("Synth Mean:", round(synth_contribution, 3), "\n")

# Visualize contributions with a bar plot
library(ggplot2)

# Create a data frame for plotting
contributions <- data.frame(
  Input = c("Slope", "Intercept", "Residual Error", "Synth Mean"),
  Contribution = c(slope_contribution, intercept_contribution, residual_contribution, synth_contribution)
)

# Plot
ggplot(contributions, aes(x = reorder(Input, -Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variance Contributions to crocwater",
       x = "Input Variable",
       y = "Proportion of Variance") +
  coord_flip()


# Glyptops ---------------------------------------------------------

# Extract the regression coefficients and standard errors
Barrick_model_summary <- summary(Barrick_lm_model)
Barrick_intercept <- coef(Barrick_lm_model)[1]
Barrick_slope <- coef(Barrick_lm_model)[2]
Barrick_intercept_se <- coef(Barrick_model_summary)[1, "Std. Error"]  # Standard error for intercept
Barrick_slope_se <- coef(Barrick_model_summary)[2, "Std. Error"]      # Standard error for slope

cat("Standard Error for Intercept:", Barrick_intercept_se, "\n")
cat("Standard Error for Slope:", Barrick_slope_se, "\n")

# Simulate regression coefficients for the Barrick regression model
set.seed(123)  # For reproducibility
n_iterations <- 1000  # Number of Monte Carlo simulations

# Define 
Barrick_residual_sd <- summary(Barrick_lm_model)$sigma

# Generate simulated slope and intercept values
Barrick_simulated_slope <- rnorm(n_iterations, mean = Barrick_slope, sd = Barrick_slope_se)
Barrick_simulated_intercept <- rnorm(n_iterations, mean = Barrick_intercept, sd = Barrick_intercept_se)

# Monte Carlo simulation for the regression
# simulate residual error
Barrick_residual_error <- rnorm(n_iterations, mean = 0, sd = Barrick_residual_sd)

# Store Glyptops d18Op synth means in vector
glyp_synthmeans_d18Op <- synth_glyptops$means
mean(glyp_synthmeans_d18Op)

# Expand d18Op values to interact with all simulated parameters
glyp_expanded_d18Op <- rep(glyp_synthmeans_d18Op, each = n_iterations)
glyp_expanded_slopes <- rep(Barrick_simulated_slope, times = length(glyp_synthmeans_d18Op))
glyp_expanded_intercepts <- rep(Barrick_simulated_intercept, times = length(glyp_synthmeans_d18Op))
glyp_expanded_residuals <- rep(Barrick_residual_error, times = length(glyp_synthmeans_d18Op))

# Calculate water simulations
glyp_water_simulations <- glyp_expanded_slopes * glyp_expanded_d18Op + glyp_expanded_intercepts + glyp_expanded_residuals
mean(glyp_water_simulations)

# Sort the simulated water values
glyp_sorted_synthwater <- sort(glyp_water_simulations)

# Calculate 95% CI percentiles
glypwatersynth_lower_95_CI <- quantile(glyp_sorted_synthwater, probs = 0.025)
glypwatersynth_upper_95_CI <- quantile(glyp_sorted_synthwater, probs = 0.975)

# Compute the mean of the middle 95% of the data
mean_glyp_synthwater <- mean(glyp_sorted_synthwater)

# Print the results
cat("Mean Glyptops d18Owater:", round(mean_glyp_synthwater, 2), "\n")
cat("95% CI for Glyptops d18Owater: [", round(glypwatersynth_lower_95_CI, 2), ",", round(glypwatersynth_upper_95_CI, 2), "]\n")

t.test(glyp_water_simulations, crocG_water_simulations, var.equal = FALSE)

# Naomichelys ---------------------------------------------------------


# Simulate regression coefficients for the Barrick regression model
set.seed(123)  # For reproducibility
n_iterations <- 1e3  # Number of Monte Carlo simulations

# Monte Carlo simulation for the regression
# Store Naomichelys d18Op synth means in vector
naomi_synthmeans_d18Op <- synth_naomichelys$means
mean(naomi_synthmeans_d18Op)

# Expand d18Op values to interact with all simulated parameters
expanded_d18Op <- rep(naomi_synthmeans_d18Op, each = n_iterations)
expanded_slopes <- rep(Barrick_simulated_slope, times = length(naomi_synthmeans_d18Op))
expanded_intercepts <- rep(Barrick_simulated_intercept, times = length(naomi_synthmeans_d18Op))
expanded_residuals <- rep(Barrick_residual_error, times = length(naomi_synthmeans_d18Op))

# Calculate water simulations
naomi_water_simulations <- expanded_slopes * expanded_d18Op + expanded_intercepts + expanded_residuals
mean(naomi_water_simulations)

# Sort the simulated water values
naomi_sorted_synthwater <- sort(naomi_water_simulations)

# Calculate 95% CI percentiles
naomiwatersynth_lower_95_CI <- quantile(naomi_sorted_synthwater, probs = 0.025)
naomiwatersynth_upper_95_CI <- quantile(naomi_sorted_synthwater, probs = 0.975)

# Compute the mean of the middle 95% of the data
mean_naomi_synthwater <- mean(naomi_sorted_synthwater)

# Print the results
cat("Mean Naomichelys d18Owater:", round(mean_naomi_synthwater, 2), "\n")
cat("95% CI for Naomichelys d18Owater: [", round(naomiwatersynth_lower_95_CI, 2), ",", round(naomiwatersynth_upper_95_CI, 2), "]\n")

## Error Analysis for Glyptops ----------------------------------------------

# Set seed for reproducibility
set.seed(123)

# Number of Monte Carlo iterations
n_iterations <- 1e3

# Input variables
slope <- Barrick_simulated_slope 
intercept <- Barrick_simulated_intercept
residual_error <- Barrick_residual_error
synth_mean <- synth_glyptops$means

# Define the model function
turtlewater_model <- function(slope, intercept, residual_error, synth_mean) {
  return(slope * synth_mean + intercept + residual_error)
}

# Simulate outputs varying all inputs
output <- turtlewater_model(slope, intercept, residual_error, synth_mean)

# Simulate outputs varying one input at a time
mean_slope <- mean(slope)
mean_intercept <- mean(intercept)
mean_residual <- mean(residual_error)
mean_synth <- mean(synth_mean)

output_slope <- turtlewater_model(slope, mean_intercept, mean_residual, mean_synth)
output_intercept <- turtlewater_model(mean_slope, intercept, mean_residual, mean_synth)
output_residual <- turtlewater_model(mean_slope, mean_intercept, residual_error, mean_synth)
output_synth <- turtlewater_model(mean_slope, mean_intercept, mean_residual, synth_mean)

# Calculate variances
total_variance <- var(output)
var_slope <- var(output_slope)
var_intercept <- var(output_intercept)
var_residual <- var(output_residual)
var_synth <- var(output_synth)

# Normalize contributions
slope_contribution <- var_slope / total_variance
intercept_contribution <- var_intercept / total_variance
residual_contribution <- var_residual / total_variance
synth_contribution <- var_synth / total_variance

# Display results
cat("Variance Contributions:\n")
cat("Slope:", round(slope_contribution, 3), "\n")
cat("Intercept:", round(intercept_contribution, 3), "\n")
cat("Residual Error:", round(residual_contribution, 3), "\n")
cat("Synth Mean:", round(synth_contribution, 3), "\n")

# Visualize contributions with a bar plot
library(ggplot2)

# Create a data frame for plotting
contributions <- data.frame(
  Input = c("Slope", "Intercept", "Residual Error", "Synth Mean"),
  Contribution = c(slope_contribution, intercept_contribution, residual_contribution, synth_contribution)
)

# Plot
ggplot(contributions, aes(x = reorder(Input, -Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variance Contributions to turtlewater",
       x = "Input Variable",
       y = "Proportion of Variance") +
  coord_flip()


sum(slope_contribution, intercept_contribution, residual_contribution, synth_contribution)

# Simulate d18Ow distribution based on crocG and glyptops water estimates ----------------------------------------------------------

# Define bootstrap iterations
n_iterations <- 1e3

# Random sampling and averaging
bootstrapped_d18Ow <- replicate(n_iterations, {
  sample_glyp <- sample(glyp_water_simulations, size = 1, replace = TRUE)
  sample_crocG <- sample(crocG_water_simulations, size = 1, replace = TRUE)
  mean(c(sample_glyp, sample_crocG))  # Average of the two
})

# Analyze combined distribution
mean_d18Ow <- mean(bootstrapped_d18Ow)
sd_d18Ow <- sd(bootstrapped_d18Ow)
quantiles <- quantile(bootstrapped_d18Ow, probs = c(0.025, 0.975))

mean_alld18Owater_synth <- mean_d18Ow
alld18Owater_lower <- quantiles[1]
alld18Owater_upper <- quantiles[2]

# Print results
cat("Mean δ¹⁸Ow:", mean_d18Ow, "\n")
cat("95% CI for δ¹⁸Ow: [", quantiles[1], ",", quantiles[2], "]\n")


# Temp (Puceat) -----------------------------------------------------------

# Extract the regression coefficients and standard errors
PLN_model_summary <- summary(PLN_lm_model)
PLN_intercept <- coef(PLN_lm_model)[1]
PLN_slope <- coef(PLN_lm_model)[2]
PLN_intercept_se <- coef(PLN_model_summary)[1, "Std. Error"]  # Standard error for intercept
PLN_slope_se <- coef(PLN_model_summary)[2, "Std. Error"]      # Standard error for slope

# Set up the number of Monte Carlo iterations
set.seed(123)
n_iterations <- 1e3

# Define input distributions (replace with your actual data)
delta_Op_distribution <- synth_gar$means
delta_Ow_distribution <- bootstrapped_d18Ow
delta_ONBS120c_distribution <- synth_NIST$means

# Sanity checks (CAN REMOVE LATER)
mean(delta_Op_distribution)
mean(delta_Ow_distribution)
mean(delta_ONBS120c_distribution)

# Define regression parameters and their uncertainties (1 sigma errors provided)
intercept <- PLN_intercept
intercept_sd <- PLN_intercept_se
slope <- PLN_slope
slope_sd <- PLN_slope_se

# Simulate regression coefficients using the 1 sigma uncertainties
PLN_intercept_simulated <- rnorm(n_iterations, mean = intercept, sd = intercept_sd)
PLN_slope_simulated <- rnorm(n_iterations, mean = slope, sd = slope_sd)

# Extract residual standard error from your regression model
residual_sd <- summary(PLN_lm_model)$sigma  # Replace `regression_model` with your lm model

# Initialize vector to store results
temperature_simulations <- numeric(n_iterations)

# Perform Monte Carlo simulations
for (i in 1:n_iterations) {
  # Sample one value from each input distribution
  delta_Op <- sample(delta_Op_distribution, 1, replace = TRUE)
  delta_Ow <- sample(delta_Ow_distribution, 1, replace = TRUE)
  delta_ONBS120c <- sample(delta_ONBS120c_distribution, 1, replace = TRUE)
  
  # Simulate regression parameters and residual error
  intercept_sim <- rnorm(1, mean = PLN_intercept, sd = PLN_intercept_se)
  slope_sim <- rnorm(1, mean = PLN_slope, sd = PLN_slope_se)
  residual_error <- rnorm(1, mean = 0, sd = residual_sd)
  
  # Calculate temperature for this iteration
  temperature_simulations[i] <- intercept_sim + slope_sim * (
    delta_Op + (22.6 - delta_ONBS120c) - delta_Ow
  ) + residual_error
}

# Summarize the results
Tw_mean_temperature <- mean(temperature_simulations)
Tw_lower_95_CI <- quantile(temperature_simulations, probs = 0.025)
Tw_upper_95_CI <- quantile(temperature_simulations, probs = 0.975)

# Display results
cat("Mean Temperature (MAWSWT °C):", Tw_mean_temperature, "\n")
cat("95% Confidence Interval (°C): [", Tw_lower_95_CI, ",", Tw_upper_95_CI, "]\n")

# Plot histogram of simulated MAWSWT means???
    # GGPLOT CODE HERE

# Summary of temperature simulations
summary(temperature_simulations)

# Standard deviation
sd_temp_sim <- sd(temperature_simulations)

# Sample size
n <- length(temperature_simulations)

# Standard error of the mean
se_mean_temp_sim <- sd_temp_sim / sqrt(n)
cat("Standard Error of the Mean:", se_mean_temp_sim, "\n")


## Temp Error Analysis ----------------------------------------------------------

# Set up the number of Monte Carlo iterations
n_iterations <- 1e3

# Calculate means of input variables
mean_d18Op <- mean(synth_gar$means)
mean_d18Ow <- mean(bootstrapped_d18Ow)
mean_NIST <- mean(synth_NIST$means)

# Set regression coefficients
intercept_simulated <- PLN_intercept_simulated
slope_simulated <- PLN_slope_simulated

# Extract residual standard error
residual_sd <- summary(PLN_lm_model)$sigma

# Monte Carlo simulations for each variable
# 1. All variables varying
temperature_all <- intercept_simulated + slope_simulated * (
  synth_gar$means + (22.6 - delta_ONBS120c_distribution) - bootstrapped_d18Ow
) + rnorm(n_iterations, mean = 0, sd = residual_sd)

# 2. Hold d18Op constant
temperature_fixed_d18Op <- intercept_simulated + slope_simulated * (
  mean_d18Op + (22.6 - delta_ONBS120c_distribution) - bootstrapped_d18Ow
) + rnorm(n_iterations, mean = 0, sd = residual_sd)

# 3. Hold d18Ow constant
temperature_fixed_d18Ow <- intercept_simulated + slope_simulated * (
  synth_gar$means + (22.6 - delta_ONBS120c_distribution) - mean_d18Ow
) + rnorm(n_iterations, mean = 0, sd = residual_sd)

# 4. Hold NIST constant
temperature_fixed_NIST <- intercept_simulated + slope_simulated * (
  synth_gar$means + (22.6 - mean_NIST) - bootstrapped_d18Ow
) + rnorm(n_iterations, mean = 0, sd = residual_sd)

# 5. Hold residual error constant (no random noise)
temperature_no_residual <- intercept_simulated + slope_simulated * (
  synth_gar$means + (22.6 - delta_ONBS120c_distribution) - bootstrapped_d18Ow
)

# 6. Hold slope constant
temperature_fixed_slope <- mean(slope_simulated) * (
  synth_gar$means + (22.6 - delta_ONBS120c_distribution) - bootstrapped_d18Ow
) + intercept_simulated + rnorm(n_iterations, mean = 0, sd = residual_sd)

# 7. Hold intercept constant
temperature_fixed_intercept <- mean(intercept_simulated) + slope_simulated * (
  synth_gar$means + (22.6 - delta_ONBS120c_distribution) - bootstrapped_d18Ow
) + rnorm(n_iterations, mean = 0, sd = residual_sd)

# Calculate variances for each scenario
variance_all <- var(temperature_all)
variance_fixed_d18Op <- var(temperature_fixed_d18Op)
variance_fixed_d18Ow <- var(temperature_fixed_d18Ow)
variance_fixed_NIST <- var(temperature_fixed_NIST)
variance_no_residual <- var(temperature_no_residual)
variance_fixed_slope <- var(temperature_fixed_slope)
variance_fixed_intercept <- var(temperature_fixed_intercept)


# Relative contribution of variance
contribution_d18Op <- 1 - variance_fixed_d18Op / variance_all
contribution_d18Ow <- 1 - variance_fixed_d18Ow / variance_all
contribution_NIST <- 1 - variance_fixed_NIST / variance_all
contribution_residual <- 1 - variance_no_residual / variance_all
contribution_slope <- 1 - variance_fixed_slope / variance_all
contribution_intercept <- 1 - variance_fixed_intercept / variance_all

# Calculate total contribution
total_contribution <- contribution_d18Op + contribution_d18Ow + contribution_NIST + 
  contribution_residual + contribution_slope + contribution_intercept

# Normalize contributions
normalized_d18Op <- contribution_d18Op / total_contribution
normalized_d18Ow <- contribution_d18Ow / total_contribution
normalized_NIST <- contribution_NIST / total_contribution
normalized_residual <- contribution_residual / total_contribution
normalized_slope <- contribution_slope / total_contribution
normalized_intercept <- contribution_intercept / total_contribution

# Display normalized results
cat("Normalized Variance Contributions:\n")
cat("d18Op Contribution:", round(normalized_d18Op * 100, 1), "%\n")
cat("d18Ow Contribution:", round(normalized_d18Ow * 100, 1), "%\n")
cat("NIST Contribution:", round(normalized_NIST * 100, 1), "%\n")
cat("Residual Error Contribution:", round(normalized_residual * 100, 1), "%\n")
cat("Slope Contribution:", round(normalized_slope * 100, 1), "%\n")
cat("Intercept Contribution:", round(normalized_intercept * 100, 1), "%\n")

# Visualize normalized contributions with a bar plot
library(ggplot2)

# Create a data frame for plotting
contributions <- data.frame(
  Input = c("d18Op", "d18Ow", "NIST", "Residual Error", "Slope", "Intercept"),
  Contribution = c(normalized_d18Op, normalized_d18Ow, normalized_NIST, 
                   normalized_residual, normalized_slope, normalized_intercept) * 100
)

# Plot
ggplot(contributions, aes(x = reorder(Input, -Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Normalized Variance Contributions to Temperature Estimate",
       x = "Input Variable",
       y = "Percentage of Variance Contribution") +
  coord_flip() +
  theme_minimal()

# Check that the normalized contributions sum to 100%
total_normalized_contribution <- sum(contributions$Contribution)
cat("Total Normalized Contribution (%):", total_normalized_contribution, "\n")

# water-air temp transform ------------------------------------------------

# Load necessary libraries
library(ggplot2)
set.seed(123) # For reproducibility

# Regression coefficients from your model
# Extract the regression coefficients and standard errors
TwTa_model_summary <- summary(TwTa_lm_model)
TwTa_intercept <- coef(TwTa_lm_model)[1]
TwTa_slope <- coef(TwTa_lm_model)[2]
TwTa_intercept_se <- coef(TwTa_model_summary)[1, "Std. Error"]  # Standard error for intercept
TwTa_slope_se <- coef(TwTa_model_summary)[2, "Std. Error"]      # Standard error for slope

# Extract residual error
TwTa_residual_sd <- summary(TwTa_lm_model)$sigma

cat("Standard Error for Intercept:", TwTa_intercept_se, "\n")
cat("Standard Error for Slope:", TwTa_slope_se, "\n")

# Monte Carlo simulation parameters
n_iterations <- 1e3 # Number of Monte Carlo iterations

# Provide your distribution of Tw_AMJJAS values
Tw_distribution <- temperature_simulations
str(temperature_simulations)
str(Tw_distribution)

# Monte Carlo simulation incorporating all uncertainties
Ta_simulations <- sapply(Tw_distribution, function(Tw) {
  TwTa_simulated_intercept <- rnorm(n_iterations, mean = TwTa_intercept, sd = TwTa_intercept_se)
  TwTa_simulated_slope <- rnorm(n_iterations, mean = TwTa_slope, sd = TwTa_slope_se)
  TwTa_residual_error <- rnorm(n_iterations, mean = 0, sd = TwTa_residual_sd)
  Tw * TwTa_simulated_slope + TwTa_simulated_intercept + TwTa_residual_error
})

# Compute mean and 95% CI for all simulations
mean_Ta <- apply(Ta_simulations, 1, mean)
Ta_lower_95_CI <- apply(Ta_simulations, 1, quantile, probs = 0.025)
Ta_upper_95_CI <- apply(Ta_simulations, 1, quantile, probs = 0.975)

# Overall mean and 95% CI
overall_mean_Ta <- mean(mean_Ta)
Ta_lower_95_CI <- mean(Ta_lower_95_CI)
Ta_upper_95_CI <- mean(Ta_upper_95_CI)

# Display results
cat("Overall Mean Temperature (°C):", overall_mean_Ta, "\n")
cat("95% Confidence Interval (°C): [", Ta_lower_95_CI, ",", Ta_upper_95_CI, "]\n")

# Create a dataframe
out_Ta <- data.frame(
  Overall_Mean_Temperature_C = overall_mean_Ta,
  Lower_95_CI_C = Ta_lower_95_CI,
  Upper_95_CI_C = Ta_upper_95_CI
)

# Export Results ----------------------------------------------------------

cat("Mean crocG d18Owater:", round(mean_crocGwater_synth, 2), "\n")
cat("95% CI for crocG d18Owater: [", round(crocGwatersynth_lower_95_CI, 2), ",", round(crocGwatersynth_upper_95_CI, 2), "]\n")

cat("Mean crocA d18Owater:", round(mean_crocAwater_synth, 2), "\n")
cat("95% CI for crocA d18Owater: [", round(crocAwatersynth_lower_95_CI, 2), ",", round(crocAwatersynth_upper_95_CI, 2), "]\n")

cat("Mean crocB d18Owater:", round(mean_crocBwater_synth, 2), "\n")
cat("95% CI for crocB d18Owater: [", round(crocBwatersynth_lower_95_CI, 2), ",", round(crocBwatersynth_upper_95_CI, 2), "]\n")

cat("Mean Glyptops d18Owater:", round(mean_glyp_synthwater, 2), "\n")
cat("95% CI for Glyptops d18Owater: [", round(glypwatersynth_lower_95_CI, 2), ",", round(glypwatersynth_upper_95_CI, 2), "]\n")

cat("Mean Naomichelys d18Owater:", round(mean_naomi_synthwater, 2), "\n")
cat("95% CI for Naomichelys d18Owater: [", round(naomiwatersynth_lower_95_CI, 2), ",", round(naomiwatersynth_upper_95_CI, 2), "]\n")

# Data for each taxon
water_sims <- data.frame(
  Taxon = c("crocG", "crocA", "crocB", "Glyptops", "Naomichelys"),
  Mean_d18Owater = c(mean_crocGwater_synth, mean_crocAwater_synth, mean_crocBwater_synth, mean_glyp_synthwater, mean_naomi_synthwater),
  Lower_95_CI = c(crocGwatersynth_lower_95_CI, crocAwatersynth_lower_95_CI, crocBwatersynth_lower_95_CI, glypwatersynth_lower_95_CI, naomiwatersynth_lower_95_CI),
  Upper_95_CI = c(crocGwatersynth_upper_95_CI, crocAwatersynth_upper_95_CI, crocBwatersynth_upper_95_CI, glypwatersynth_upper_95_CI, naomiwatersynth_upper_95_CI)
)

# Print the DataFrame
print(water_sims)

# Export to CSV
write.csv(water_sims, file = "/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/d18Owater_MCsim_summary.csv", row.names = FALSE)

mean_alld18Owater_synth <- mean_d18Ow
alld18Owater_lower <- quantiles[1]
alld18Owater_upper <- quantiles[2]


# Create a data frame summarizing the results
V1075_MC_output_summary <- data.frame(
  Metric = c("Glyptops δ18Osw", "CrocG δ18Osw", "dual_d18Osw", "MAWSWT", "MAWSAT"),
  Mean = c(mean_glyp_synthwater, mean_crocGwater_synth, mean_alld18Owater_synth, Tw_mean_temperature, overall_mean_Ta),
  Lower_95_CI = c(glypwatersynth_lower_95_CI, crocGwatersynth_lower_95_CI, alld18Owater_lower, Tw_lower_95_CI, Ta_lower_95_CI),
  Upper_95_CI = c(glypwatersynth_upper_95_CI, crocGwatersynth_upper_95_CI, alld18Owater_upper, Tw_upper_95_CI, Ta_upper_95_CI)
)


# write and export the .csv of results summary 
write.csv(V1075_MC_output_summary, file = "/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/DataV1075_MCoutput_summary.csv", row.names = FALSE)

