
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

summary(Amiot_lm_model)
summary(PLN_lm_model)
coef(PLN_lm_model)[1]


# Monte Carlo Parameters --------------------------------------------------

# Set number of Monte Carlo repetitions 
nMCrepetitions <- 1e5

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
mean(synth_gar$means)

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


# Neosuchian G --------------------------------------------------------------

# Extract the regression coefficients and standard errors
Amiot_model_summary <- summary(Amiot_lm_model)
Amiot_intercept <- coef(Amiot_lm_model)[1]
Amiot_slope <- coef(Amiot_lm_model)[2]
Amiot_intercept_se <- coef(Amiot_model_summary)[1, "Std. Error"]  # Standard error for intercept
Amiot_slope_se <- coef(Amiot_model_summary)[2, "Std. Error"]      # Standard error for slope

cat("Standard Error for Intercept:", Amiot_intercept_se, "\n")
cat("Standard Error for Slope:", Amiot_slope_se, "\n")

# Define function to compute d18Owater with regression uncertainty
crocwater <- function(crocG_synthmeans, slope, intercept) {
  slope * crocG_synthmeans + intercept
}

# Simulate regression coefficients for the regression model
set.seed(123)  # For reproducibility
n_iterations <- 10  # Number of Monte Carlo simulations

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

# simulate d18Owater estimates
crocG_water_simulations <- Amiot_simulated_slope * crocG_synthmeans_d18Op + Amiot_simulated_intercept + Amiot_residual_error
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
cat("95% CI for crocG d18Owater: [", round(crocGwatersynth_lower_95_CI, 2), ",", round(crocwatersynth_upper_95_CI, 2), "]\n")

summary(crocG_water_simulations)

# Neosuchian A --------------------------------------------------------------

# Simulate regression coefficients for the regression model
set.seed(123)  # For reproducibility
n_iterations <- 1e5  # Number of Monte Carlo simulations

# Store Croc A d18Op synth means in vector
crocA_synthmeans_d18Op <- synth_crocA$means
mean(crocA_synthmeans_d18Op)

# simulate d18Owater estimates
crocA_water_simulations <- Amiot_simulated_slope * crocA_synthmeans_d18Op + Amiot_simulated_intercept + Amiot_residual_error
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
cat("95% CI for crocA d18Owater: [", round(crocAwatersynth_lower_95_CI, 2), ",", round(crocwatersynth_upper_95_CI, 2), "]\n")

summary(crocA_water_simulations)

# Neosuchian B --------------------------------------------------------------

# Simulate regression coefficients for the regression model
set.seed(123)  # For reproducibility
n_iterations <- 1e5  # Number of Monte Carlo simulations

# Store Croc B d18Op synth means in vector
crocB_synthmeans_d18Op <- synth_crocB$means
mean(crocB_synthmeans_d18Op)

# simulate d18Owater estimates
crocB_water_simulations <- Amiot_simulated_slope * crocB_synthmeans_d18Op + Amiot_simulated_intercept + Amiot_residual_error
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
cat("95% CI for crocB d18Owater: [", round(crocBwatersynth_lower_95_CI, 2), ",", round(crocwatersynth_upper_95_CI, 2), "]\n")

summary(crocB_water_simulations)
## Error Analysis for crocwater --------------------------------------------

# Set seed for reproducibility
set.seed(123)

# Number of Monte Carlo iterations
n_iterations <- 1e5

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

# Store Glyptops d18Op synth means in vector
glyp_synthmeans_d18Op <- synth_glyptops$means

# Define function to compute d18Owater with regression uncertainty
turtlewater <- function(glyp_synthmeans_d18Op, Barrick_slope, Barrick_intercept) {
  Barrick_slope * glyp_synthmeans_d18Op + Barrick_intercept
}

# Simulate regression coefficients for the Barrick regression model
set.seed(123)  # For reproducibility
n_iterations <- 1e5  # Number of Monte Carlo simulations

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

# simulate d18Owater estimates
glyp_water_simulations <- Barrick_simulated_slope * glyp_synthmeans_d18Op + Barrick_simulated_intercept + Barrick_residual_error
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
n_iterations <- 1e5  # Number of Monte Carlo simulations

# Monte Carlo simulation for the regression
# Store Naomichelys d18Op synth means in vector
naomi_synthmeans_d18Op <- synth_naomichelys$means
mean(naomi_synthmeans_d18Op)

# simulate d18Owater estimates
naomi_water_simulations <- Barrick_simulated_slope * naomi_synthmeans_d18Op + Barrick_simulated_intercept + Barrick_residual_error
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
n_iterations <- 1e5

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
n_iterations <- 1e5

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
n_iterations <- 1e5

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

# Monte Carlo simulation for the regression
temperature_simulations <- PLN_intercept_simulated + PLN_slope_simulated * (
  delta_Op_distribution + (22.6 - delta_ONBS120c_distribution) - delta_Ow_distribution
) + rnorm(n_iterations, mean = 0, sd = residual_sd)

# Summarize the results
mean_temperature <- mean(temperature_simulations)
lower_95_CI <- quantile(temperature_simulations, probs = 0.025)
upper_95_CI <- quantile(temperature_simulations, probs = 0.975)

# Display results
cat("Mean Temperature (°C):", mean_temperature, "\n")
cat("95% Confidence Interval (°C): [", lower_95_CI, ",", upper_95_CI, "]\n")

# Calculate statistics
mean_temp <- mean(temperature_simulations)
ci_lower <- quantile(temperature_simulations, probs = 0.025)
ci_upper <- quantile(temperature_simulations, probs = 0.975)

# Create the histogram using ggplot2
library(ggplot2)

ggplot(data.frame(temperature_simulations), aes(x = temperature_simulations)) +
  geom_histogram(bins = 50, fill = "blue", color = "black", alpha = 0.7) +
  # Add vertical lines for mean and 95% CI
  geom_vline(xintercept = mean_temp, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = ci_lower, color = "blue", linetype = "dotted", size = 1) +
  geom_vline(xintercept = ci_upper, color = "blue", linetype = "dotted", size = 1) +
  # Add labels for the lines
  annotate("text", x = mean_temp, y = 4000, label = paste("Mean:", round(mean_temp, 2)), 
           color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = ci_lower, y = 4000, label = paste("Lower 95% CI:", round(ci_lower, 2)), 
           color = "blue", angle = 90, vjust = -0.5) +
  annotate("text", x = ci_upper, y = 4000, label = paste("Upper 95% CI:", round(ci_upper, 2)), 
           color = "blue", angle = 90, vjust = -0.5) +
  # Add titles and labels
  labs(title = "Simulated Temperature Distribution",
       x = "Temperature (°C)",
       y = "Frequency") +
  theme_minimal()

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
n_iterations <- 1e5

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
intercept <- 4.4788
slope <- 0.7668
intercept_se <- 3.5883
slope_se <- 0.1340
residual_sd <- 1.586 # Residual standard error from the model

# Monte Carlo simulation parameters
n_iterations <- 1e5 # Number of Monte Carlo iterations

# Provide your distribution of Tw_AMJJAS values
Tw_distribution <- temperature_simulations

# Vectorized Monte Carlo simulation
# Simulate regression coefficients
simulated_intercept <- rnorm(n_iterations, mean = intercept, sd = intercept_se)
simulated_slope <- rnorm(n_iterations, mean = slope, sd = slope_se)
residual_error <- rnorm(n_iterations, mean = 0, sd = residual_sd)

# Compute Ta for all iterations and Tw values in a single step
Ta_simulations <- Tw_distribution * simulated_slope + simulated_intercept + residual_error

# Compute statistics for each Tw value
mean_Ta <- mean(Ta_simulations)
lower_95_CI <- quantile(Ta_simulations, probs = 0.025)
upper_95_CI <- quantile(Ta_simulations, probs = 0.975)

# Display results
cat("Mean Temperature (°C):", mean_Ta, "\n")
cat("95% Confidence Interval (°C): [", lower_95_CI, ",", upper_95_CI, "]\n")


# Export Results ----------------------------------------------------------

cat("Mean crocG d18Owater:", round(mean_crocGwater_synth, 2), "\n")
cat("95% CI for crocG d18Owater: [", round(crocGwatersynth_lower_95_CI, 2), ",", round(crocwatersynth_upper_95_CI, 2), "]\n")

cat("Mean crocA d18Owater:", round(mean_crocAwater_synth, 2), "\n")
cat("95% CI for crocA d18Owater: [", round(crocAwatersynth_lower_95_CI, 2), ",", round(crocwatersynth_upper_95_CI, 2), "]\n")

cat("Mean crocB d18Owater:", round(mean_crocBwater_synth, 2), "\n")
cat("95% CI for crocB d18Owater: [", round(crocBwatersynth_lower_95_CI, 2), ",", round(crocwatersynth_upper_95_CI, 2), "]\n")

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
