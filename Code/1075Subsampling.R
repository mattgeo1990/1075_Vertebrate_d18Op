# Assuming 'data' is your data frame with a column named 'x'

# Install and load necessary packages
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

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
  subsample_size <- sample(3:n, 1)
  subsample <- sample(scales$d18O..VSMOW., subsample_size, replace = FALSE)
  
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

# Plot using ggplot
ggplot(results_df, aes(x = Subsample_Size, y = Variance)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Variance vs Subsample Size",
       x = "Subsample Size",
       y = "Variance") +
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
x <- scales$d18O..VSMOW. # extract the vector of carbon data from the data matrix

output <- matrix(0, nrow = 50, ncol = 1000)  # create empty output matrices
output1 <- matrix(0, nrow = 50, ncol = 1000)
output2 <- matrix(0, nrow = 48, ncol = 1)
output3 <- matrix(0, nrow = 48, ncol = 1)

# Sampling Loop
for (n in 1:49) {  # initiate main loop
  for (m in 1:1000) {  # initiate sub loop
    y <- sample(x, 50, replace = TRUE)  # draw m samples at random from the carbon data with replacement
    output[n, m] <- mean(y[1:(n + 1)])  # average samples of sizes 2 to 50 from the random sample in y
  }
}

# Test Loop
for (n in 1:49) {  # initiate main loop
  for (m in 1:1000) {  # initiate sub loop
    output1[n, m] <- abs(output[n, m] - output[n + 1, m])  # calculate absolute differences between successive sample sizes
  }
}

# Gather Statistics
for (n in 1:48) {  # initiate loop
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
  geom_line() +
  labs(title = "Proportion of Estimates Containing True Mean vs. Sample Size",
       x = "Sample Size",
       y = "Proportion") +
  theme_minimal()
