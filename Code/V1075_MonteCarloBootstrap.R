

# Packages, Data, Objects -------------------------------------------------

# Install and load packages if not already installed
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}


# Setup
  # Omit Large Theropod and Sharks. Too few data and possible dentine contamination
    V1075_MC <- subset(V1075_BySpec, eco_type != "Large Theropod" & eco_type != "Shark")

  # Group by eco_type
    grouped_data <- V1075_MC %>% group_by(eco_type)

# Look at your data 
  # Plotting a panel of histograms with consistent bin sizes and performing normality tests
    p <- ggplot(grouped_data, aes(x = d18O, fill = eco_type)) +
      geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
      labs(title = "Histogram of d18O by eco_type",
           x = "d18O",
           y = "Frequency") +
      theme_minimal() +
      facet_wrap(~eco_type, scales = "free")
    
  # Perform Shapiro-Wilk test for normality for each eco_type
    shapiro_tests <- by(grouped_data$d18O, grouped_data$eco_type, shapiro.test)
    
  # Display normality test results
    cat("\nNormality Tests:\n")
    print(shapiro_tests)
    
  # Extract p-values from the test results
    p_values <- sapply(shapiro_tests, function(x) x$p.value)
    
  # Highlight non-normally distributed groups (p-value < 0.05)
    non_normal_groups <- names(p_values[p_values < 0.05])
    cat("\nNon-normally distributed groups:", ifelse(length(non_normal_groups) > 0, paste(non_normal_groups, collapse = ", "), "None"), "\n")
    
  # Print the ggplot object
    print(p)
    

# Simulate dataset ------------------------------------------------------

  # Function to generate simulated dataset for a given eco_type
    simulate_eco_type <- function(subset_data, n_simulations = 10000) {
      mean_val <- mean(subset_data$d18O)
      sd_val <- sd(subset_data$d18O)
      
      simulated_values <- replicate(n_simulations, rnorm(1, mean_val, sd_val))
      return(simulated_values)
    }
    
  # Generate simulated datasets for each eco_type
    simulated_data <- grouped_data %>%
      summarise(simulated_d18O = list(simulate_eco_type(cur_data(), n_simulations = 10000))) %>%
      unnest(simulated_d18O)
    
  # Check the structure of the simulated_data
    str(simulated_data)
    
  # Plot histograms of the simulated data to compare against the empirical data
    ggplot(simulated_data, aes(x = simulated_d18O, fill = eco_type)) +
      geom_histogram(binwidth = 0.2, position = "identity", alpha = 0.7) +
      labs(title = "Simulated Histograms of d18O by eco_type", x = "Simulated d18O", y = "Frequency") +
      scale_fill_manual(values = c("A" = "blue", "B" = "green")) +
      facet_wrap(~eco_type, scales = "free") +
      theme_minimal()
  
    
    

# Resampling --------------------------------------------------------------


    # Function to generate resamples for a given eco_type
    generate_resamples <- function(subset_data, n_resamples = 10000, min_sample_size = 2, max_sample_size = 50) {
      mean_val <- mean(subset_data$d18O)
      sd_val <- sd(subset_data$d18O)
      
      resampled_data <- lapply(seq(min_sample_size, max_sample_size), function(sample_size) {
        replicate(n_resamples, mean(sample(rnorm(sample_size, mean_val, sd_val))))
      })
      
      return(data.frame(sample_size = rep(seq(min_sample_size, max_sample_size), each = n_resamples),
                        resampled_d18O = unlist(resampled_data)))
    }
    
    # Generate resamples for each eco_type
    resampled_data <- grouped_data %>%
      group_modify(~ generate_resamples(.x, n_resamples = 10000))
    
    # Check the structure of the resampled_data
    str(resampled_data)
    

# Gather Stats on Resamples -----------------------------------------------


# Sand Box ----------------------------------------------------------------

    # Assuming "grouped_data" is your grouped data frame and includes "d18O" column
    # Replace "d18O" and "eco_type" with the actual column names in your data frame
    
    # Function to generate random samples for a given eco_type and calculate mean and sd
    generate_and_summarize_samples <- function(subset_data, n_samples = 1000000, min_sample_size = 2, max_sample_size = 50) {
      if (nrow(subset_data) == 0) {
        return(data.frame())  # Return an empty data frame if there are no rows for the given eco_type
      }
      
      sampled_data <- lapply(seq(min_sample_size, max_sample_size), function(sample_size) {
        replicate(n_samples, {
          sample_values <- sample(subset_data$d18O, size = sample_size, replace = TRUE)
          data.frame(
            eco_type = subset_data$eco_type[1],  # use the first value, assuming it's the same for all
            sample_size = sample_size,
            mean_value = mean(sample_values),
            sd_value = sd(sample_values),
            sampled_d18O = list(sample_values)
          )
        })
      })
      
      return(bind_rows(unlist(sampled_data, recursive = FALSE)))
    }
    
    # Generate random samples and calculate mean and sd for each eco_type
    random_samples_data <- grouped_data %>%
      group_modify(~ generate_and_summarize_samples(.x, n_samples = 1000000))
    
    # Check the structure of the random_samples_data
    str(random_samples_data)
    
    
    
    
    
    
# Recycling Bin -----------------------------------------------------------

   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
# Assuming 'x' is your d18O values and 'Eco' is your grouping variable
x <- V1075_MCbs$d18O
groups <- V1075_MCbs$eco_type

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
       y = "Mean Improvement",
       color = "Eco Group",
       linetype = "Eco Group") +
  theme_minimal()

# Ovis simulation 2

# Setup
output1 <- matrix(0, nrow = 1000, ncol = 49)
output2 <- matrix(0, nrow = 1000, ncol = 49)
output3 <- matrix(0, nrow = 1000, ncol = 49)

# Sampling Loop
for (m in 2:50) {
  for (n in 1:1000) {
    y <- sample(x, m, replace = TRUE)
    a <- mean(y)
    b <- 1.96 * (sd(y) / sqrt(m))
    output1[n, m - 1] <- a + b
    output2[n, m - 1] <- a - b
  }
}

truemean <- median(boot::boot(x, R = 1000, statistic = function(x, i) mean(x[i]))$t)

# Test Loop
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

# Create a data frame for plotting
plot_data <- data.frame(n = 1:49, proportion = output4[1, ])

# Plot with ggplot2
ggplot(plot_data, aes(x = n, y = proportion)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Proportion of Estimates Containing True Mean vs. Sample Size",
       x = "Sample Size",
       y = "Proportion") +
  theme_minimal()










# Assuming 'x' is your d18O values
x <- V1075_cl$d18O..VSMOW.

# Ovis Simulation 1

# Setup
output <- matrix(0, nrow = 50, ncol = 1000)
output1 <- matrix(0, nrow = 50, ncol = 1000)
output2 <- matrix(0, nrow = 48, ncol = 1)
output3 <- matrix(0, nrow = 48, ncol = 1)

# Sampling Loop
for (n in 1:49) {
  for (m in 1:1000) {
    y <- sample(x, 50, replace = TRUE)
    output[n, m] <- mean(y[1:(n + 1)])
  }
}

# Test Loop
for (n in 1:49) {
  for (m in 1:1000) {
    output1[n, m] <- abs(output[n, m] - output[n + 1, m])
  }
}

# Gather Statistics
for (n in 1:48) {
  output2[n, 1] <- mean(output1[n, 1:1000])
  output3[n, 1] <- sd(output1[n, 1:1000])
}

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
output1 <- matrix(0, nrow = 1000, ncol = 49)
output2 <- matrix(0, nrow = 1000, ncol = 49)
output3 <- matrix(0, nrow = 1000, ncol = 49)

# Sampling Loop
for (m in 2:50) {
  for (n in 1:1000) {
    y <- sample(x, m, replace = TRUE)
    a <- mean(y)
    b <- 1.96 * (sd(y) / sqrt(m))
    output1[n, m - 1] <- a + b
    output2[n, m - 1] <- a - b
  }
}

truemean <- median(boot::boot(x, R = 1000, statistic = function(x, i) mean(x[i]))$t)

# Test Loop
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

# Create a data frame for plotting
plot_data <- data.frame(n = 1:49, proportion = output4[1, ])

# Plot with ggplot2
ggplot(plot_data, aes(x = n, y = proportion)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Proportion of Estimates Containing True Mean vs. Sample Size",
       x = "Sample Size",
       y = "Proportion") +
  theme_minimal()









