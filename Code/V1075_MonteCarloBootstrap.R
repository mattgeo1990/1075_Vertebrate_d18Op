

# Packages, Data, Objects -------------------------------------------------
  
  # Packages
    # Vector of package names
    packages <- c("dplyr", "ggplot2", "readr", "magrittr", "tidyr", "purrr")
    
    # Function to check and install packages
    check_and_install_packages <- function(packages) {
      # Check if each package is installed
      missing_packages <- setdiff(packages, installed.packages()[,"Package"])
      
      # Install missing packages
      if (length(missing_packages) > 0) {
        install.packages(missing_packages, dependencies = TRUE)
      }
      
      # Load all packages
      loaded_packages <- sapply(packages, function(pkg) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          message(paste("Installing and loading", pkg))
          install.packages(pkg, dependencies = TRUE)
        }
        library(pkg, character.only = TRUE)
      })  
      
      # Return a logical vector indicating whether each package is successfully loaded
      return(loaded_packages)
    }
    
    check_and_install_packages(packages)

  # Data
    # GitHub raw URL for the CSV file
    github_url <- "https://raw.githubusercontent.com/mattgeo1990/1075_Vertebrate_d18Op/main/Data/V1075_BySpec.csv"
    
    # Read the CSV file into a data frame
    V1075_BySpec <- read_csv(github_url)
    str(V1075_BySpec)
    
    # READ IN NIST120c STD d18Op VALUES FROM RUN 1 and RUN 2
    # NEED NIST120C STD d18Op from Run 3!!!!!
    standards_githubURL <-"https://raw.githubusercontent.com/mattgeo1990/1075_Vertebrate_d18Op/main/Data/V1075_NIST120c.csv"
    NIST120c <- read_csv(standards_githubURL)
    # check for outliers
    hist(NIST120c$d.18O.16O)
    # gather mean
    NIST120c_mean <- mean(NIST120c$d.18O.16O)
    

# Setup
  # Omit Large Theropod and Sharks. Too few data and possible diagenetic influence
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
    simulate_eco_type <- function(subset_data, n_simulations = 1000) {
      mean_val <- mean(subset_data$d18O)
      sd_val <- sd(subset_data$d18O)
      
      simulated_values <- replicate(n_simulations, rnorm(1, mean_val, sd_val))
      return(simulated_values)
    }
    
  # Generate simulated datasets for each eco_type
    simulated_data <- grouped_data %>%
      summarise(simulated_d18O = list(simulate_eco_type(cur_data(), n_simulations = 1000))) %>%
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
  
    
    

# Resampling simulated data--------------------------------------------------------------


    # Function to generate resamples for a given eco_type
    generate_resamples <- function(subset_data, sample_sizes = c(5, 10, 15, 20), n_resamples = 1000) {
      resampled_data <- lapply(sample_sizes, function(sample_size) {
        resamples <- replicate(n_resamples, sample(subset_data$d18O, size = sample_size, replace = TRUE))
        means <- colMeans(resamples)
        variances <- apply(resamples, 2, var)
        standard_errors <- apply(resamples, 2, function(x) sd(x) / sqrt(length(x)))
        standard_deviations <- apply(resamples, 2, sd)
        
        data.frame(
          sample_size = rep(sample_size, each = n_resamples),
          sim_d18Op_mean = means,
          sim_d18Op_var = variances,
          sim_d18Op_SE = standard_errors,
          sim_d18Op_SD = standard_deviations
        )
      })
      
      return(do.call(rbind, resampled_data))
    }

    
    # Generate resamples for each eco_type
    resampled_data <- grouped_data %>%
      group_modify(~ generate_resamples(.x, n_resamples = 1000))
    
    # Assuming resampled_data is the output from generate_resamples function
    resamples <- list()
    
    sample_sizes <- unique(resampled_data$sample_size)
    
    for (size in sample_sizes) {
      resamples[[paste0("sample_size_", size)]] <- subset(resampled_data, sample_size == size)
    }
    
    # Now, each subset is stored in resamples list
    # Access them using resamples$sample_size_5, subsetted_data$sample_size_10, etc.
    
    
    
    
  # Simulated resampling of NIST120c d18O data
    
    
    # NEED TO RUN MONTE CARLO ON NIST120c
    
    


# d18Omw calculations -----------------------------------------------------
  
  # Define d18Omw functions
    
    # d18Omw from croc d18Op (Amiot et al., 2007)
      crocwater <- function(mean_d18Op) {
        result <- 0.82 * mean_d18Op - 19.93
        return(data.frame(d18Omw = result))
      }

    # d18Omw from turtle d18Op (Barrick et al., 1999)
      turtlewater <- function(mean_d18Op) {
        result <- 1.01 * mean_d18Op - 22.3
        return(data.frame(d18Omw = result))
      }
    
    

    # meanCrocFish_temp <- 118.7 - 4.22*((GarScales_d18Op_mean  +(22.6 - NIST120c_mean)) - crocwater ) #Puceat et al. (2010)
  
  # # Function to process Croc G data
    process_croc_data <- function(sim_d18Op_mean) {
      crocwater_result <- crocwater(sim_d18Op_mean)
      return(crocwater_result$d18Omw)
    }
    
    # Function to process Aquatic Turtle data
    process_turtle_data <- function(sim_d18Op_mean) {
      turtlewater_result <- turtlewater(sim_d18Op_mean)
      return(turtlewater_result$d18Omw)
    }
    
    # Iterate over each data frame in the resamples list
    for (size in sample_sizes) {
      current_df <- resamples[[paste0("sample_size_", size)]]
      
      # Process Croc G data and add d18Omw column
      current_df$d18Omw <- ifelse(current_df$eco_type == "Croc G", process_croc_data(current_df$resampled_d18O_mean), NA)
      
      # Process Aquatic Turtle data and add d18Omw column
      current_df$d18Omw <- ifelse(current_df$eco_type == "Aquatic Turtle", process_turtle_data(current_df$resampled_d18O_mean), current_df$d18Omw)
      
      # If needed, replace NAs with values from the other condition
      # current_df$d18Omw <- ifelse(is.na(current_df$d18Omw), process_turtle_data(current_df$resampled_d18O_mean), current_df$d18Omw)
      
      # Update the modified data frame in the list
      resamples[[paste0("sample_size_", size)]] <- current_df
    }
    
    
    # Check to make sure only specificed taxa have d18Omw values
    for (size in sample_sizes) {
      current_df <- resamples[[paste0("sample_size_", size)]]
      
      # Check for rows where eco_type is "Croc G" or "Aquatic Turtle" and d18Omw is not missing
      check_rows <- current_df$eco_type %in% c("Croc G", "Aquatic Turtle") & !is.na(current_df$d18Omw)
      
      # If there are rows violating the condition, print a message or take necessary action
      if (any(!check_rows)) {
        cat("Warning: In sample_size_", size, "data frame, there are rows with invalid d18Omw values.\n")
        # You may take additional actions here, such as printing problematic rows or fixing the issue
        # print(current_df[!check_rows, ])
      }
    }
    
    
    
    
    
    
    
    
    
    
    
    
    # Calculate d18Omw estimates
    #create d18Omw column
      sim_stats$d18Omw <- NA
    # calculate d18Omw
      sim_stats <- sim_stats %>%
      mutate(d18Omw = case_when(
        eco_type == "Aquatic Turtle" & is.na(d18Omw) ~ 1.01 * mean_d18Op - 22.3,
        eco_type == "Croc G" & is.na(d18Omw) ~ 0.82 * mean_d18Op - 19.93,
        TRUE ~ d18Omw  # Keep existing values for other cases
      ))
      
      
    
    # Check if only rows with eco_type = "Croc G" have non-missing d18Omw values
      only_croc_g_rows <- sim_stats %>%
        filter(eco_type == "Croc G") %>%
        pull(d18Omw) %>%
        complete.cases()
    
    # Find unique eco_types with non-missing d18Omw values
      eco_types_with_d18Omw <- sim_stats %>%
        filter(!is.na(d18Omw)) %>%
        distinct(eco_type)
    
    # Print the results
      if (all(only_croc_g_rows)) {
        cat("All rows with eco_type = 'Croc G' have non-missing d18Omw values.\n")
      } else {
        cat("There are rows with eco_type = 'Croc G' that have missing d18Omw values.\n")
      }
      
      cat("Eco_types with non-missing d18Omw values:", unique(eco_types_with_d18Omw$eco_type), "\n")
   
    # Subset data for "Croc G"
      croc_g_data <- sim_stats %>%
        filter(eco_type == "Croc G")
      
    # Calculate simulated values
      croc_g_data$sim_crocwater <- 0.82 * croc_g_data$mean_d18Op - 19.93
    
    # Plot the distribution of sim_crocwater for "Croc G"
      hist(croc_g_data$d18Omw, main = "Distribution of d18Omw estimates (simulated Croc G)", xlab = "sim_crocwater", col = "skyblue", border = "black")
   
      
    # Subset data for "Fish"
    gar_data <- sim_stats %>%
        filter(eco_type == "Fish")
    
    # Subset data for "Aquatic Turtle"
    aquaturtle_data <- sim_stats %>%
      filter(eco_type == "Aquatic Turtle")
    
      
    
# Sim Means and CI --------------------------------------------------------


    
# Sand Box ----------------------------------------------------------------

    generate_resamples <- function(subset_data, sample_sizes = c(5, 10, 15, 20), n_resamples = 1000) {
      resampled_data <- lapply(sample_sizes, function(sample_size) {
        resamples <- replicate(n_resamples, sample(subset_data$d18O, size = sample_size, replace = TRUE))
        means <- colMeans(resamples)
        variances <- apply(resamples, 2, var)
        standard_errors <- apply(resamples, 2, function(x) sd(x) / sqrt(length(x)))
        standard_deviations <- apply(resamples, 2, sd)
        
        data.frame(
          sample_size = rep(sample_size, each = n_resamples),
          resampled_d18O_mean = means,
          resampled_d18O_variance = variances,
          resampled_d18O_standard_error = standard_errors,
          resampled_d18O_standard_deviation = standard_deviations
        )
      })
      
      return(do.call(rbind, resampled_data))
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # Subset the data for each eco_type
    aquatic_turtle_data <- sim_stats %>% filter(eco_type == "Aquatic Turtle")
    croc_g_data <- sim_stats %>% filter(eco_type == "Croc G")
    
    # Create a function to calculate variance for each sample size
    calculate_variance <- function(data) {
      data %>%
        group_by(sample_size) %>%
        summarize(variance = var(d18Omw, na.rm = TRUE))
    }
    
    # Apply the function to each eco_type
    aquatic_turtle_variance <- calculate_variance(aquatic_turtle_data)
    croc_g_variance <- calculate_variance(croc_g_data)
    
    # Plot variance vs sample size for each eco type using ggplot
    ggplot(aquatic_turtle_variance, aes(x = sample_size, y = variance)) +
      geom_point() +
      geom_line() +
      labs(title = "Variance vs Sample Size (Aquatic Turtle)",
           x = "Sample Size",
           y = "Variance")
    
    ggplot(croc_g_variance, aes(x = sample_size, y = variance)) +
      geom_point() +
      geom_line() +
      labs(title = "Variance vs Sample Size (Croc G)",
           x = "Sample Size",
           y = "Variance")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
# Recycling Bin -----------------------------------------------------------

    # create function to generate resamples
    generate_resamples <- function(subset_data, sample_sizes = c(5, 10, 15, 20), n_resamples = 1000) {
      resampled_data <- lapply(sample_sizes, function(sample_size) {
        replicate(n_resamples, mean(sample(subset_data$d18O, size = sample_size, replace = TRUE)))
      })
      
      return(data.frame(
        sample_size = rep(sample_sizes, each = n_resamples),
        resampled_d18O = unlist(resampled_data)
      ))
    }
    
    
    
    
    
    
    
    
    
    
    
    

    
  # Gather stats on resampled data
    
    # Function to calculate mean, standard error, and gather results
    calculate_summary_stats <- function(resampled_data) {
      eco_type <- unique(resampled_data$eco_type)[1]  # Assuming eco_type is the same for all rows
      summary_stats <- resampled_data %>%
        group_by(sample_size) %>%
        summarise(
          mean_d18Op = mean(resampled_d18O),
          se_d18Op = sd(resampled_d18O) / sqrt(length(resampled_d18O)),
          eco_type = eco_type
        )
      return(summary_stats)
    }
    
    # Apply the function to each group in resampled_data
    sim_stats <- resampled_data %>%
      group_split(eco_type) %>%
      map_dfr(~ calculate_summary_stats(.x))
    
    # Check the structure of the sim_stats
    str(sim_stats)
    
    # Plot se_d18Op against sample_size 
    #ggplot(sim_stats, aes(x = sample_size, y = se_d18Op)) +
    geom_point() +
      geom_line() +
      labs(title = "Plot of se_d18Op against sample_size", x = "Sample Size", y = "se_d18Op")
    
    
    
    
    
    
    
    
    
    #Attempt at calculating temperature distribution
    
    # Set seed for reproducibility
    set.seed(123)
    
    # Create a data frame to store the results
    croc_temps <- data.frame()
    
    # Define the Croc_temp function
    calculate_Croc_temp <- function(gar_d18Op_mean, croc_d18Omw, NIST120c_mean) {
      return(118.7 - 4.22 * ((gar_d18Op_mean + (22.6 - NIST120c_mean)) - croc_d18Omw))
    }
    
    your_sample_sizes_vector <- unique(sim_stats$sample_size)
    
    # Loop over each sample size
    for (sample_size in your_sample_sizes_vector) {
      # Generate combinations of data
      combinations <- expand.grid(
        gar_d18Op_mean = gar_data$mean_d18Op,
        croc_d18Omw = croc_g_data$d18Omw
      )
      
      # Calculate Croc_temp for each combination
      combinations$sample_size <- sample_size
      combinations$Croc_temp <- calculate_Croc_temp(
        combinations$gar_d18Op_mean,
        combinations$croc_d18Omw,
        NIST120c_mean
      )
      
      # Store the results in croc_temps
      croc_temps <- rbind(croc_temps, combinations)
    }
    
    # Print the first few rows of croc_temps
    head(croc_temps)
    
    
# proxy calculations
    
    # Calculate Croc G d18Omw from formula in Amiot et al.(2007) and store d18Omw values in a new column
    sim_stats <- sim_stats %>%
      mutate(d18Omw = ifelse(eco_type == "Croc G", 0.82 * mean_d18Op - 19.93, NA))
    
    # Check if only rows with eco_type = "Croc G" have non-missing d18Omw values
    only_croc_g_rows <- sim_stats %>%
      filter(eco_type == "Croc G") %>%
      pull(d18Omw) %>%
      complete.cases()
    
    # Print the result
    if (all(only_croc_g_rows)) {
      cat("All rows with eco_type = 'Croc G' have non-missing d18Omw values.\n")
    } else {
      cat("There are rows with eco_type = 'Croc G' that have missing d18Omw values.\n")
    }
    
    # Subset data for "Croc G"
    croc_g_data <- sim_stats %>%
      filter(eco_type == "Croc G")
    
    # Calculate simulated values
    croc_g_data$sim_crocwater <- 0.82 * croc_g_data$mean_d18Op - 19.93
    
    # Plot the distribution of sim_crocwater for "Croc G"
    hist(croc_g_data$sim_crocwater, main = "Distribution of sim_crocwater (Croc G)", xlab = "sim_crocwater", col = "skyblue", border = "black")
    croc_g_datasd()
    
    
    # Calculate Aquatic Turtle d18Omw using formula from Barrick et al. (1999)
    sim_stats <- sim_stats %>%
      mutate(d18Omw = ifelse(eco_type == "Aquatic Turtle", 1.01 * mean_d18Op - 22.3, NA))
    
    # Check if rows with eco_type = "Aquatic Turtle" have non-missing d18Omw values
    only_aquaturtle_rows <- sim_stats %>%
      filter(eco_type == "Aquatic Turtle") %>%
      pull(d18Omw) %>%
      complete.cases()
    
    # Print the result
    if (all(only_aquaturtle_rows)) {
      cat("All rows with eco_type = 'Aquatic Turtle' have non-missing d18Omw values.\n")
    } else {
      cat("There are rows with eco_type = 'Aquatic Turtle' that have missing d18Omw values.\n")
    }
    
    

    
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









