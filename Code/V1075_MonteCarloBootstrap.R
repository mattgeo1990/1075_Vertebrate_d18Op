

# Packages, Data, Objects -------------------------------------------------
  
# Define required packages
packages <- c("dplyr", "ggplot2", "readr", "magrittr", "tidyr", "purrr", "tictoc")

# Create function to check, install, and load packages
check_and_install_packages <- function(packages) {
  missing_packages <- setdiff(packages, installed.packages()[,"Package"])
  if (length(missing_packages) > 0) install.packages(missing_packages, dependencies = TRUE)
  sapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing and loading", pkg))
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  })
}

# Run the package function
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
    # gather stats
      sd(NIST120c$d.18O.16O)
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

    sample_sizes <- c(5, 10, 15, 20)
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
    
    


# Generate Water Isotope Data -----------------------------------------------------
  
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
    
  # Subset sim data by eco_type
    
    # Croc G
    # Subset resamples and extract Croc G rows
    croc_subsets <- lapply(resamples, function(df) subset(df, eco_type == "Croc G"))
    # Combine the Croc G subsets into a single data frame
    sim_croc <- do.call(rbind, croc_subsets)
    # Add d18Omw column to sim_croc
    sim_croc$d18Omw <- NA
    
    # Aquatic Turtle
    # Subset resamples and extract Aquatic Turtle rows
    turtle_subsets <- lapply(resamples, function(df) subset(df, eco_type == "Aquatic Turtle"))
    # Combine the Aquatic Turtle subsets into a single data frame
    sim_turtle <- do.call(rbind, turtle_subsets)
    # Add d18Omw column to sim_turtle
    sim_turtle$d18Omw <- NA
    str(sim_turtle)
    
  # Compute d18Omw from turtle d18Op
    sim_turtle$d18Omw <- turtlewater(sim_turtle$sim_d18Op_mean)$d18Omw
    # Compute d18Omw from croc d18Op
    sim_croc$d18Omw <- crocwater(sim_croc$sim_d18Op_mean)$d18Omw
    
      
  # Run t-test between croc and turtle d18Omw
      # List to store t-test results for each sample_size
      t_test_results <- list()
      
      # Iterate over each sample_size
      for (size in sample_sizes) {
        # Subset sim_croc and sim_turtle for the current sample_size
        croc_subset <- subset(sim_croc, sample_size == size)
        turtle_subset <- subset(sim_turtle, sample_size == size)
        
        # Perform t-test
        t_test_result <- t.test(croc_subset$d18Omw, turtle_subset$d18Omw)
        
        # Store the t-test result in the list
        t_test_results[[paste0("sample_size_", size)]] <- t_test_result
      }
      
      # Check if all p-values are less than 0.05
      all_p_values_less_than_05 <- all(sapply(t_test_results, function(result) result$p.value < 0.05))
      
      # Print message based on the result
      if (all_p_values_less_than_05) {
        cat("d18Omw distributions do not differ (p-values < 0.05)\n")
      } else {
        cat("At least one pair of d18Omw distributions differs (at least one p-value >= 0.05)\n")
      }
    
  # Plot histograms to look at d18Omw distributions 
      
      # Combine sim_croc and sim_turtle into a single data frame for easier plotting
      combined_data <- rbind(cbind(sim_croc, species = "Croc"), cbind(sim_turtle, species = "Turtle"))
      
      # Plot histograms for each combination of eco_type and sample_size
      plot_panel <- ggplot(combined_data, aes(x = d18Omw, fill = species)) +
        geom_histogram(binwidth = 0.25, position = "dodge", alpha = 0.7) +
        facet_grid(eco_type ~ sample_size, scales = "free") +
        labs(title = "Histogram of d18Omw for Simulated Croc and Turtle Data",
             x = "d18Omw", y = "Frequency") +
        theme_minimal()
      
      # Display the plot
      print(plot_panel)
      
# Dual-Taxon Temps --------------------------------------------------------

  # set up
      
    # Subset resamples and extract Fish rows
      fish_subsets <- lapply(resamples, function(df) subset(df, eco_type == "Fish"))
      # Combine the Fish subsets into a single data frame
      sim_fish <- do.call(rbind, fish_subsets)
      
      str(sim_fish)
    
    # Create fishtemp function (Puceat et al., 2010)
      fishtemp <- function(fish_d18Op, NIST120c_mean, d18Omw) {
      temp <- 118.7 - 4.22 * ((fish_d18Op + (22.6 - NIST120c_mean)) - d18Omw)
      return(temp)
      }
      
  # Compute temps
      # Get distinct sample sizes
      distinct_sample_sizes <- unique(sim_turtle$sample_size)
      
      # Function to calculate temperature
      fishtemp <- function(fish_d18Op, NIST120c_mean, d18Omw) {
        temp <- 118.7 - 4.22 * ((fish_d18Op + (22.6 - NIST120c_mean)) - d18Omw)
        return(temp)
      }
      
      # Create an empty data frame to store results
      result_df <- data.frame(Sample_Size = rep(as.numeric(NA), 4*10^6),
                              Temperature = rep(as.numeric(NA), 4*10^6))
      tempLength <- 1*10^4
      temp_df <- data.frame(Sample_Size = rep(as.numeric(NA), tempLength),
                            Temperature = rep(as.numeric(NA), tempLength))

      # Start the timer
      tic()
      
      # Iterate over distinct sample sizes
      LoopDex <- 0
      KeyDex <- 0
      cat("LoopDex is: "); for (Sample_Size in distinct_sample_sizes) {
        # Subset sim_fish and sim_turtle for the current sample_size
        subset_sim_fish <- subset(sim_fish, sample_size == Sample_Size)
        subset_sim_turtle <- subset(sim_turtle, sample_size == Sample_Size)

        # Iterate over each combination of sim_fish$sim_d18Op_mean and sim_turtle$d18Omw
        for (fish_d18Op_mean in subset_sim_fish$sim_d18Op_mean) {
          cat(LoopDex, " ")
          for (turtle_d18Omw in subset_sim_turtle$d18Omw) {
            # Call fishtemp for the current combination & store in a buffer
            temp_df[LoopDex %% tempLength + 1,] <- c(Sample_Size, fishtemp(fish_d18Op_mean, 22.6, turtle_d18Omw))
            LoopDex <- LoopDex + 1
          }
          # Dump the buffer into result_df, then continue the loop
          KeyDex <- KeyDex + 1
          result_df[KeyDex*(1:tempLength),] <- temp_df 
        }
      }; cat("\n")

      # Stop the timer
      toc()
      
  # Plot temp distributions
      ggplot(result_df, aes(x = Temperature, fill = as.factor(Sample_Size))) +
        geom_histogram(position = "identity", alpha = 0.7, bins = 20) +
        facet_wrap(~Sample_Size, scales = "free") +
        labs(title = "Histogram of Temperature for Each Sample Size", x = "Temperature", y = "Frequency") +
        theme_minimal()

# Sand Box ----------------------------------------------------------------
      # Group by Sample_Size and calculate summary statistics for Temperature
      summary_stats <- result_df %>%
        group_by(Sample_Size) %>%
        summarise(
          Mean_Temperature = mean(Temperature, na.rm = TRUE),
          Variance_Temperature = var(Temperature, na.rm = TRUE),
          SE_Temperature = sd(Temperature, na.rm = TRUE) / sqrt(n()),
          SD_Temperature = sd(Temperature, na.rm = TRUE)
        )
      
      # Print the summary statistics
      print(summary_stats)
      
      
      # Calculate confidence intervals for the mean of Temperature for each Sample_Size
      confidence_intervals <- tapply(result_df$Temperature, result_df$Sample_Size, function(x) {
        mean_value <- mean(x, na.rm = TRUE)
        se_value <- sd(x, na.rm = TRUE) / sqrt(length(x))
        conf_int <- confint(lm(Temperature ~ 1, data = data.frame(Temperature = x)))
        c(Mean_Temperature = mean_value, 
          Lower_CI = mean_value - conf_int[2], 
          Upper_CI = mean_value - conf_int[1])
      })
      
      # Convert the result to a data frame
      confidence_intervals_df <- as.data.frame(do.call(rbind, confidence_intervals))
      
      # Print the confidence intervals
      print(confidence_intervals_df)
      
# EECM for later ----------------------------------------------------------

# one attempt
      # use Tom Cullen's ectotherm-endotherm combined mean approach to estimate temp (modified from Fricke and Wing, 2005)
      
      # calc d18O of body water from each endothermic taxon
      # d18Op <- (0.76) * (d18Obw) + 19.94
      # d18Obw <- (d18Op - 19.94)/(0.76)
      
      # Compute mean d18Obw for endotherms 
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
      
      
# a different attempt
      
      # Set up: need to add thermophysiology as variable in simulated dataset
      # In the future, perhaps preserve thermophysiology data when simulating data from empirical dataset
      # Define the conditions for thermophysiology
      ectotherm_conditions <- c("Fish", "Aquatic Turtle", "Terrestrial Turtle", "Croc G", "Croc B", "Croc A")
      endotherm_conditions <- c("Small Theropod", "Sauropoda", "Ornithischian")
      
      # Add the 'thermophysiology' column to each data frame
      resamples <- lapply(resamples, function(df) {
        df$thermophysiology <- ifelse(df$eco_type %in% ectotherm_conditions, "ectotherm",
                                      ifelse(df$eco_type %in% endotherm_conditions, "endotherm", NA))
        return(df)
      })
      
      # Subset into two separate lists
      resamples_ectotherm <- lapply(resamples, function(df) {
        df[df$thermophysiology == "ectotherm", ]
      })
      
      resamples_endotherm <- lapply(resamples, function(df) {
        df[df$thermophysiology == "endotherm", ]
      })
      
      
      
      
      
      
      
      
      
      
    
    
# Recycling Bin -----------------------------------------------------------

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









