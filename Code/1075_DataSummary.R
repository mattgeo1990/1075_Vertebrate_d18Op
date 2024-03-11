setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/")
raw <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")

summary(raw$d18O..VSMOW.)

aggregate(raw$d18O..VSMOW, by = list(Eco = raw$Eco), FUN = summary)

# Summarize Gar by element type

  # Scales
    # Filter data for Fish with Element.type "ganoid scale"
    fish_ganoid_scale <- subset(raw, Eco == "Fish" & Element.type == "ganoid scale")
    
    # Calculate summary statistics
    summary_ganoid_scale <- summary(fish_ganoid_scale$d18O..VSMOW)
    cat("Summary statistics for Fish with Element.type 'ganoid scale':\n")
    print(summary_ganoid_scale)
    
    
    # Standard Deviation of fish scales
    sd(fish_ganoid_scale$d18O..VSMOW)
  
  # Teeth
    # Filter data for Fish with Element.type "tooth"
    fish_tooth <- subset(raw, Eco == "Fish" & Element.type == "tooth")
    
    # Calculate summary statistics
    summary_tooth <- summary(fish_tooth$d18O..VSMOW)
    cat("\nSummary statistics for Fish with Element.type 'tooth':\n")
    print(summary_tooth)
    
    # SD of gar teeth
      sd(fish_tooth$d18O..VSMOW)

# Standard Deviation by EcoType (EXCLUDING FISH, see above)
      
      # Exclude Fish entries
      filtered_data <- subset(raw, Eco != "Fish")
      
      # Calculate standard deviation for each Eco group
      standard_deviations <- tapply(filtered_data$d18O..VSMOW, filtered_data$Eco, sd)
      
      # Print the standard deviations
      print(standard_deviations)

# Tooth enamel vs dentine by eco type
      # Subset data for "Croc G" Eco group individuals with "tissue" = "dentine"
      dentine_data <- subset(raw, Eco == "Croc G" & Tissue == "dentine")
      
      dentine_data$d18O..VSMOW.
      
      # Subset data for "Croc G" Eco group individuals with "tissue" = "enamel"
      enamel_data <- subset(raw, Eco == "Croc G" & Tissue == "enamel")
      
      # Perform t-test
      t_test_result <- t.test(dentine_data$d18O..VSMOW, enamel_data$d18O..VSMOW)
      
      # Print the result
      print(t_test_result)
      
  