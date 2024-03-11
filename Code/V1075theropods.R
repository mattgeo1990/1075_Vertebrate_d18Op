# Is there difference between dromaeosaurs and troodontids?

  # Subset the data based on Taxon groups
  subset_data <- subset(raw, Taxon %in% c("Theropoda (small)", "Deinonychus antirrhopus"))
  
  # Create a ggplot object
  ggplot(subset_data, aes(x = Taxon, y = d18O..VSMOW.)) +
    geom_point(color = "blue") +
    labs(x = "Taxon", y = "d18O", title = "d18O by Taxon") +
    theme_minimal()

# Is there chronological pattern in large theropoda

  # Subset the data based on Eco group "Large Theropod"
  large_theropod_data <- subset(raw, Eco == "Large Theropod")
  
  # Create a ggplot object
  ggplot(large_theropod_data, aes(x = Sample.ID, y = `d18O..VSMOW.`)) +
    geom_point(color = "blue") +
    labs(x = "Sample ID", y = "d18O..VSMOW.", title = "d18O..VSMOW. by Sample ID for Large Theropod") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  