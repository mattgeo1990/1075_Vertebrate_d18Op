
# Data, Packages ----------------------------------------------------------

Barrick_d18O <- read.csv('/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/Barrick1999_data copy.csv')
Barrick_d18O$Genus <- trimws(Barrick_d18O$Genus)


Amiot2007_d18O <- read.csv('/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/AmiotEtAl2007_d18Odata copy.csv')


LonginelliNuti_d18O <- read.csv('/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/Longinelli&Nuti1973_d18Odata copy.csv')

Kolodny_d18O <- read.csv('/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/KolodnyEtAl1983_d18Odata copy.csv')
Kolodny_d18O$t..C..calculated <- as.numeric(Kolodny_d18O$t..C..calculated)
  
Puceat_d18O <- read.csv('/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/PucetEtAl2010_d18Odata copy.csv')

Puceat2010_LonginelliNuti1973_compiled <- read.csv("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/Puceat2010_LongNuti1973_compiled_data.csv")

# Prelim Data wrangling ---------------------------------------------------



# Kolodny data is funky
# Dataset has all replicates
# Here we subset only the means
# They already provided Δa, where  Δa = (d18Op-d18Ow)
# So, we can compare T with Δa to fit regression
# NOW WRANGLE, YEEHAW!

# quick visual confirmation (John Kelly says look at your data!)
plot(Kolodny_d18O_subset$Δa_corrected, Kolodny_d18O_subset$t..C..calculated)

# this doesn't look like Puceat's plot, what went wrong? 



# YOU NEED TO SEE WHY PUCEAT HAS MORE KOLODNY POINTS THAN YOU DO






# Longinelli and Nuti (1973)



# Reproduce Barrick Regression (Turtles) ----------------------------------

### Check the genera distribution in the data
table(Barrick_d18O$Genus)

### Subset the data to include only the specified genera
filtered_data <- subset(Barrick_d18O, Genus %in% c("Pseudemys", "Chrysemys", "Trionyx", "Caretta", "Dermochelys"))

### Check for non-NA values in the relevant columns
#filtered_data <- filtered_data[!is.na(filtered_data$d18Op_VSMOW) & !is.na(filtered_data$d18Ow_VSMOW), ]

### Fit a linear regression model
lm_model <- lm(d18Ow_VSMOW ~ d18Op_VSMOW, data = filtered_data)

### Print regression summary, export the model
summary(lm_model)

# Save the list as an .RDS file
saveRDS(lm_model, file = "/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/Barrick_reg_lm.rds")


### Plot the data and overlay the regression line
  ggplot(filtered_data, aes(x = d18Op_VSMOW, y = d18Ow_VSMOW)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = "Barrick Turtle Regression",
         x = "d18Op_VSMOW",
         y = "d18Ow_VSMOW") +
    theme_minimal()

## Barrick Formula -----------------------------------------------------
  
  # Get the coefficients from the linear model
  coefficients <- coef(lm_model)
  
  # Print the equation in mathematical form
  cat("Equation: d18Ow_VSMOW =", coefficients[1], "+", coefficients[2], "* d18Op_VSMOW\n")
  
  # Calculate residuals
  residuals <- lm_model$residuals
  
  # Calculate residuals and ensure they are numeric
  residuals <- as.numeric(lm_model$residuals)
  
  # Compute the Residual Standard Error (RSE)
  rse <- sqrt(sum(residuals^2) / lm_model$df.residual)
  
  # Print the RSE
  cat("Residual Standard Error (RSE):", rse, "\n")


# Reproduce Amiot Regression (Crocs) --------------------------------------

### Check for non-NA values in the relevant columns
cl_Amiot2007_d18O <- Amiot2007_d18O[!is.na(Amiot2007_d18O$d18Op_SMOW) & !is.na(Amiot2007_d18O$d18Ow_SMOW), ]

### Fit a linear regression model
  Amiot_lm_model <- lm(d18Ow_SMOW ~ d18Op_SMOW, data = cl_Amiot2007_d18O)
  
### Print the regression summary
  summary(Amiot_lm_model)
  
### Plot the data and overlay the regression line
  ggplot(cl_Amiot2007_d18O, aes(x = d18Op_SMOW, y = d18Ow_SMOW)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = "Amiot Croc Regression",
         x = "d18Op_VSMOW",
         y = "d18Ow_VSMOW") +
    theme_minimal()
# Save the list as an .RDS file
  saveRDS(Amiot_lm_model, file = "/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/Amiot_reg_lm.rds")
  
## Amiot Formula -----------------------------------------------------
  
  # Get the coefficients from the linear model
  coefficients <- coef(Amiot_lm_model)
  
  # Print the equation in mathematical form
  cat("Equation: d18Ow_VSMOW =", coefficients[1], "+", coefficients[2], "* d18Op_VSMOW\n")
  
  # Calculate residuals
  residuals <- Amiot_lm_model$residuals
  
  # Calculate residuals and ensure they are numeric
  residuals <- as.numeric(lm_model$residuals)
  
  # Compute the Residual Standard Error (RSE)
  rse <- sqrt(sum(residuals^2) / Amiot_lm_model$df.residual)
  
  # Print the RSE
  cat("Residual Standard Error (RSE):", rse, "\n")


# Reproduce Puceat Temperature Regression (Fish) --------------------------------------

  # Here's the deal with the Puceat regression:
  # Puceat defined a new relationship from new data: T = 124.6 - 4.52 * (d18Op_SMOW - d18Ow_SMOW)
  # They also modeled this regression : T = 118.7 - 4.22 * (d18Op_SMOW - d18Ow_SMOW) by combining their data with Longinelli and Nuti (1973) and Kolodny et al., (1983)
  # They applied a correction D <- ((d18Op-d18Ow) + 2.2 permil) to 1973 and 1983 datasets before combining with the new data
  # Could compute temps with either of these two regressions:
  # FunA (Puceat data only): T = 124.6 - 4.52 * (d18Op_SMOW - d18Ow_SMOW)
  # OR, FunB (Puceat and compiled/corrected data): T = 118.7 - 4.22 * (d18Op_SMOW - d18Ow_SMOW)
  # previously, you used FunB
  # if you stick with FunB, make sure you have the same data Puceat does before you reproduce the regression
  
  PLN_comp <- Puceat2010_LonginelliNuti1973_compiled
  
  ### Check for non-NA values in the relevant columns
  cl_PLN_d18O <- PLN_comp[!is.na(PLN_comp$T..C.) & !is.na(PLN_comp$D18O_corrected), ]
  
  ## Plot data by data source
  ggplot(cl_PLN_d18O, aes(x = D18O_corrected, y = T..C.)) +
    geom_point() +
    labs(title = "Puceat and Longinelli/Nuti",
         x = "d18Op_VSMOW - d18Ow_VSMOW",
         y = "T(°C)") +
    theme_minimal()
  
 
  # Compile data from Puceat et al. (2010) and Longinelli and Nuti (1973)
  
  
  ### Fit a linear regression model
  PLN_lm_model <- lm(T..C. ~ D18O_corrected, data = cl_PLN_d18O)
  
  # Save the list as an .RDS file
  saveRDS(PLN_lm_model, file = "/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/PLNd18Op_reg_lm.rds")
  
  ### Print the regression summary
  summary(PLN_lm_model)

  # Extract and print standard errors
  model_summary <- summary(PLN_lm_model)
  intercept_se <- coef(model_summary)[1, "Std. Error"]  # Standard error for intercept
  slope_se <- coef(model_summary)[2, "Std. Error"]      # Standard error for slope
  
  cat("Standard Error for Intercept:", intercept_se, "\n")
  cat("Standard Error for Slope:", slope_se, "\n")
  
  
  ### Plot the data and overlay the regression line
  ggplot(PLN_comp, aes(x = D18O_corrected, y = T..C.)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = "PLN",
         x = "D18O",
         y = "T") +
    theme_minimal()
  
## Puceat Formula -----------------------------------------------------
  
  # Get the coefficients from the linear model
  coefficients <- coef(PLN_lm_model)
  coefficients[1]
  
  # Print the equation in mathematical form
  cat("Equation: T (°C) =", coefficients[1], "+", coefficients[2], "* d18Op-d18Ow_VSMOW\n")
  
  # Calculate residuals
  residuals <- PLN_lm_model$residuals
  
  # Calculate residuals and ensure they are numeric
  residuals <- as.numeric(PLN_lm_model$residuals)
  
  # Compute the Residual Standard Error (RSE)
  rse <- sqrt(sum(residuals^2) / PLN_lm_model$df.residual)
  
  # Print the RSE
  cat("Residual Standard Error (RSE):", rse, "\n")


# Water-Air Transform Function --------------------------------------------


  