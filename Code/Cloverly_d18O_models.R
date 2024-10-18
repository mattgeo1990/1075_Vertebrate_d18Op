

# Install/load required packages ------------------------------------------

# For now, just source from local 
setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
V1075_all <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")
V1075_BySpec <- read.csv("V1075_BySpec.csv")
NIST120c <- read.csv("V1075_NIST120c.csv")


# subset fish scales
Gar_scales <- subset(V1075_all, Element.type == "ganoid scale")
# subset Croc G
CrocG <- subset(V1075_BySpec, Taxon == "Neosuchian G")
# subset Aquatic Turtle
Glyp <- subset(V1075_BySpec, Taxon == "Glyptops sp.")

# Just hang on to this for a while, will ya? For sanity checks
Gar_all_d18Op <- subset(V1075_all$d18O..VSMOW., V1075_all$Taxon == "Lepisosteids")
Gar_scales_d18Op <- subset(V1075_all$d18O..VSMOW., V1075_all$Element.type == "ganoid scale")
summary(Gar_all_d18Op)
summary(Gar_scales_d18Op)


# Gather Means
# Croc G
AquaCroc_d18Op_mean <- mean(CrocG$d18O)
# Glyptops
AquaTurt_d18Op_mean <- mean(Glyp$d18O)
# Gar scales
d18Ogar_phosphate <- mean(Gar_scales$d18O)
# NIST
NIST120c_mean <- mean(NIST120c$d.18O.16O)

# create "d18Owater from d18Ophsophate" function
d18O_w_from_p <- function(MAT, d18Op, NIST120c_mean) {
  turtwater <- (MAT - 118.7 + 4.22 * d18Op + 4.22 * (22.6 - NIST120c_mean)) / 4.22
  return(turtwater)
}

# Set MAT and d8Ophosphate inputs
  MAT <- seq(12.3, 23.5, by = 0.1)
  d18Op <- d18Ogar_phosphate 

# run the function
  d18Ow <- d18O_w_from_p(MAT, d18Op, NIST120c_mean)

# plot water vs MAT range
  plot(d18Ow, MAT)
  
  
# Set MAT at modeled minimum (Cool K phase), compute d18Ow for each d18Op
  
  # Set inputs 
  MAT <- 12
  d18Op <- Gar$d18O
  
  # run the function
  d18Ow <- d18O_w_from_p(MAT, d18Op, NIST120c_mean) 
  
  # plot 
  plot(d18Ow, d18Op, main="d18Ogar @ 12 degC")

# Set MAT at modeled maximum (Warm K phase), compute d18Ow for each d18Op
  # Set inputs 
    MAT <- 25.3
    d18Op <- Gar$d18O
  
    # run the function
    d18Ow <- d18O_w_from_p(MAT, d18Op, NIST120c_mean) 
    
    # plot 
    plot(d18Ow, d18Op, main="d18Ogar @ 12 degC")

    summary(d18Ow)
    
    summary(d18Op)
    

# # ChatGPT  Cloverly temp model ------------------------------------------

    # Function to create sine curve with specified minima, maxima, and wavelength
    create_sine_curve <- function(minima, maxima, wavelength, x_limits) {
      x <- seq(x_limits[1], x_limits[2], length.out = 1000)
      y <- (maxima - minima) / 2 * sin(2 * pi * (x / wavelength)) + (maxima + minima) / 2
      
      upper_band <- y + 4
      lower_band <- y - 4
      
      plot(x, y, type = "l", xlab = "Time (Ma)", ylab = "Value", xlim = x_limits, ylim = c(min(lower_band), max(upper_band)))
      
      # Adding band around the sine curve
      polygon(c(x, rev(x)), c(upper_band, rev(lower_band)), col = "lightblue", border = NA)
    }
    
    # Define parameters
    minima <- 12.3
    maxima <- 23.5
    wavelength <- 1
    x_limits <- c(0, 5)
    
    # Create and plot the sine curve with band
    create_sine_curve(minima, maxima, wavelength, x_limits)
   
    
    
    
    # Function to create original sine curve
    create_sine_curve <- function(minima, maxima, wavelength, x_limits) {
      x <- seq(x_limits[1], x_limits[2], length.out = 1000)
      y <- (maxima - minima) / 2 * sin(2 * pi * (x / wavelength)) + (maxima + minima) / 2
      lines(x, y, col = "black", lwd = 2)  # Plotting original sine curve as black bold line
    }
    
    # Function to create high band limit
    high_band_limit <- function(minima, maxima, wavelength, x_limits, band_width) {
      x <- seq(x_limits[1], x_limits[2], length.out = 1000)
      y <- (maxima - minima) / 2 * sin(2 * pi * (x / wavelength)) + (maxima + minima) / 2 + band_width
      lines(x, y, col = "red", lty = 2)  # Plotting upper limit curve as red dashed line
      upper_band <- y + 15
      lower_band <- y - 15
      polygon(c(x, rev(x)), c(upper_band, rev(lower_band)), col = rgb(1,0,0,0.2), border = NA)  # Adding transparent band around upper limit curve
    }
    
    # Function to create low band limit
    low_band_limit <- function(minima, maxima, wavelength, x_limits, band_width) {
      x <- seq(x_limits[1], x_limits[2], length.out = 1000)
      y <- (maxima - minima) / 2 * sin(2 * pi * (x / wavelength)) + (maxima + minima) / 2 - band_width
      lines(x, y, col = "blue", lty = 2)  # Plotting lower limit curve as blue dashed line
      upper_band <- y + 15
      lower_band <- y - 15
      polygon(c(x, rev(x)), c(upper_band, rev(lower_band)), col = rgb(0,0,1,0.2), border = NA)  # Adding transparent band around lower limit curve
    }
    
    # Define parameters
    minima <- 12.3
    maxima <- 23.5
    wavelength <- 1
    x_limits <- c(0, 5)
    band_width <- 4
    
    # Calculate overall y-axis limits
    y_min <- minima - 15
    y_max <- maxima + 5
    
    # Plotting
    plot(x = c(0, 5), y = c(y_min, y_max), type = "n", xlab = "Time (Ma)", ylab = "T (°C)")
    create_sine_curve(minima, maxima, wavelength, x_limits)
    high_band_limit(minima, maxima, wavelength, x_limits, band_width)
    low_band_limit(minima, maxima, wavelength, x_limits, band_width)
    
    mean(c(12.3, 23.5))

# Estimate d18Ow from min and max -----------------------------------------

# Warm K Mode, Warm-season mean of 27.5 °C
    # Set MAT and d8Ophosphate inputs
    MAT <- 27.5
    d18Op <- d18Ogar_phosphate 
    
    # run the function
    d18Ow <- d18O_w_from_p(MAT, d18Op, NIST120c_mean)
    
    # print 
    paste( "Warm K (warm-season mean) d18Owater: ", round(d18Ow, 2), "per mille VSMOW")   
    
   
# Cool K Mode, Warm-season mean of 16.3 °C
    # Set MAT and d8Ophosphate inputs
    MAT <- 16.3
    d18Op <- d18Ogar_phosphate 
    
    # run the function
    d18Ow <- d18O_w_from_p(MAT, d18Op, NIST120c_mean)
    
    # print 
    paste( "Cool K (warm-season mean) d18Owater: ", round(d18Ow, 2), "per mille VSMOW")   

# Now run d18Ow on range of warm-season means -----------------------------

  # Set MAT and d8Ophosphate inputs
    MAT <- seq(16.3, 27.5, by = 0.1)
    d18Op <- min(d18Ogar_phosphate)
    
    # run the function
    d18Ow <- d18O_w_from_p(MAT, d18Op, NIST120c_mean)
    
    # plot water vs MAT range
    plot(d18Ow, MAT)
    abline(h = 24)   
    
    # print summary stats
    summary(d18Ow)
 
# Now assess d18Ow variability from gar scales -----------------------------
    
  # Warm K Mode, Warm-season mean of 27.5 °C
    # Set MAT and d8Ophosphate inputs
    MAT <- 27.5
    d18Op <- Gar_scales$d18O..VSMOW.
    
    # run the function
    gar_warmK_d18Ow <- d18O_w_from_p(MAT, d18Op, NIST120c_mean)
    d18Ow <- gar_warmK_d18Ow
    
    # plot water vs gar d18Op
    plot(d18Ow, Gar$d18O..VSMOW.)
    
    # print summary stats
    hist(d18Ow)   
    
  # Cool K Mode, Warm-season mean of 16.3 °C
    # Set MAT and d8Ophosphate inputs
    MAT <- 16.3
    d18Op <- Gar_scales$d18O..VSMOW.
    
    # run the function
    gar_coolK_d18Ow <- d18O_w_from_p(MAT, d18Op, NIST120c_mean)
    d18Ow <- gar_warmK_d18Ow
    
    # plot water vs gar d18Op
    plot(d18Ow, Gar$d18O..VSMOW.)  
    
    # print summary stats
    summary(d18Ow) 
 
  # Hold T constant at 22.3°C (dual-taxon T estimate)
  # Set MAT and d8Ophosphate inputs
    MAT <- 22.3
    d18Op <- Gar_scales$d18O..VSMOW.
    
  # run the function
    gar_22.3_d18Ow <- d18O_w_from_p(MAT, d18Op, NIST120c_mean)
    d18Ow <- gar_22.3_d18Ow
    
  # plot water vs gar d18Op
    plot(d18Ow, Gar_scales$d18O..VSMOW.)  
    
  # print summary stats
    summary(d18Ow) 
    

# Compute turtle_water for each turtle d18Op ------------------------------

# run turtle water on all Glyptops values
    Glyp$d18Owater <- 1.01 *(Glyp$d18O..VSMOW.) - 22.3 #Barrick et al. (1999)

    # one plot for turtle
    
    ggplot(data = Glyp, aes(x = d18Owater, y = Taxon)) +
      geom_point(color = "blue") +
      labs(x = "d18Owater", y = "") +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
    
# Compute croc_water for each croc G d18Op ------------------------------
  # calculate croc water
    CrocG$d18Owater <- 0.82*(CrocG$d18O..VSMOW.) - 19.93 #Amiot et al.(2007)
    
  # test your code, take a look
    plot(CrocG$d18Owater, CrocG$d18O..VSMOW.)
     summary(CrocG$d18Owater)
     hist(CrocG$d18Owater)
    # one plot for croc G
    
    ggplot(data = Glyp, aes(x = d18Owater, y = Taxon)) +
      geom_point(color = "blue") +
      labs(x = "d18Owater", y = "") +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
    

# look at the summaries together ------------------------------------------------------------
    summary(Glyp$d18Owater)
    summary(CrocG$d18Owater)
    summary(gar_coolK_d18Ow)
    summary(gar_warmK_d18Ow)

# dino water --------------------------------------------------------------
 # create function
    Herbivore_Water <- function(d18Op, h){
      (d18Op) - 26.8 + (8.9*(h))/0.76
    }  
  # set inputs
    h <- seq(0, 1, by = 0.1)
    sauro_d18O <- subset(V1075_all$d18O..VSMOW., V1075_all$Taxon %in% "Sauropods")
    d18Op <- mean(sauro_d18O)
  
  # run function 
    d18Ow <- Herbivore_Water(d18Op, h)

    plot(h, d18Ow )    

# dino phosphate experiment -----------------------------------------------
 
  # set inputs
    d18Ow <-  seq(0, -10, by = -1)
      h <-  0.5
      
      d18Ow <- -4
      h <- seq(0, 1, by = 0.1)
      
  # run function
      d18Op <- d18Ow + 26.8 - (8.9 * h) / 0.76
     
  # plot
      plot(h, d18Op)    

