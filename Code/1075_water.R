crocwaterfun <- function(d18Op) {
  result <- 0.82 * d18Op - 19.93
  return(result)
}

# Apply the function to Crocs$d18O..VSMOW and store the results in a new column
Goni$d18Owater <- crocwaterfun(Goni$d18O..VSMOW.)

# Set bottom margin to be more narrow
par(mar = c(5, 4, 4, 2) + 0.1)

# Plotting Crocs$d18O..VSMOW against Crocs$d18Owater
plot(Goni$d18O..VSMOW, Goni$d18Owater, xlab = "d18O..VSMOW", ylab = "d18Owater")


#turtles
turtwater <- 1.01 *(mean(Glyp$d18O..VSMOW.)) - 22.3 #Barrick et al. (1999)

turtwaterfun <- function(d18Op) {
  result <- 1.01 *(d18Op) - 22.3
  return(result)
}

# Apply the function to Crocs$d18O..VSMOW and store the results in a new column
Glyp$d18Owater <- turtwaterfun(Glyp$d18O..VSMOW.)

# Set bottom margin to be more narrow
par(mar = c(5, 4, 4, 2) + 0.1)

# Plotting Crocs$d18O..VSMOW against Crocs$d18Owater
plot(Glyp$d18O..VSMOW, Glyp$d18Owater, xlab = "d18O..VSMOW", ylab = "d18Owater")
