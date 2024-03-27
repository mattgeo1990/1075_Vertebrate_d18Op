d18Ow <- 0:-20

t <- 118.7 - 4.22*((14.46073  +(22.6 - 22.56714)) - d18Ow) 

plot(t,d18Ow)

# based on your empirical data and your Monte Carlo, your water value is ~ -8.5 per mille
118.7 - 4.22*((14.46073  +(22.6 - 22.56714)) - -8.5) 
  # this gives you Temp of ~ 22 °C, which is reasonable

# If your d18Ow matched the GENMOM gradient, the d18Ow @ 50.25°N would be ~ -13 per mille
-0.005* ((50.25)^2) +0.0730* 50.25 - 4.001 #Zhou et al. (2008)
# this would give you a Temp of 2.7 °C, which is unreasonable 
118.7 - 4.22*((14.46073  +(22.6 - 22.56714)) - -13) 
