
# Data, Packages ----------------------------------------------------------


# Setup -------------------------------------------------------------------


# D47  --------------------------------------------------------------------


# Temperature -----------------------------------------------------------

Tw <- 27.5


# Water-Air Correction ----------------------------------------------------

TwTa_reg_lm <- readRDS("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/TwTa_reg_lm.rds")

lm(formula = Ta_AMJJAS ~ Tw_AMJJAS, data = plus12degC_data)

0.7852*(Tw)+3.7298

