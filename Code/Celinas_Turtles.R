turts <- read.csv('/Users/allen/Documents/Data Analysis/Data/Geochem/SuarezEtAl2020_TurtleDATA.csv')

# Plotting by turtle taxon
# Basic boxplot of d18O by taxon
ggplot(turts, aes(x = Taxon, y = d18Op)) +
  geom_jitter(width = 0, alpha = 0.6) +
  theme_minimal() +
  labs(
    title = expression(paste(delta^{18}, "O by Taxon")),
    x = "Taxon",
    y = expression(delta^{18}*O~"(‰ VSMOW)")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
