
# Packages ----------------------------------------------------------------

packages_to_install <- c("ggpubr", "gridExtra", "ggplot2", "knitr", "outliers", "dplyr")

if (length(setdiff(packages_to_install, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages_to_install, rownames(installed.packages())))
}

library(gridExtra)
library(ggpubr)
library(knitr)
library(ggplot2)
library(outliers)
library(dplyr)


# Read Data -------------------------------
  # Read raw sample data
    setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/")
    raw <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")

  # Read in NIST120c data ( !!! Need to recompile NIST120c data, include run 3 !!! )

    setwd("/Users/allen/Documents/Data Analysis/Data/Geochem")
    NIST120c <- read.csv("V1075_NIST120c_Run1&2.csv")
    # check for outliers
      hist(NIST120c$d.18O.16O)
    # identified a single outlier. Why just this one bust? Anyways, omit it.
      NIST120c <- subset(NIST120c, NIST120c$d.18O.16O > 20)
      hist(NIST120c$d.18O.16O)
    # looks good, now export
      setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
      write.csv(NIST120c, "V1075_NIST120c.csv", row.names = FALSE)

# Clean data --------------------------------------------------------------

  # DO NOT REMOVE OUTLIERS! YOU ARGUE THAT ALL OF YOUR DATA IS VALID
  
  # Test for and remove outliers

    # Define a vector of unique categories in the "Eco" column
      #eco_categories <- unique(raw$Eco)

    # Create an empty list to store the test results
      #test_results <- list()

    # Perform Grubbs' test for each category
      #for (category in eco_categories) {
        #subset_data <- raw$d18O..VSMOW.[which(raw$Eco %in% category)]
        #test <- grubbs.test(subset_data)
        #test_results[[category]] <- test
      #}

    # Print the results
      #for (category in eco_categories) {
        #cat("Grubbs' Test for", category, ":\n")
        #print(test_results[[category]])
        #cat("\n")
      #}

    # outliers identified in Croc A, Small Theropod
      #V1075_cl <- subset(raw, raw$d18O..VSMOW. < 22.47)
        # 2 outliers removed

  # Remove dentine samples
    #V1075_cl <- subset(V1075_cl, V1075_cl$Tissue != "dentine")
  # Remove gar teeth
    #V1075_cl <- subset(V1075_cl, !(Eco == "Fish" & Element.type == "tooth"))

  # Remove groups with n < 3
    #table(V1075_cl$Eco)
    #V1075_cl <- V1075_cl[!(V1075_cl$Eco %in% c("Large Theropod")), ]
    
# Group by Specimen -------------------------------------------------------

    # for each unique specimen in V1075, calculate mean of that specimen 
    # use dplyr to summarize by group, assign to new dataframe 
    V1075_BySpec <- group_by(raw, Specimen.ID)
    # compute mean and n by specimen
    V1075_mean <- summarize(V1075_BySpec, d18O = mean(d18O..VSMOW.), n = n()) # remember that n = n() command produces column with sample size for each group
    SD <- summarize(V1075_BySpec, SD = sd(d18O..VSMOW.))
    # clean and merge
    # condense lit_data df to one row per unique specimen
     a<- V1075_BySpec[!duplicated(V1075_BySpec$Specimen.ID), ]
     V1075_BySpec <- V1075_BySpec[!duplicated(V1075_BySpec$Specimen.ID), ]
    # remove unwanted columns
    V1075_BySpec <- dplyr::select(V1075_BySpec, c(-1, -4:-6, -8, -14:-26))
    # merge condensed lit_data with summarized data
    V1075_BySpec <- merge(V1075_BySpec,V1075_mean,by="Specimen.ID")
    V1075_BySpec <- merge(V1075_BySpec,SD,by="Specimen.ID")
    # calculate SE and merge with V1075_BySpec
    V1075_BySpec <- mutate(V1075_BySpec, SE = (SD/sqrt(n)))
    # rename columns consistent with lit_data dataset
    names(V1075_BySpec)[names(V1075_BySpec) == "Eco"] <- "eco_type"
    
# Remove d18O..VSMOW., it shouldn't be there anymore
V1075_BySpec <- V1075_BySpec %>% select(-d18O..VSMOW.)

# Look at your data (eval distributions) ----------------------------------
table(V1075_BySpec$Taxon)
gar_raw <- V1075_BySpec$d18O[which(V1075_BySpec$Taxon == "Lepisosteids")]
glyp_raw <- V1075_BySpec$d18O[which(V1075_BySpec$Taxon == "Glyptops sp.")]
crocG_raw <- V1075_BySpec$d18O[which(V1075_BySpec$Taxon == "Neosuchian G")]
NIST_raw <- NIST120c$d.18O.16O


shapiro.test(gar_raw)
shapiro.test(glyp_raw)
shapiro.test(crocG_raw)
shapiro.test(NIST_raw)

# Export Data -------------------------------------------------------------
  
  # Set WD
    setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
  
  # Export all V1075_BySpec
    V1075_BySpec <- V1075_BySpec[]
    write.csv(V1075_BySpec, "V1075_BySpec.csv", row.names = FALSE)
  
  # Remove unnecessary columns for Monte Carlo
    colnames(V1075_BySpec)
    V1075MC_data <- V1075_BySpec[, c("Specimen.ID", "Taxon", "d18O")]
  
  # Export V1075MC_data
    write.csv(V1075MC_data, "V1075MC_data.csv", row.names = FALSE)
  
  # Export just Gar, Turtle, and Croc G for dual-taxon temps
    # subset gar, aquatic turtle, and Croc G
    V1075_GarTurtleCroc <- V1075MC_data[(V1075MC_data$eco_type %in% c("Fish", "Aquatic Turtle", "Croc G")), ]
    # check that it worked
    table(V1075_GarTurtleCroc$eco_type)
    # export
    write.csv(V1075_GarTurtleCroc, "V1075_GarTurtleCroc.csv", row.names = FALSE)
    

    