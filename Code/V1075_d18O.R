

oxydelt <- expression("δ"^18 * "O‰ (VSMOW)")

# Install and load required packages
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


# Read Data,  Create Objects, and Summarize -------------------------------

# Need to move files and directory over to Github desktop
#setwd("/Users/allen/Documents/Data Analysis/Data/Geochem")
# raw <- read.csv("vertd180/V1075_Run1_Results.csv")
# raw <- read.csv("V1075Results2.0.csv")

setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/")
raw <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")


# READ IN NIST120c STD d18Op VALUES FROM RUN 1 and RUN 2

setwd("/Users/allen/Documents/Data Analysis/Data/Geochem")
NIST120c <- read.csv("V1075_NIST120c_Run1&2.csv")
hist(NIST120c$d.18O.16O)
# identified a single outlier. Why just this one bust? Anyways, omit it.
NIST120c <- subset(NIST120c, NIST120c$d.18O.16O > 20)
hist(NIST120c$d.18O.16O)
setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
write.csv(NIST120c, "V1075_NIST120c.csv", row.names = FALSE)


NIST120c_mean <- mean(NIST120c$d.18O.16O)


head(raw)

# Moving on to something else

tax <- raw$Taxon
raw$Taxon <- as.factor(raw$Taxon)
d18O <- raw$d18O..VSMOW.
Taxa <- unique(tax)
print(Taxa)
nTaxa <- as.matrix(table(tax))

# clean data
  #eval for and remove outliers
    # test for outliers

# Define a vector of unique categories in the "Eco" column
eco_categories <- unique(raw$Eco)

# Create an empty list to store the test results
test_results <- list()



# Perform Grubbs' test for each category
for (category in eco_categories) {
  subset_data <- raw$d18O..VSMOW.[which(raw$Eco %in% category)]
  test <- grubbs.test(subset_data)
  test_results[[category]] <- test
}

# Print the results
for (category in eco_categories) {
  cat("Grubbs' Test for", category, ":\n")
  print(test_results[[category]])
  cat("\n")
}

    #test <- grubbs.test(raw$d18O..VSMOW.[which(raw$Eco %in% "Ornithischian")])
    #test
    #test <- grubbs.test(raw$d18O..VSMOW.[which(raw$Eco %in% "Aquatic Turtle")])
    #test
    #test <- grubbs.test(raw$d18O..VSMOW.[which(raw$Eco %in% "Shark")])
    #test
    #test <- grubbs.test(raw$d18O..VSMOW.[which(raw$Eco %in% "Croc A")])
    #test
    #test <- grubbs.test(raw$d18O..VSMOW.[which(raw$Eco %in% "Sauropoda")])
    #test
    #test <- grubbs.test(raw$d18O..VSMOW.[which(raw$Eco %in% "Small Theropod")])
    #test
    # outliers identified in Croc A, Small Theropod
        V1075_cl <- subset(raw, raw$d18O..VSMOW. < 22.47)
          # 2 outliers removed
  
  # remove dentine samples
    V1075_cl <- subset(V1075_cl, V1075_cl$Tissue != "dentine")
  # remove gar teeth
    V1075_cl <- subset(V1075_cl, !(Eco == "Fish" & Element.type == "tooth"))
    
    # V1075_cl <- V1075_cl[V1075_cl$Eco == "Fish" && V1075_cl$Element.type != "tooth"]
    # V1075_cl <- subset(V1075_cl, V1075_cl$Eco == 'Fish' | V1075_cl$Element.type != "tooth")
    # V1075_cl <- V1075_cl[V1075_cl$Eco == 'Fish' | V1075_cl$Element.type != "tooth",]

# Subset into new dataframes by taxon or ecotype
  
    # Define a function to calculate mean, standard error, and standard deviation
    calculate_stats <- function(data, taxon) {
      subset_data <- data[which(data$Taxon %in% taxon), ]
      mean_val <- mean(subset_data$d18O..VSMOW.)
      se_val <- sd(subset_data$d18O..VSMOW.) / sqrt(length(subset_data$d18O..VSMOW.))
      sd_val <- sd(subset_data$d18O..VSMOW.)
      return(list(mean = mean_val, se = se_val, sd = sd_val))
    }
    
    # List of taxa
    taxa_list <- list(
      Hybo = c("Hybodontiformes"),
      CrocA = c("\"Atoposauridae\" (croc. indet.)"),
      Gar = c("Lepisosteidae"),
      TurtleN = c("Naomichelys sp."),
      TheroSmall = c("Theropoda (small)"),
      Teno = c("Tenontosaurus tillettii"),
      Sauropelta = c("Sauropelta edwardsi"),
      Glyp = c("Glyptops sp."),
      TheroLarge = c("Large Theropoda"),
      Sauropoda = c("Sauropoda"),
      Goni = c("Goniopholidae"),
      Berni = c("Bernissartia sp."),
      Deino = c("Deinonychus antirrhopus"),
      Orni = c("Ornithischian")
    )
    
    # Calculate and print statistics for each taxon
    for (taxon_name in names(taxa_list)) {
      stats <- calculate_stats(V1075_cl, taxa_list[[taxon_name]])
      cat("Taxon:", taxon_name, "\n")
      cat("Mean:", stats$mean, "\n")
      cat("Standard Error:", stats$se, "\n")
      cat("Standard Deviation:", stats$sd, "\n\n")
    }
    
  table(V1075_cl$Taxon)
 unique(V1075_cl$Eco)
  
  Hybo <- subset(V1075_cl, Taxon == "Hybodontiformes")
  CrocA <- subset(V1075_cl, Taxon == "\"Atoposauridae\" (croc. indet.)")
  Gar <- subset(V1075_cl, Taxon == "Lepisosteidae")
  TurtleN <- subset(V1075_cl, Taxon == "Naomichelys sp.")
  TheroSmall <- subset(V1075_cl, Taxon == "Theropoda (small)")
  Teno <- subset(V1075_cl, Taxon == "Tenontosaurus tillettii")
  Sauropelta <- subset(V1075_cl, Taxon == "Sauropelta edwardsi")
  Glyp <- subset(V1075_cl, Taxon == "Glyptops sp.")
  TheroLarge <- subset(V1075_cl, Taxon == "Large Theropoda")
  Sauropoda <- subset(V1075_cl, Taxon == "Sauropoda")
  Goni <- subset(V1075_cl, Taxon == "Goniopholidae")
  Berni <- subset(V1075_cl, Taxon == "Bernissartia sp.")
  Deino <- subset(V1075_cl, Taxon == "Deinonychus antirrhopus")
  Orni <- subset(V1075_cl, Taxon == "Ornithischian")
  
  scales <- V1075_cl[which(V1075_cl$Element.type %in% "ganoid scale"), ]
  nrow(scales) 
  GarScales <- subset(Gar, Element.type = "ganoid scale")
  
  Hybo <- subset(V1075_cl, Taxon == "Hybodontiformes")
  allfish <- rbind(scales, Hybo)
  
  sd(V1075_cl$d18O..VSMOW.)
  
  Crocs <- rbind(CrocA, Berni, Goni)
  
  
# compute mean and n by specimen
  eco_type <- group_by(V1075_cl, Eco)
  eco_summary <- summarize(eco_type, mean = mean(d18O..VSMOW.), n = n(), min = min(d18O..VSMOW.), max = max(d18O..VSMOW.), sd = sd(d18O..VSMOW.), se = (sd(d18O..VSMOW.)/sqrt(n))) # remember that n = n() command produces column with sample size for each group
  
  write.csv(eco_summary,file='/Users/allen/Desktop/table.csv', row.names=FALSE)
  grid.table(eco_summary)
  
  ggdotchart(eco_summary, x = "Eco", y = "mean",
             sorting = "ascending",                        # Sort value in descending order
             ggtheme = theme_pubr()                        # ggplot2 theme
  )


  summary(raw$Amount)
  
  
# export data
  
  # Specify the file path where you want to save the CSV file
  file_path <- "/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data/V1075_cl.csv"
  
  # Use write.table to export the data frame to a CSV file with a different delimiter (e.g., tab-separated)
  write.table(V1075_cl, sep = ",", file = file_path, row.names = FALSE, quote = FALSE)
  
  
# ANOVA -------------------------------------------------------------------

  #install.packages("pwr")
  #library(pwr)
  
  croc.aov <- aov(Crocs$d18O..VSMOW.~Crocs$Taxon)
  summary(croc.aov)
  TukeyCrocs <- TukeyHSD(croc.aov)
  TukeyCrocs <- TukeyCrocs$`Crocs$Taxon`
  print(TukeyCrocs)

  fish.aov <- aov(allfish$d18O..VSMOW.~allfish$Taxon)
  summary(fish.aov)
  TukeyFish <- TukeyHSD(fish.aov)
  TukeyFish <- TukeyFish$`allfish$Taxon`
  print(TukeyFish)

  all.aov <- aov(V1075_cl$d18O..VSMOW.~V1075_cl$Eco)
  summary(all.aov)
  AllTukey <- TukeyHSD(all.aov)
  AT <- AllTukey$`V1075_cl$Eco`
  ATdf_pvals <- (AT[,4 ])
  mean(ATdf_pvals)
  
  
  taxa.aov <- aov(V1075_cl$d18O..VSMOW.~V1075_cl$Taxon)
  summary(taxa.aov)
  TukeyTaxa <- TukeyHSD(taxa.aov)
  print(TukeyTaxa)
  
  
  
  capture.output(summary(taxa.aov),file="TaxaANOVA.docx")
  
  stripchart(V1075_cl$d18O..VSMOW.~V1075_cl$Taxon, V1075_cl,
             col="brown3",
             vertical=TRUE,
             pch=16)
  
  stripchart(V1075_cl$d18O..VSMOW.~V1075_cl$Eva_.Sens, V1075_cl,
             ylim= c(0,30),
             col="brown3",
             vertical=TRUE,
             pch=16)
  
  
  boxplot(V1075_cl$d18O..VSMOW.~V1075_cl$Eva_.Sens, V1075_cl,
          ylim= c(0,30))
  
  evap.aov <- aov(V1075_cl$d18O..VSMOW.~V1075_cl$Eva_.Sens)
  summary(evap.aov)
  
  plot(V1075_cl$d18O..VSMOW.~V1075_cl$X.O, V1075_cl, ylim= c(0,30),
             col="brown3",
             vertical=TRUE,
             pch=16, main = "%O v. d18O")
  
  plot(V1075_cl$d18O..VSMOW.~V1075_cl$Area.28..Vs., V1075_cl, ylim= c(0,30),
             col="brown3",
             vertical=TRUE,
             pch=16, main = "Area 28 (Vs) v. d18O")
  
  plot(V1075_cl$d18O..VSMOW.~V1075_cl$Ampl.28..mV., V1075_cl, ylim= c(0,30),
             col="brown3",
             vertical=TRUE,
             pch=16, main = "Ampl. 28 (mV) v. d18O")
  
  plot(V1075_cl$d18O..VSMOW.~V1075_cl$Xtal.Wt..ug., V1075_cl, ylim= c(0,30),
             col="brown3",
             vertical=TRUE,
             pch=16, 
             main = "Xtal Wt (ug) v. d18O")
  
  
  boxplot(V1075_cl$d18O..VSMOW.~V1075_cl$Eva_.Sens, V1075_cl,
          ylim= c(0,30))
  
#Overall Summary Stats
  summary(V1075_cl$d18O..VSMOW.)
  hist(V1075_cl$d18O..VSMOW.)
  boxplot(V1075_cl$d18O..VSMOW., ylab = "d180",  main = "V1075 d18O")

# Intrasample variation
  Specimen_sd <- data.frame()

  summary(TheroLarge$d18O..VSMOW.)
  Specimen_sd[1, ] <- sd(TheroLarge$d18O..VSMOW.)

  summary(Sauropoda$d18O..VSMOW.)
  Specimen_sd[2, ] <- sd(Sauropoda$d18O..VSMOW.)

  Data60442a <- c(13.03, 13.07)
  Specimen_sd[3] <- sd(Data60442a)

  Data61286c <- c(15.46, 15.52)
  Specimen_sd[4] <- sd(Data61286c)

  Data61402a <- c(15.12, 17.13)
  Specimen_sd[5] <- sd(Data61402a)

  plot(Specimen_sd)

# Intrataxon variability

TaxoMean <- c(mean(CrocA$d18O..VSMOW.)
              ,mean(Gar$d18O..VSMOW.)
              ,mean(TurtleN$d18O..VSMOW.)
              ,mean(TheroSmall$d18O..VSMOW.)
              ,mean(Teno$d18O..VSMOW.)
              ,mean(Sauropelta$d18O..VSMOW.)
              ,mean(Glyp$d18O..VSMOW.)
              ,mean(TheroLarge$d18O..VSMOW.)
              ,mean(Sauropoda$d18O..VSMOW.)
              ,mean(Goni$d18O..VSMOW.)
              ,mean(Berni$d18O..VSMOW.)
              ,mean(Deino$d18O..VSMOW.))

TaxoSD <- c(sd(CrocA$d18O..VSMOW.)
,sd(Gar$d18O..VSMOW.)
,sd(TurtleN$d18O..VSMOW.)
,sd(TheroSmall$d18O..VSMOW.)
,sd(Teno$d18O..VSMOW.)
,sd(Sauropelta$d18O..VSMOW.)
,sd(Glyp$d18O..VSMOW.)
,sd(TheroLarge$d18O..VSMOW.)
,sd(Sauropoda$d18O..VSMOW.)
,sd(Goni$d18O..VSMOW.)
,sd(Berni$d18O..VSMOW.)
,sd(Deino$d18O..VSMOW.))
plot(TaxoSD)

TaxoList <- list(Gar, TurtleN, TheroSmall, Teno, Sauropelta, Glyp, TheroLarge, Sauropoda, Goni, Berni, Deino)

Taxo_n <- c(length(CrocA$d18O..VSMOW.)
            ,length(Gar$d18O..VSMOW.)
            ,length(TurtleN$d18O..VSMOW.)
            ,length(TheroSmall$d18O..VSMOW.)
            ,length(Teno$d18O..VSMOW.)
            ,length(Sauropelta$d18O..VSMOW.)
            ,length(Glyp$d18O..VSMOW.)
            ,length(TheroLarge$d18O..VSMOW.)
            ,length(Sauropoda$d18O..VSMOW.)
            ,length(Goni$d18O..VSMOW.)
            ,length(Berni$d18O..VSMOW.)
            ,length(Deino$d18O..VSMOW.))

plot(TaxoSD~Taxo_n, col = "blue",
     main = "SD vs. Sample Size",
     xlab = "Group Sample Size",
     ylab = "Group Standard Deviation",
     las = 1)

boxplot(TaxoSD, ylab = "Group Standard Deviation")
sd(na.omit(TaxoSD))

table(TaxoSD)

# Summaries of Particular Groups

EI <- V1075_cl[which(V1075_cl$Eva_.Sens %in% c("EI")), ]
summary(EI$d18O..VSMOW.)

ES <- V1075_cl[which(V1075_cl$Eva_.Sens %in% c("ES")), ]
summary(ES$d18O..VSMOW.)

# Compare to other datasets -----------------------------------------------


setwd("/Users/allen/Documents/Data Analysis/Data")

CMFV1075_cl <- read.csv("SuarezEtAl2014DATA.csv")
RRM <- CMFV1075_cl[which(CMFV1075_cl$site %in% c("RRM")), ]
table(RRM$Taxa)
RRM$Taxa <- as.factor(RRM$Taxa)

RRM.aov <- aov(RRM$δ18Op~RRM$Taxa)
summary(RRM.aov)

TukeyHSD(RRM.aov)

par(las=3)
par(mar = c(12,5,2,1))
boxplot(RRM$δ18Op~RRM$Taxa, RRM
        ,ylab = oxydelt
        ,xlab = "Taxon/Clade"
        ,main = "CMF(RRM) δ18Op")

boxplot(RRM$δ18Op
        ,ylab = oxydelt
        ,main = expression("CMF(RRM) " * "δ"^18 * "O from Phosphates"))

mean(RRM$δ18Op)
mean(V1075_cl$d18O..VSMOW.)

length(RRM$SAMPLE.ID)
length(V1075_cl$d18O..VSMOW.)

RRMcrocs <- RRM[which(RRM$Taxa %in% c("crocodile tooth")), ]
sd(RRMcrocs$δ18Op)

RRM_Naomy <- RRM[which(RRM$Taxa %in% c("Naomichelys sp")), ]
sd(RRM_Naomy$δ18Op)

# Aptian-Albian turtles (Suarez et al., 2020)

setwd("/Users/allen/Documents/Data Analysis/Data/Geochem")
CelinaTurtles <- read.csv("SuarezEtAl2020_AptianAlbianTurtleDATA.csv")


table(CelinaTurtles$Formation)
table(CelinaTurtles$Member)

RRCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("Ruby Ranch")), ]
LSCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("Little Sheep")), ]
HFCelinaTurts <- CelinaTurtles[which(CelinaTurtles$Member %in% c("")), ]


CelinaTurtles$Taxon <- as.factor(CelinaTurtles$Taxon)
unique(CelinaTurtles$Taxon)
CelinaGlyp <- CelinaTurtles[which(CelinaTurtles$Taxon %in% c("Glyptops")), ]
length(CelinaGlyp$Specimen..)

plot(CelinaTurtles$d18Op~CelinaTurtles$Taxon)
CelinaTurtles.aov <- aov(CelinaTurtles$d18Op~CelinaTurtles$Taxon)
summary(CelinaTurtles.aov)
CelinaTurtlesTukey <- TukeyHSD(CelinaTurtles.aov)

CelinaNaomispec <- CelinaTurtles[which(CelinaTurtles$Taxon %in% c("Naomichelys speciosa")), ]
mean(TurtleN$d18O..VSMOW.)
mean(CelinaNaomispec$d18Op)
t.test(CelinaNaomispec$d18Op, TurtleN$d18O..VSMOW.)

CelinaTrionych <- CelinaTurtles[which(CelinaTurtles$Taxon %in% c("Trionychoidea")), ]
CelinaAquaTurtsd18Op <- c(CelinaGlyp$d18Op, CelinaTrionych$d18Op)
mean(CelinaTrionych$d18Op)
t.test(CelinaTrionych$d18Op, Glyp$d18O..VSMOW.)
t.test(CelinaAquaTurtsd18Op, Glyp$d18O..VSMOW.)

MeanCelinaAquaTurtsd18O <- mean(CelinaAquaTurtsd18Op)
sd(CelinaAquaTurtsd18Op)/sqrt(length((CelinaAquaTurtsd18Op)))
sd(CelinaAquaTurtsd18Op)
CelinaturtleWater_d18O <- 1.01 *(MeanCelinaAquaTurtsd18O) - 22.3 #Barrick et al. (1999)
CelinaPhosphateTemp <- 111.37 - (4.3 * (  - CelinaturtleWater_d18O)) #Barrick et al. (1999)

# Frederickson's V1075 Goniopholid crocs (d18Oco3)

setwd("/Users/allen/Library/CloudStorage/OneDrive-UniversityofKansas/M. Allen Dissertation/Vertebrate Geochemistry/vertd180")
FredsCrocs <- read.csv("Frederickson_CloverlyCrocs_d18Oco3.csv")

FredsCrocs$δ18O_SMOW <- (FredsCrocs$δ18O)*(1.03091)+30.91
boxplot(FredsCrocs$δ18O_SMOW, ylim = c(8, 24), ylab = oxydelt, main = "Freds Cloverly(V1075) Goniopholids (n=16)")

boxplot(Goni$d18O..VSMOW., ylim = c(8, 24), ylab = oxydelt, main = "Matt's Cloverly(V1075) Goniopholids (n=14)")

t.test(FredsCrocs$δ18O_SMOW, Goni$d18O..VSMOW.)

# Frederickson's V1075 Deinonychus (d18Oco3)

setwd("/Users/allen/Documents/Data Analysis/Data")

FredsDeino <- read.csv("Frederickson_Deinonychus_d18Oco3.csv")
VPBD <- na.omit(FredsDeino$δ18O_VPB)
FredsDeino$δ18O_SMOW <- (VPBD)*(1.03091)+30.91
boxplot(FredsDeino$δ18O_SMOW, ylim = c(10, 30), ylab = oxydelt, main = "Fred's Deinonychus (CO3, n=20)")

boxplot(Deino$d18O..VSMOW., ylim = c(10, 30), ylab = oxydelt, main = "Matt's Deinonychus (phosphate, n=3)")

sd(Deino$d18O..VSMOW.)

# by formation?
boxplot(FredsDeino$δ18O_SMOW~FredsDeino$Formation, 
        ylim = c(10, 30), 
        ylab = oxydelt,
        xlab = "Formation Name", 
        main = "Fred's Deinonychus (CO3, n=20)")
t.test(FredsDeino$δ18O_SMOW, Deino$d18O..VSMOW.)

FredsCloverlyDeinos <- FredsDeino[which(FredsDeino$Formation %in% c("Cloverly")), ]
FredsAntlersDeinos <- FredsDeino[which(FredsDeino$Formation %in% c("Antlers")), ]

t.test(FredsCloverlyDeinos$δ18O_SMOW, Deino$d18O..VSMOW.)

t.test(FredsCloverlyDeinos$δ18O_SMOW, FredsAntlersDeinos$δ18O_SMOW)




# BODY TEMPS and δ18Omw ------------------------------
GarScales <- scales
crocwater <- 0.82*(mean(Goni$d18O..VSMOW.)) - 19.93 #Amiot et al.(2007)
turtwater <- 1.01 *(mean(Glyp$d18O..VSMOW.)) - 22.3 #Barrick et al. (1999)
118.7 - 4.22*((Gar$d18O..VSMOW.  +(22.6 - NIST120c_mean)) - crocwater ) #Puceat et al. (2010)







# take a look at distribution of d18Omw and temp estimates
GarScales <- scales
crocwater <- 0.82*(mean(Goni$d18O..VSMOW.)) - 19.93 #Amiot et al.(2007)
turtwater <- 1.01 *(mean(Glyp$d18O..VSMOW.)) - 22.3 #Barrick et al. (1999)
plot(118.7 - 4.22*((Gar$d18O..VSMOW.  +(22.6 - NIST120c_mean)) - crocwater )) #Puceat et al. (2010)


maxGar <- max(GarScales$d18O..VSMOW.)
minGar <- min(GarScales$d18O..VSMOW.)
maxCrocG <- max(Goni$d18O..VSMOW.)
minCrocG <- min(Goni$d18O..VSMOW.)

maxwater <- 0.82*(maxCrocG) - 19.93 #Amiot et al.(2007)
minwater <- 0.82*(minCrocG) - 19.93 #Amiot et al.(2007)

maxTemp <- 118.7 - 4.22*((maxGar  + (22.6 - NIST120c_mean)) - maxwater)
minTemp <- 118.7 - 4.22*((minGar  + (22.6 - NIST120c_mean)) - minwater)


print(EECM, TempFishCrocG, TempFishTurt)

# Install and load required packages if not already installed
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)

# Sample data for illustration (replace this with your actual data)
EECM <- 25.5
TempFishCrocG <- 28.2
TempFishTurt <- 22.8

# Create a data frame with the variables
table_data <- data.frame(EECM, TempFishCrocG, TempFishTurt)

# Create a table using kable and kableExtra with rotated column names
table <- kable(table_data, format = "html", row.names = TRUE, align = "c") %>%
  kable_styling("striped", full_width = FALSE) %>%
  column_spec(1:ncol(table_data), angle = 45)

# Print the table
print(table)






# T calcs with proper error analysis

crocwater <- 0.82*(Goni$d18O..VSMOW.) - 19.93 #Amiot et al.(2007)

d18Op_fish <- mean(GarScales$d18O..VSMOW.)

118.7 - 4.22*((GarScales$d18O..VSMOW.  + (22.6 - NIST120c_mean)) - d18Omw_crocG)

118.7 - 4.22*((d18Op_fish  + (22.6 - NIST120c_mean)) - turtwater)
118.7 - 4.22*((d18Op_fish  + (22.6 - NIST120c_mean)) - crocwater)

SE_turt <- sd(Glyp$d18O..VSMOW.)/sqrt(nrow(Glyp))
SE_croc <- sd(Goni$d18O..VSMOW.)/sqrt(nrow(Goni))
SE_NIST120c <- sd(NIST120c$d.18O.16O)/sqrt(nrow(NIST120c))
SE_fish <- sd(scales$d18O..VSMOW.)/sqrt(nrow(scales))

sqrt((64.437^2) + (36.514^2) + (-16.123^2))
table(Gar$Element.type)
GarScales <- Gar[which(Gar$Element.type %in% "ganoid scale"), ]
d_stand <- deriv(mean(NIST120c$d.18O.16O))
sig2T <- sig2turt(dT/d_d18Op_turt) + sig2crocG(dT/d_d18Op_crocG) sig2stand(dT/d_stand)




length(Goni$d18O..VSMOW.)



#temp est. per measurement
Glyp$turtleWater_d18O <- 1.01 *(Glyp$d18O..VSMOW) - 22.3
sd(Glyp$turtleWater_d18O)/sqrt(length((Glyp$turtleWater_d18O)))
sd(Glyp$turtleWater_d18O)



Gar$fishTemp <- 118.7 - 4.22*((Gar$d18O..VSMOW. +(22.6 - NIST120c_mean)) - turtleWater_d18O ) #Puceat et al. (2010) 
summary(Gar$fishTemp)
sd(Gar$fishTemp)/sqrt(length((Gar$fishTemp)))
sd(Gar$fishTemp)
boxplot(Gar$fishTemp)


#d18Ow_ALL_crocA <- 0.82*(CrocA$d18O..VSMOW.) - 19.93 #Amiot et al.(2007)
#TempFishCrocA_ALL <- 118.7 - 4.22*(fishd18Op - d18Ow_ALL_crocA ) #Puceat et al. (2010)

#d18Op_crocB <- mean(Berni$d18O..VSMOW., na.rm = TRUE)
#d18Ow_crocB <- 0.82*(d18Op_crocB) - 19.93 #Amiot et al.(2007)
#TempFishCrocB <- 118.7 - 4.22*(fishd18Op - d18Ow_crocB ) #Puceat et al. (2010)

#d18Op_crocG <- mean(Goni$d18O..VSMOW., na.rm = TRUE)
#d18Ow_crocG <- 0.82*(d18Op_crocG) - 19.93 #Amiot et al.(2007)
#TempFishCrocG <- 118.7 - 4.22*(fishd18Op - d18Ow_crocG ) #Puceat et al. (2010)
  
#Fishtmp_bxplt <- ggplot(Gar, aes(aes(y=fishTemp)))+
  geom_boxplot(fishTemp)


#calculate ingested water d18O of herbivorous dinosaurs
herbDinod18Op <- mean(c(Sauropoda$d18O..VSMOW., Sauropelta$d18O..VSMOW., Teno$d18O..VSMOW.))
herbDinoWater_d18O <- 1.41 * (herbDinod18Op) + 12.11 * (relHumidity_h) - 41.59



# #  EPSILON CALCULATIONS ---------------------------------------------------

options(digits = 20)

#SMOW isotopic RATIO (R)
Rsmow <- 2.004 *10^-3

TheroSm <- V1075_cl[which(V1075_cl$Eco %in% c("Small Theropod"))]

# All ES and EI taxa
ES_mean <- mean(ES$d18O..VSMOW.)
EI_mean <- mean(EI$d18O..VSMOW.)
R_EI <- Rsmow * (EI_mean/1000 +1)
R_ES <- Rsmow * (ES_mean/1000 +1)  
Epsi_ESvEI <- (((R_ES)/(R_EI)) - 1) * 1000

# R by taxon / eco
R_Gar <- Rsmow * (mean(Gar$d18O..VSMOW.)/1000 +1)
R_Glyp <- Rsmow * (mean(Glyp$d18O..VSMOW.)/1000 +1)
R_Goni <- Rsmow * (mean(Goni$d18O..VSMOW.)/1000 +1)
R_Srpda <- Rsmow * (mean(Sauropoda$d18O..VSMOW.)/1000 +1)
R_Berni <- Rsmow * (mean(Berni$d18O..VSMOW.)/1000 +1)
R_CrocA <- Rsmow * (mean(CrocA$d18O..VSMOW.)/1000 +1)
R_TurtleN <- Rsmow * (mean(TurtleN$d18O..VSMOW.)/1000 +1)
R_TheroSm <- Rsmow * (mean(V1075_cl$d18O..VSMOW.[which(V1075_cl$Eco %in% c("Small Theropod"))])/1000 +1)


R_Aqua <- Rsmow * (mean(c(Glyp$d18O..VSMOW.,Goni$d18O..VSMOW., Gar$d18O..VSMOW.))/1000 +1)
R_Terr <- Rsmow * (mean(c(CrocA$d18O..VSMOW.,Berni$d18O..VSMOW., Orni$d18O..VSMOW.))/1000 +1)
R_TerrWthero <- Rsmow * (mean(c(CrocA$d18O..VSMOW.,Berni$d18O..VSMOW., Orni$d18O..VSMOW., V1075_cl$d18O..VSMOW.[which(V1075_cl$Eco %in% c("Small Theropod"))] ))/1000 +1)

R_all <- c(R_Gar, R_Glyp, R_Goni, R_Srpda, R_Berni, R_CrocA, R_TurtleN)

# Epsilon
Epsi_AquaVTerr <- (((R_Terr)/(R_Aqua)) - 1) * 1000

Epsi_ESvGar <- (((R_ES)/(R_Gar)) - 1) * 1000

Epsi_SrpdaGar <- (((R_Srpda)/(R_Gar)) - 1) * 1000

Epsi_BerniGar <- (((R_Berni)/(R_Gar)) - 1) * 1000

Epsi_ESvGlyp <- (((R_ES)/(R_Glyp)) - 1) * 1000

Epsi_AquaVTerrSmThero <- (((R_TerrWthero)/(R_Aqua)) - 1) * 1000

a <- 

EpsiNames <- c("ES-EI", "ES-Gar", "CrocB-Gar", "Sauropod-Gar", "Aqua-Terr", "ES-Glyptops", "Aqua-TerrThero")
#EpsiNames <- c(expression(epsilon[ES-EI]), expression(epsilon[ES-Gar]), expression(epsilon[CrocB-Gar]), expression(epsilon[Sauropod-Gar]), expression(epsilon[Aq-Tr]), expression(epsilon[ES-Glyptops]), expression(epsilon[Aq-Tr+SmTheropods]))
EpsiData <- c(Epsi_ESvEI, Epsi_ESvGar, Epsi_BerniGar, Epsi_SrpdaGar, Epsi_AquaVTerr, Epsi_ESvGlyp, Epsi_AquaVTerrSmThero)
Epsi_all <- data.frame(EpsiData, EpsiNames)


t(Epsi_all)
options(digits = 3)
gt(Epsi_all)

oxydelt
ggplot(Epsi_all, aes(x = reorder(EpsiNames, EpsiData), EpsiData)) + 
  geom_point(size = 3, shape = 1, stroke = 2) +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text.x = element_text(size = 15, angle = -45, hjust = 0, face = "bold")) +
  theme(axis.text.y = element_text(size = 15, hjust = 0, face = "bold")) +
  theme(axis.ticks.y = element_line())+
  theme(panel.background = element_rect(colour = "black", size=2, fill=NA)) +
  theme(plot.margin = margin(20, 60, 20, 20, "pt")) +
  theme(axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(breaks = seq(0, 5, by = 0.5)) +
  labs(x="", y = "")
  
new_order <- with(Epsi_all, reorder(Epsi_all$EpsiData, Epsi_all$EpsiNames, median))
stripchart(Epsi_all$EpsiData~new_order, vertical = TRUE, pch=1)



# Estimating Water Deficit (See Blumenthal et al.,2017 appendix @ www.pnas.org/lookup/suppl/doi:10.1073/pnas.1700597114/-/DCSupplemental/pnas.1700597114.sapp.pdf)

#Giraffidae-Hippopotamidae

WD_GH <- (Epsi_AquaVTerr) * 340.9 - 1686.5

#Tragelaphini-Hippopotamidae

WD_TH <- (Epsi_AquaVTerr) * 204.3 - 766.0

#Hippotragini-Hippopotamidae

WD_HH <- (Epsi_AquaVTerr) * 316.9 - 561.4

#Tragelaphini-Elephantidae

WD_TE <- (Epsi_AquaVTerr) * 237.2 - 184.8

#Tragelaphini-Rhinoceratidae

WD_TR <- (Epsi_AquaVTerr) * 246.6 - 134.3

WD <- c(WD_GH, WD_HH, WD_TE, WD_TH, WD_TR)

boxplot(WD)
stripchart(WD)
mean(WD)

#What about other epsilon values?
(Epsi_ESvEI) * 204.3 - 766.0
(Epsi_ESvGar) * 204.3 - 766.0
(Epsi_SrpdaGar) * 204.3 - 766.0
(Epsi_ESvGlyp) * 204.3 - 766.0
(Epsi_BerniGar) * 204.3 - 766.0


# PRINT OUTS
CloverlyPAleolat <- "44.9° N"


mean(Glyp$d18O..VSMOW.)
print(GlyptopsSE)

GarSE <- sd(Gar$d18O..VSMOW)/sqrt(length((Gar$d18O..VSMOW)))
mean(Gar$d18O..VSMOW.)
print(GarSE)

BerniSE <- sd(Berni$d18O..VSMOW)/sqrt(length((Berni$d18O..VSMOW)))
mean(Berni$d18O..VSMOW.)
print(BerniSE)

# BOXPLOTS --------------------------------------------------------

# vioplot(V1075_cl$d18O..VSMOW.~V1075_cl$Taxon)


#BOXPLOT: TAXA v d18O
par(las=3)
par(mar = c(12,5,2,1))
boxplot(V1075_cl$d18O..VSMOW.~V1075_cl$Taxon, V1075_cl, xlab = NULL, ylab = oxydelt, main = oxydelt)

  # boundaries <- 
# text( x= c(1:nbGroup), y = boundaries$stats[nrow(boundaries$stats),] +0.5, paste("n = ", table()) #Trying to post sample size above each box


  
#BOXPLOT: ECO v d18O
 
table (V1075_cl$Eco)

ggplot(V1075_cl, aes(x=reorder(Eco, d18O, na.rm = TRUE), y=d18O..VSMOW.)) +
  stat_boxplot(geom ="errorbar", width = 0.3)+
  geom_boxplot(fill = "gray", alpha = 1, outlier.size=2.5) +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text.x = element_text(size = 15, angle = -45, hjust = 0, face = "bold")) +
  theme(axis.text.y = element_text(size = 15, hjust = 0, face = "bold")) +
  theme(axis.ticks.x = element_line())+
  theme(panel.background = element_rect(colour = "black", size=2, fill=NA)) +
  theme(plot.margin = margin(20, 40, 20, 20, "pt")) +
  theme(axis.title=element_text(size=20)) +
  scale_y_continuous(breaks = seq(10, 25, by = 2)) +
  labs(x="", y = oxydelt)


#BOXPLOT: Crocs


Crocs_NoOut.aov <- aov(Crocs_NoOut$d18O..VSMOW.~Crocs_NoOut$Eco)
summary(Crocs_NoOut.aov)
TukeyCrocs_NoOut <- TukeyHSD(Crocs_NoOut.aov)
TukeyCrocs_NoOut <- TukeyCrocs_NoOut$`Crocs_NoOut$Taxon`
print(TukeyCrocs_NoOut)

Crocs
#Crocs_NoOut <- Crocs[-8,] #to remove the outlier
ggplot(Crocs, aes(x=Eco, y=d18O..VSMOW.)) +
  stat_boxplot(geom ="errorbar", width = 0.3)+
  geom_boxplot(fill = "gray", alpha = 1, outlier.size=2.5) +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text.x = element_text(size = 15, angle = -45, hjust = 0, face = "bold")) +
  theme(axis.text.y = element_text(size = 15, hjust = 0,face="bold")) +
  theme(axis.ticks.x = element_line())+
  theme(axis.ticks.y = element_line())+
  theme(panel.background = element_rect(colour = "black", size=2, fill=NA)) +
  theme(plot.margin = margin(20, 20, 20, 20, "pt")) +
  theme(axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(breaks = seq(10, 25, by = 2)) +
  labs(x="", y = oxydelt) 
  # geom_text(data=Crocs %>% group_by(Eco) %>% summarise(q3 = quantile(d18O..VSMOW., 0.75),
                                                       q1 = quantile(d18O..VSMOW., 0.25),
                                                       iqr = q3 - q1,
                                                       top = min(q3 + 1.5*iqr, max(d18O..VSMOW.)), 
                                                       n=n()), 
            aes(x=Eco, y=top, label= paste0("n = ", n)), nudge_y=1, fontface = "bold", size = 5)


TukCrocNoDF <- as.data.frame(TukeyCrocs_NoOut$`Crocs_NoOut$Eco`)
TukCrocNoDF$test <- c("CrocB-CrocA", "CrocG-CrocA", "CrocG-CrocB")
TukCrocNoDF <- TukCrocNoDF[, c(5, 1, 2, 3, 4)]
gt(TukCrocNoDF)

# STRIPCHARTS -------------------------------------------------------------

#STRIPCHART: ECO vs d18O

# stripchart(V1075_cl$d18O..VSMOW.~V1075_cl$Eco, vertical = TRUE, pch=1, method="jitter")

ggplot(V1075_cl, aes(x=reorder(Eco, d18O..VSMOW., na.rm = TRUE), y=d18O..VSMOW.))+
  theme(panel.background = element_rect(color = "black", size = 2, fill = NA)) +
  geom_jitter(position=position_jitter(0), shape = 1, size = 3, stroke = 1) +
  theme(panel.grid.major.x = element_line(color = "black", size = 0.1)) +
  theme(axis.text.x = element_text(size = 15, angle = -45, hjust = 0, face = "bold")) +
  theme(axis.text.y = element_text(size = 15, hjust = 0, color = "black")) +
  theme(axis.ticks.x = element_line())+
  theme(panel.grid.major.y = element_blank())+
  theme(panel.grid.minor.y = element_blank())+
  theme(plot.margin = margin(20, 40, 20, 20, "pt")) +
  theme(axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(breaks = seq(10, 25, by = 2)) +
  labs(x="", y = oxydelt)



taxatab <- table(V1075_cl$Taxon)
print(taxatab)
table(V1075_cl$Eco)
#plot(V1075_cl$Eco, )

stripchart(V1075_cl$d18O..VSMOW.~V1075_cl$Eco, V1075_cl, xlab = NULL, ylab = "d18O ‰(VSMOW)", col="brown3",
           vertical=TRUE,
           pch=16)

V1075_cl$Specimen.ID <- as.factor(V1075_cl$Specimen.ID)

plot(V1075_cl$d18O..VSMOW.~V1075_cl$Specimen.ID)


crocs <- V1075_cl[which(V1075_cl$Eco %in% c("Croc A", "Croc B", "Croc G")), ] #why it include all Eco groups in plot?


stripchart(crocs$d18O..VSMOW.~crocs$Taxon, V1075_cl, 
           col="brown3",
           vertical=TRUE,
           pch=16)

stripchart(V1075_cl)

table(crocs$Taxon)


V1075_cl[which(V1075_cl$Taxon %in% c("Deinonychus antirrhopus")), ]

boxplot(crocs$d18O..VSMOW.~crocs$Taxon)






# HERE I AM TRYING OTHER PLOTS
ggplot(V1075_cl, aes(x=d18O..VSMOW., y=Eco)) + 
  geom_point() + 
  ggtitle("")


# using ggpubr
   mp <- ggdotchart(V1075_cl, x = "Eco", y = "d18O..VSMOW.",
             sorting = "ascending",
             ggtheme = theme_pubr())
  
  # Change the plot orientation: horizontal
  ggpar(mp, yticks.by = 1)



#install.packages('jmcm')
#meanplot(crocs$Taxon~crocs$d18O..VSMOW.)



# LATITUDINAL GRADIENTS ---------------------------------------------------

# Latitude vs MAT (see Table 1, Suarez et al. 2011)

# Leaf physiognomy Wolfe and Upchurch, 1987 <- - 0.0006 * lat2−0.2025* lat+30.25 
#LeafPhysCret <- -0.004* lat2+0.0796* lat−4.329 # Leaf physiognomy Wolfe and Upchurch, 1987; Spicer and Corfield, 1992


lat <- seq(0,90,by=1)
LatTempModern <- -0.003* (lat^2) - 0.2514 * lat+30.113 #Rozanski et al., 1993
LatTemp_CoolK  <- -0.004 * (lat^2) - 0.0586 * lat + 25.662 #Barron (1983)
LatTemp_WarmK <- -0.003 * (lat^2) + 0.0538 * lat + 28.534 #Barron (1983)
LatTemp_GENESIS_MOM <- -0.003 * (lat^2) - 0.0321 * lat + 32.514 #Zhou et al. (2008) 

LatGrads <- data.frame(lat, LatTempModern, LatTemp_CoolK, LatTemp_WarmK)
head(LatGrads)

#Kate's Antlers Data

AntPlat <- c(32, 32, 32, 32)
AntPhyTemps <- c(31,31,27,26)
AntlersPhyloTemps <- data.frame(AntPlat, AntPhyTemps)
AntlersPhyloTemps <- AntlersPhyloTemps %>% 
  rename(
    lat = AntPlat,
    temp = AntPhyTemps
  )

library(tidyverse)
df <- gather(LatGrads, key = ModelID, value = temp, 
       c("LatTempModern", "LatTemp_CoolK", "LatTemp_WarmK"))

# Suarez et al. 2021 Ruby Ranch Data

RRtemps <- c(38.6, 32.4, 31.3, 44.5, 19.8)
RRlat <- c(36.9, 36.9,36.9,36.9,36.9)
RRtempLat <- data.frame(RRtemps, RRlat)

#GGPLOT

ggplot(df, aes(lat,temp, group = ModelID, color = ModelID)) +
  geom_line(size =1)


df$ModelID <- as.factor(df$ModelID)
levels(df$ModelID)
df$ModelID <- factor(df$ModelID, levels = c("LatTempModern", "LatTemp_CoolK", "LatTemp_WarmK"))

library(dplyr)
fishTempLat <- data.frame(TempFishTurt, 45, 0)
fishTempLat$X0 <- c("gar")
fishTempLat <- fishTempLat %>% 
  rename(
    lat = X45,
    temp = TempFishTurt,
    ModelID = X0
  )


# Base R Plot

garV1075 <- fishTempLat$temp
x
y1 <- LatTempModern$temp

par(mar = c(5,6,1,1))

plot(lat, LatTempModern, las = 1, type = "l", lwd = 2, ylim = c(-10, 40), xlab = substitute(paste(bold("Latitude (°N)"))), ylab=substitute(paste(bold("T (°C)"))), font = 2, cex.lab = 2, cex.axis = 1.5) 
box(lwd =3)
legend(0,15, 
       legend = c("Modern Gradient", "Cool Cretaceous Gradient", "Warm Cretaceous Gradient", "Cretaceous Climate Model"), 
       lty = c(1,2,3,5), lwd = 2,
       box.lwd = 0,
       bg ="transparent")
legend(0,0, 
       legend = c("Fish Phosphates (This Study)", "Phyllosilicates (Andrzejewski and Tabor, 2020)"), 
       pch = 1:2,
       box.lwd = 0,
       pt.cex = 1.5)
lines(LatTemp_CoolK, lty = 2, lwd = 2 )
lines(LatTemp_WarmK, lty = 3, lwd = 2)
lines(LatTemp_GENESIS_MOM, lty = 5, lwd = 2)
points(fishTempLat$lat, fishTempLat$temp, pch = 1, lwd = 2, cex = 1.5)
points(AntlersPhyloTemps$lat, AntlersPhyloTemps$temp, pch =2, lwd = 2, cex = 1.5)
#points(RRtempLat$RRlat, RRtempLat$RRtemps, pch =3, lwd = 2, cex = 1.5)

LatTemp_CoolK  <- -0.004 * (lat^2) - 0.0586 * lat + 25.662 #Barron (1983)
LatTemp_WarmK <- -0.003 * (lat^2) + 0.0538 * lat + 28.534 #Barron (1983)
LatTemp_GENESIS_MOM <- -0.003 * (lat^2) - 0.0321 * lat + 32.514 #Zhou et al. (2008) 

# Latitude vs Meteoric water d180 (see Table 1, Suarez et al. 2011)
LatMeteoric_Modern <- -0.003* (lat^2) + 0.0595* lat - 3.699 #Rozanski et al., 1993
LatMeteoric_CoolK <- -0.005 * (lat^2) + 0.1137 * lat - 5.270 #Barron (1983)
LatMeteoric_WarmK <- -0.005 * (lat^2) + 0.1299 * lat - 4.901 #Barron (1983)
LatMeteoric_GENESIS_MOM <- -0.005* (lat^2) +0.0730* lat - 4.001 #Zhou et al. (2008)




oxydeltmw <- expression("δ"^18 * "O‰ (VSMOW)")

par(mar = c(5,6,1,1))


# plot d18Omw vs latitude with models overlain
plot(lat, LatMeteoric_Modern, type = "l", las = 1, lwd = 2, xlim = c(20, 60), ylim = c(-10, -2), xlab = substitute(paste(bold("Latitude (°N)"))), ylab=substitute(paste(bold("d18Omw"))), font = 2, cex.lab = 2, cex.axis = 1.5) 
box(lwd =3)
legend("bottomleft", 
       box.lwd = 0,
       bg = "transparent",
       legend = c("Modern", "Cool K", "Warm K", "K Climate Model"), 
       lty = c(1,2,3,5), 
       lwd = 2 )
legend("topright",
       box.lwd = 0,
       bg = "transparent",
       legend = c("Turtles (Suarez et al., 2020)", "Glyptops sp.", "'Croc A'", "'Croc B'", "'Croc G'"), 
       pch = c(4, 7, 12, 10, 9),
       pt.cex = 1.5)
lines(LatMeteoric_CoolK, lty = 2, lwd = 2 )
lines(LatMeteoric_WarmK, lty = 3, lwd = 2)
lines(LatMeteoric_GENESIS_MOM, lty = 5, lwd = 2)
points(median(LSCelinaTurts$Palaeolatitude), median(LSCelinaTurts$d18Ow), pch = 4, col="black", lwd = 2, cex = 1.5)
points(median(HFCelinaTurts$Palaeolatitude), median(HFCelinaTurts$d18Ow), pch = 4, col="black", lwd = 2, cex = 1.5)
points(median(RRCelinaTurts$Palaeolatitude), median(RRCelinaTurts$d18Ow), pch = 4, col="black", lwd = 2, cex = 1.5)
points(c(40), d18Omw_turt, pch = 7, col="black", lwd = 2, cex = 1.5)
points(40, d18Ow_crocA, pch = 12, col="black", lwd = 2, cex = 1.5)
points(40, d18Omw_crocB, pch = 10, col="black", lwd = 2, cex = 1.5)
points(40, d18Omw_crocG, pch = 9, col="black", lwd = 2, cex = 1.5)
#points(RRCelinaTurts$Palaeolatitude, RRCelinaTurts$d18Ow, pch = 1, lwd = 2, cex = 1.5)
#points(HFCelinaTurts$Palaeolatitude, HFCelinaTurts$d18Ow, pch =2, lwd = 2, cex = 1.5)

dev.off()

#TempModels <- c(LatTempModern, LatTemp_CoolK, LatTemp_WarmK, LatTemp_GENESIS_MOM)
#df <- data.frame(lat,TempModels, ModelID)
#plot(LatTempModern, ylim = c(-20, 40), xlim = c(0,90))
#points(40,fishTemp, col = "orange")
#points(40, TempFishCroc, col = "dark green")
#points(37, 31)

#points(37, 27)

