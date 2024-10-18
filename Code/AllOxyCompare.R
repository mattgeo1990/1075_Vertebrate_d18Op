
# Packages, Functions, Data -----------------------------------------------

# Packages, Functions
  oxydelt <- expression("δ"^18 * "O‰ (VSMOW)")

  # Define a list of packages to be installed and loaded
  packages <- c("tidyverse", "knitr", "ggplot2", "plotrix")
  
  # Check if each package is installed, and if not, install it
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
  }
  
  # Load the required packages
  library(knitr)
  library(ggplot2)
  library(tidyverse)
  library(plotrix)

# Load compiled Cretaceous d180p dataset
  setwd("/Users/allen/Documents/Data Analysis/Data")
  lit_data <- read.csv("Cretaceous_d18Ocompile.csv")
  lit_dataindiv <- read.csv("Cretaceous_oxyIndividuals.csv")
  lit_dataindiv$specimen <- as.factor(lit_dataindiv$specimen)

# Load Cloverly V1075 data
  # source("/Users/allen/Documents/Data Analysis/Code/V1075_d18O.R")
  setwd("/Users/allen/Documents/Data Analysis/Data/")
  # raw <- read.csv("vertd180/V1075_Run1_Results.csv")
  # V1075 <- read.csv("V1075Results2.0.csv")
  #V1075 <- read.csv("V1075_PhosphateSamples copy.csv")
  V1075 <- read.csv("V1075_PhosphateData_8-18-23_copy.csv")
  # eval for and remove outliers
    # outliers identified in Croc A, Small Theropod
      V1075 <- subset(V1075, V1075$d18O..VSMOW. < 22.47)
        # 2 outliers removed
  # remove dentine samples
      V1075 <- subset(V1075, V1075$Tissue != "dentine")
  
  
# for each unique specimen in lit_data, calculate mean of that specimen 
  # use dplyr to summarize by group, assign to new dataframe 
    BySpec <- group_by(lit_data, specimen)
  # compute mean and n by specimen
    mean <- summarize(BySpec, d18O = mean(d18O_phosphate), n = n()) # remember that n = n() command produces column with sample size for each group
    # SD <-aggregate(BySpec$d18O_phosphate, list(BySpec$specimen), FUN=sd) 
    SD <- summarize(BySpec, SD = sd(d18O_phosphate))
             
# clean and merge
  # condense lit_data df to one row per unique specimen
    lit_data_BySpec <- lit_data[!duplicated(lit_data$specimen), ]
  # remove unwanted columns
    lit_data_BySpec <- dplyr::select(lit_data_BySpec, c(-2, -7:-9))
  # merge condensed lit_data with summarized data
    lit_data_BySpec <- merge(lit_data_BySpec,mean,by="specimen")
    lit_data_BySpec <- merge(lit_data_BySpec,SD,by="specimen")
  
  # calculate SE and merge with lit_data_BySpec
  lit_data_BySpec <- mutate(lit_data_BySpec, SE = (SD/sqrt(n)))
  setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
  write.csv(lit_data_BySpec, "lit_data_BySpec.csv", row.names = FALSE)
  
  
  # Calculate sample size for each eco_type and site
  sample_size_by_eco_site <- lit_data_BySpec %>%
    group_by(eco_type, site) %>%
    summarise(Sample_Size = n())
  
  # Create the plot
  ggplot(sample_size_by_eco_site, aes(x = site, y = Sample_Size, fill = eco_type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Sample Size by Eco Type and Site",
         x = "Site",
         y = "Sample Size",
         fill = "Eco Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  

# V1075
  # for each unique specimen in V1075, calculate mean of that specimen 
    # use dplyr to summarize by group, assign to new dataframe 
     V1075_BySpec <- group_by(V1075, Specimen.ID)
    # compute mean and n by specimen
    V1075_mean <- summarize(V1075_BySpec, d18O = mean(d18O..VSMOW.), n = n()) # remember that n = n() command produces column with sample size for each group
    SD <- summarize(V1075_BySpec, SD = sd(d18O..VSMOW.))
  # clean and merge
    # condense lit_data df to one row per unique specimen
    V1075_BySpec <- V1075_BySpec[!duplicated(V1075_BySpec$Specimen.ID), ]
    # remove unwanted columns
    V1075_BySpec <- dplyr::select(V1075_BySpec, c(-1, -4:-8, -14:-26))
    # merge condensed lit_data with summarized data
    V1075_BySpec <- merge(V1075_BySpec,V1075_mean,by="Specimen.ID")
    V1075_BySpec <- merge(V1075_BySpec,SD,by="Specimen.ID")
    # calculate SE and merge with V1075_BySpec
      V1075_BySpec <- mutate(V1075_BySpec, SE = (SD/sqrt(n)))
    # rename columns consistent with lit_data dataset
      names(V1075_BySpec)[names(V1075_BySpec) == "Eco"] <- "eco_type"

      setwd("/Users/allen/Documents/GitHub/1075_Vertebrate_d18Op/Data")
      V1075_BySpec <- V1075_BySpec[]
      write.csv(V1075_BySpec, "V1075_BySpec.csv", row.names = FALSE)
      
# visualize by specimen
    # lit_data_BySpec
      ggplot(lit_data_BySpec, aes(x=n, y=SE)) + 
        geom_point() + 
        ggtitle("")

      ggplot(lit_data_BySpec, aes(x=eco_type, y=SE)) + 
        geom_point() + 
        ggtitle("")
      
      ggplot(lit_data_BySpec, aes(x=n, y=SE)) + 
        geom_point() + 
        ggtitle("")
      
      table(lit_data_BySpec$ref[lit_data_BySpec$SE >1 ])
      table(lit_data_BySpec$eco_type[lit_data_BySpec$SE >1 ])
      
    # V1075_BySpec
      # Is there higher variation in serial samples vs bulk samples?
        subset(V1075_BySpec, n > 1)
      ggplot(V1075_BySpec, aes(x=n, y=SE)) + 
        geom_point() + 
        ggtitle("")
      
      ggplot(V1075_BySpec, aes(x=eco_type, y=SE)) + 
        geom_point() + 
        ggtitle("")
      
      ggplot(V1075_BySpec, aes(x=n, y=SE)) + 
        geom_point() + 
        ggtitle("")


# Subset by site --------------------------------------------------------------------
  # look at your data
    table(lit_data_BySpec$taxon)
    table(lit_data_BySpec$eco_type)
    table(lit_data_BySpec$site)
  # change taxon data to factor data type
    lit_data_BySpec$taxon <- as.factor(lit_data_BySpec$taxon)
  # now execute subsetting by site
    # "Kuwajima Kaseki-kabe" is not included because there are no specimen #'s. This site is marine, anyways
    bateun <- subset(lit_data_BySpec, site == "Bateun el Hmaima")
    crato <- subset(lit_data_BySpec, site == "Crato Fm.")
    hollycrk <- subset(lit_data_BySpec, site == "Holly Creek")
    KhokPhaSuam <- subset(lit_data_BySpec, site == "Khok Pha Suam")
    kitadani <- subset(lit_data_BySpec, site == "Kitadani Dinosaur Quarry")
    # kuwajima <- subset(lit_data_BySpec, site == "Kuwajima Kaseki-kabe")
    cloverlyLSM <- subset(lit_data_BySpec, site == "LSM")
    romualdo <- subset(lit_data_BySpec, site == "Romualdo Fm.")
    rubyranch <- subset(lit_data_BySpec, site == "RRM")
    tamba <- subset(lit_data_BySpec, site == "Tamba")
    oldman <- subset(lit_data_BySpec, site == "Rainy Day Site")
    MM <- subset(lit_data_BySpec, site == "MM")
    uYCM <- subset(lit_data_BySpec, site == "uYCM")
    lYCM <- subset(lit_data_BySpec, site == "lYCM")
    # Here's the old way I did the above operations:
    # bateun <- lit_data_BySpec[which(lit_data_BySpec$site %in% "Bateun el Hmaima"), ]


nrow(oldman)
nrow(V1075_BySpec)

# Eval --------------------------------------------------------------------
  # taxon plots
    # Bateun el Hmaima, Early Albian, Africa (Amiot, 2006)
      ggplot(bateun, aes(x=d18O, y=eco_type)) + 
        geom_point() +
        ggtitle("Bateun el Hmaima, Early Albian, Africa (Amiot, 2006)")

    # Crato Fm, Aptian-Albian, South America (Amiot, 2010)
      ggplot(crato, aes(x=d18O, y=eco_type)) + 
        geom_point() +
        ggtitle("Crato Fm, Aptian-Albian, South America (Amiot, 2010)")

    # Holly Creek Fm, Aptian-Albian, Arkansas (Suarez et al., 2020)
      ggplot(hollycrk, aes(x=d18O, y=eco_type)) + 
        geom_point() +
        ggtitle("Holly Creek Fm, Aptian-Albian, Arkansas (Suarez et al., 2020)")

    # Khok Pha Suam, Aptian-Albian, Southeast Asia (Amiot et al., 2021)
      ggplot(KhokPhaSuam, aes(x=d18O, y=eco_type)) + 
        geom_point() +
        ggtitle("Khok Pha Suam, Aptian-Albian, Southeast Asia (Amiot et al., 2021)")
      
    # Kitadani Dinosaur Quarry, Early Aptian, Japan (Amiot et al., 2021)
        ggplot(kitadani, aes(x=d18O, y=eco_type)) + 
          geom_point() +
          ggtitle("Kitadani Dinosaur Quarry, Early Aptian, Japan (Amiot et al., 2021)")
        
    # Kuwajima Kaseki-kabe, Barremian-Aptian, Japan (Amiot et al., 2021)
      # SEE COMMENT ABOVE ABOUT Kuwajima Kaseki-kabe
      #ggplot(kuwajima, aes(x=d18O, y=eco_type)) + 
        # geom_point() +
        # ggtitle("Kuwajima Kaseki-kabe, Barremian-Aptian, Japan (Amiot et al., 2021)")
      
    # Cloverly Little Sheep Mudstone, Barremian-Aptian, Wyoming/Montana (Suarez et al., 2020)
      ggplot(cloverlyLSM, aes(x=d18O, y=eco_type)) + 
        geom_point() +
        ggtitle("Cloverly Little Sheep Mudstone, Barremian-Aptian, Wyoming/Montana (Suarez et al., 2020)")
      
    # Romualdo Fm (marine), Aptian-Albian, South America (Amiot et al., 2010)
      ggplot(romualdo, aes(x=d18O, y=eco_type)) + 
        geom_point() +
        ggtitle("Romualdo Fm (marine), Aptian-Albian, South America (Amiot et al., 2010)")

    # PLOTTING CMF DATA BOTH BY TAXON AND ECO_TYPE
      # CMF, Ruby Ranch Mbr, Aptian-Albian, Utah (Suarez et al., 2014)
        ggplot(rubyranch, aes(x=d18O, y=eco_type)) + 
          geom_point() +
          ggtitle("CMF, Ruby Ranch Mbr, Aptian-Albian, Utah (Suarez et al., 2014)")
      # CMF, Ruby Ranch Mbr, Aptian-Albian, Utah (Suarez et al., 2014)
        ggplot(rubyranch, aes(x=d18O, y=taxon)) + 
          geom_point() +
          ggtitle("CMF, Ruby Ranch Mbr, Aptian-Albian, Utah (Suarez et al., 2014)")

    # And now we resume our normal programming for the rest of the sites (d18O x eco_type)
    # Tamba, Early Albian, Japan (Amiot et al., 2021)
      ggplot(tamba, aes(x=d18O, y=eco_type)) + 
        geom_point() +
        ggtitle("Tamba, Early Albian, Japan (Amiot et al., 2021)")

    # Oldman Fm, Campanian, Alberta (Cullen et al., 2020)
      ggplot(oldman, aes(x=d18O, y=taxon)) + 
        geom_point() + 
        ggtitle("Oldman Fm, Campanian, Alberta (Cullen et al., 2020)")
      
    # Mussentuchit Mbr, Cedar Mtn Fm, Cenomanian, Utah (Suarez et al., 2014)
      ggplot(MM, aes(x=d18O, y=taxon)) + 
        geom_point() + 
        ggtitle("Mussentuchit Mbr, Cedar Mtn Fm, Cenomanian, Utah (Suarez et al., 2014)")
      
    # upper Yellow Cat Mbr, Cedar Mtn Fm, Early K, Utah (Suarez et al., 2014)
      ggplot(uYCM, aes(x=d18O, y=taxon)) + 
        geom_point() + 
        ggtitle("upper Yellow Cat Mbr, Cedar Mtn Fm, Early K, Utah (Suarez et al., 2014)")
      
    # lower Yellow Cat Mbr, Cedar Mtn Fm, Early K, Utah (Suarez et al., 2014)
      ggplot(lYCM, aes(x=d18O, y=taxon)) + 
        geom_point() + 
        ggtitle("lower Yellow Cat Mbr, Cedar Mtn Fm, Early K, Utah (Suarez et al., 2014)")
      
    # Himes Mbr, Cloverly Fm, Albian-Cenomanian, Montana (1075 data)
      ggplot(V1075, aes(x=d18O..VSMOW., y=Eco)) + 
        geom_point() + 
        ggtitle("Himes Mbr, Cloverly Fm, Albian-Cenomanian, Montana (1075 data)")

  # summary stats
    # range
      diff(range(V1075_BySpec$d18O))
      diff(range(MM$d18O))
      diff(range(kitadani$d18O))
      diff(range(lYCM$d18O))
      diff(range(uYCM$d18O))
      diff(range(rubyranch$d18O))
    # number of taxa
      length(unique(V1075_BySpec$Taxon))
        length(unique(V1075_BySpec$eco_type))
      length(unique(MM$taxon)) # Celina coded the taxon names inconsistently, clean it up 
        length(unique(MM$eco_type))

    diff(range(MM$d18O[which(MM$eco_type %in% "theropod")]))   
    diff(range(lYCM$d18O[which(lYCM$eco_type %in% "theropod")]))   
    diff(range(uYCM$d18O[which(uYCM$eco_type %in% "theropod")]))   
    diff(range(rubyranch$d18O[which(rubyranch$eco_type %in% "theropod")]))   
    diff(range(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Small Theropod")]))   

# SD <-aggregate(lit_data$d18O, list(lit_data$specimen), FUN=sd) 
#SDE <-aggregate(lit_data$d18O_phosphate, list(lit_data$specimen), FUN=s) 

#ggplot(SD, aes(x=Group.1, y=x)) + 
  #geom_point()
#table(lit_data$specimen)

#SD[which(SD$x > "2"), ]
#lit_data$taxon[which(SD$x > "2")]
#lit_data$ref[which(SD$x > "2")]

#hist(SD$x)

#rrSD <-aggregate(rubyranch$d18O_phosphate, list(rubyranch$specimen), FUN=sd) 









# calculate aggregates
#create new dataframe, each row is a taxon at a site
taxsite <- data.frame()
#compute aggregates
#mean


# writing a function to create taxon-site dataframes for each site and then combine them

taxsite <- function(data) {
  #mean
  taxmean <- aggregate(data$d18O, list(data$eco_type), FUN=mean)
  colnames(taxmean)[colnames(taxmean) == "x"] = "mean"
  
  #sample size
  n <- data.frame(table(data$eco_type))
  colnames(n)[1] = "Group.1"
  colnames(n)[2] = "n"
  taxmean <- merge(n, taxmean, by = "Group.1")
  
  #standard deviation
  taxSD <- aggregate(data$d18O, list(data$eco_type), FUN=sd)
  taxmean <- merge(taxSD, taxmean, by = "Group.1")
  colnames(taxmean)[colnames(taxmean) == "x"] = "SD"
  
  #standard error
  taxmean <- mutate(taxmean, SE = (SD/sqrt(n)))
  
  #rename eco_type column
  colnames(taxmean)[colnames(taxmean) == "Group.1"] = "eco_type"
  
  #other data
  other_cols <- data[!duplicated(data$eco_type), ] 
  other_cols <- other_cols[c('eco_type', 'age', 'formation', 'strat_mbr', 'site', 'continent', 'paleolat', 'paleolng', 'ref')]
  merge(taxmean, other_cols, by = "eco_type")
  
  }

# now run taxsite on each site from lit_data
    bateun_taxmean <- taxsite(bateun)
    crato_taxmean <- taxsite(crato)
    hollycrk_taxmean <- taxsite(hollycrk)
    KhokPhaSuam_taxmean <- taxsite(KhokPhaSuam)
    kitadani_taxmean <- taxsite(kitadani)
    cloverlyLSM_taxmean <- taxsite(cloverlyLSM)
    romualdo_taxmean <- taxsite(romualdo)
    rubyranch_taxmean <- taxsite(rubyranch)
    tamba_taxmean <- taxsite(tamba)
    oldman_taxmean <- taxsite(oldman)
    MM_taxmean <- taxsite(MM)
    uYCM_taxmean <- taxsite(uYCM)
    lYCM_taxmean <- taxsite(lYCM)

# combine taxsite df's
    lit_data_taxmean <- rbind(bateun_taxmean, 
                         crato_taxmean, 
                         hollycrk_taxmean, 
                         KhokPhaSuam_taxmean, 
                         kitadani_taxmean, 
                         cloverlyLSM_taxmean,
                         romualdo_taxmean,
                         rubyranch_taxmean,
                         tamba_taxmean,
                         oldman_taxmean,
                         MM_taxmean,
                         uYCM_taxmean,
                         lYCM_taxmean)  

# Apply taxsite function to V1075 to compute stats for each eco_type
  # taxsite function not working here because of formatting diffs. 
  # Run taxsite code manually on V1075
    # mean
      taxmean <- aggregate(V1075_BySpec$d18O, list(V1075_BySpec$eco_type), FUN=mean)
      colnames(taxmean)[colnames(taxmean) == "x"] = "mean"
    # sample size
      n <- data.frame(table(V1075_BySpec$eco_type))
      colnames(n)[1] = "Group.1"
      colnames(n)[2] = "n"
      taxmean <- merge(n, taxmean, by = "Group.1")
    # standard deviation
      taxSD <- aggregate(V1075_BySpec$d18O, list(V1075_BySpec$eco_type), FUN=sd)
      taxmean <- merge(taxSD, taxmean, by = "Group.1")
      colnames(taxmean)[colnames(taxmean) == "x"] = "SD"
    # standard error
      taxmean <- mutate(taxmean, SE = (SD/sqrt(n)))
    # rename eco_type column
      colnames(taxmean)[colnames(taxmean) == "Group.1"] = "eco_type"
    # format to match lit_data_BySpec columns
      other_cols <- data.frame(matrix(ncol = 19, nrow = 11))
      colnames(other_cols) <- colnames(lit_data_BySpec)
      V1075_taxmean <- cbind(taxmean, other_cols)
      tax_cols <- colnames(lit_data_taxmean)
      V1075_taxmean <- subset(V1075_taxmean, select=c(tax_cols))
      V1075_taxmean$age <- "Albian"
      V1075_taxmean$formation <- "Cloverly"
      V1075_taxmean$strat_mbr <- "Himes"
      V1075_taxmean$site <- "V1075"
      V1075_taxmean$ref <- "Allen2023"
      # from previous attempts:
        # other_cols <- V1075_BySpec[!duplicated(V1075_BySpec$eco_type), ] 
        # other_cols <- dplyr::select(other_cols, c(-3:-4, -7:-11))
        # other_cols <- other_cols[-3:-4, -7:-11]
        # other_cols <- other_cols[c('eco_type', 'OMNH.Item..', 'Eva_.Sens', 'Element.type', 'Tissue'), ]
      

# combine taxsite df's
all_taxmean <- rbind(V1075_taxmean,
                     bateun_taxmean, 
                     crato_taxmean, 
                     hollycrk_taxmean, 
                     KhokPhaSuam_taxmean, 
                     kitadani_taxmean, 
                     cloverlyLSM_taxmean,
                     romualdo_taxmean,
                     rubyranch_taxmean,
                     tamba_taxmean,
                     oldman_taxmean,
                     MM_taxmean,
                     uYCM_taxmean,
                     lYCM_taxmean)









# sauropods
  # format V1075 sauropod eco_type
    table(V1075_taxmean$eco_type)
    all_taxmean$eco_type[all_taxmean$eco_type == "Sauropoda"] <- "sauropod"
  # create object, plot
    sauropods <- subset(all_taxmean, eco_type == "sauropod")
    ggplot(sauropods, aes(x=mean, y=site)) + 
      geom_point() + 
      ggtitle("Cretaceous sauropods")

# theropods
    # format V1075 theropod eco_type
      table(V1075_taxmean$eco_type)
      all_taxmean$eco_type[which(all_taxmean$eco_type %in% c("Small Theropod", "Large Theropod"))] <- "theropod"
      table(all_taxmean$eco_type)
    # create object, plot
      theropods <- subset(all_taxmean, eco_type == "theropod")
      ggplot(theropods, aes(x=mean, y=site)) + 
        geom_point() + 
        ggtitle("Cretaceous Theropods")

# playing visuals with summary data
      ggplot(all_taxmean, aes(x=n, y=SE)) + 
        geom_point() + 
        ggtitle("") 
      

      ggplot(all_taxmean, aes(x=SE, y=ref)) + 
        geom_point() + 
        ggtitle("")
      
      ggplot(all_taxmean, aes(x=n, y=SD)) + 
        geom_point() + 
        ggtitle("")
      
      ggplot(all_taxmean, aes(x=SE, y=eco_type)) + 
        geom_point() + 
        ggtitle("")
      
      ggplot(all_taxmean, aes(x=SD, y=SE)) + 
        geom_point() + 
        ggtitle("")

      ggplot(all_taxmean, aes(x=SE, y=site)) + 
        geom_point() + 
        ggtitle("")
      
      ggplot(all_taxmean, aes(x = SE, y = site, size = n)) +
        geom_point() +
        ggtitle("Variability by Site in Cretaceous Studies")

      ggplot(all_taxmean, aes(x = SE, y = paste(site, " (", ref, ")", sep = ""), size = n)) +
        geom_point() +
        ggtitle("Variability by Site Assemblage in Cretaceous Studies") +
        scale_size_continuous(limits = c(0, 60)) +
        ylab("Assemblage")
      
      
table(all_taxmean$ref[all_taxmean$SE >1 ])
table(all_taxmean$eco_type[all_taxmean$SE >1 ])

ggplot(V1075_taxmean, aes(x=SE, y=eco_type)) + 
  geom_point() + 
  ggtitle("")

ggplot(all_taxmean, aes(x=n, y=eco_type)) + 
  geom_point() + 
  ggtitle("")

#creating new taxon-site dataframe

#mean
bateun_taxmean <- aggregate(bateun$d18O, list(bateun$eco_type), FUN=mean)
colnames(bateun_taxmean)[colnames(bateun_taxmean) == "x"] = "mean"

#sample size
bateun_n <- data.frame(table(bateun$eco_type))
colnames(bateun_n)[1] = "Group.1"
colnames(bateun_n)[2] = "n"
bateun_taxmean <- merge(bateun_n, bateun_taxmean, by = "Group.1")

#standard deviation
bateun_taxSD <- aggregate(bateun$d18O, list(bateun$eco_type), FUN=sd)
bateun_taxmean <- merge(bateun_taxSD, bateun_taxmean, by = "Group.1")
colnames(bateun_taxmean)[colnames(bateun_taxmean) == "x"] = "SD"

#standard error
bateun_taxmean <- mutate(bateun_taxmean, SE = (SD/sqrt(n)))

#site column
bateun_taxmean$site <- "Bateun"

#rename taxon column
colnames(bateun_taxmean)[colnames(bateun_taxmean) == "Group.1"] = "eco_type"







crato_taxmean <- aggregate(crato$d18O, list(crato$taxon), FUN=mean)
hollycrk_taxmean <- aggregate(hollycrk$d18O, list(hollycrk$taxon), FUN=mean)
KhokPhaSuam_taxmean <- aggregate(KhokPhaSuam$d18O, list(KhokPhaSuam$taxon), FUN=mean)
kitadani_taxmean <- aggregate(kitadani$d18O, list(kitadani$taxon), FUN=mean)
cloverlyLSM_taxmean <- aggregate(cloverlyLSM$d18O, list(cloverlyLSM$taxon), FUN=mean)
romualdo_taxmean <- aggregate(romualdo$d18O, list(romualdo$taxon), FUN=mean)
rubyranch_taxmean <- aggregate(rubyranch$d18O, list(rubyranch$taxon), FUN=mean)
tamba_taxmean <- aggregate(tamba$d18O, list(tamba$taxon), FUN=mean)
oldman_taxmean <- aggregate(oldman$d18O, list(oldman$taxon), FUN=mean)
MM_taxmean <- aggregate(MM$d18O, list(MM$taxon), FUN=mean)
uYCM_taxmean <- aggregate(uYCM$d18O, list(uYCM$taxon), FUN=mean)
lYCM_taxmean <- aggregate(lYCM$d18O, list(lYCM$taxon), FUN=mean)

mean2 <- aggregate(lit_data_BySpec$d18O, list(lit_data_BySpec$taxon), FUN=mean)
SD <-aggregate(lit_data_BySpec$d18O, list(lit_data_BySpec$specimen), FUN=sd) 
SE <-aggregate(lit_data_BySpec$d18O, list(lit_data_BySpec$specimen), FUN=std.error) 
# merge aggregate df's into taxsite dataframe
taxsite <- merge(mean2, SD, by = "Group.1")
taxsite <- merge(taxsite, SE, by = "Group.1")
#rename columns
colnames(taxsite)[1] <- "specimen"
colnames(taxsite)[2] <- "mean"
colnames(taxsite)[3] <- ""
colnames(taxsite)[4] <- ""


# merge dataframes???
names(SD)[1] <- "specimen"
left_lit_dataindiv <- left_join(lit_dataindiv, SD, by = "specimen")
names(left_lit_dataindiv)[names(left_lit_dataindiv) == "x"] <- "sd_d18O"

names(SE)[1] <- "specimen"
left_lit_dataindiv <- left_join(left_lit_dataindiv, SE, by = "specimen")
names(left_lit_dataindiv)[names(left_lit_dataindiv) == "x"] <- "se_d18O"

names(mean)[1] <- "specimen"
left_lit_dataindiv <- left_join(left_lit_dataindiv, mean, by = "specimen")
names(left_lit_dataindiv)[names(left_lit_dataindiv) == "x"] <- "mean_d18O"

# create subsets by site

rubyranch <- subset(left_lit_dataindiv, site == "RRM",
            select = c(taxon, eco_type, mean_d18O))

cloverlyLSM <- subset(left_lit_dataindiv, site == "LSM",
                      select = c(taxon, eco_type, mean_d18O))

uYCM <- subset(left_lit_dataindiv, site == "uYCM",
               select = c(taxon, eco_type, mean_d18O))

lYCM <- subset(left_lit_dataindiv, site == "lYCM",
               select = c(taxon, eco_type, mean_d18O))

MM <- subset(left_lit_dataindiv, site == "MM",
             select = c(taxon, eco_type, mean_d18O))


bateun <- lit_data[which(lit_data$site %in% "Bateun el Hmaima"), ]
crato <- lit_data[which(lit_data$site %in% "Crato Fm."), ]
hollycrk <- lit_data[which(lit_data$site %in% "Holly Creek"), ]
KhokPhaSuam <- lit_data[which(lit_data$site %in% "Khok Pha Suam"), ]
kitadani <- lit_data[which(lit_data$site %in% "Kitadani Dinosaur Quarry"), ]
kuwajima <- lit_data[which(lit_data$site %in% "Kuwajima Kaseki-kabe"), ]

romualdo <- lit_data[which(lit_data$site %in% "Romualdo Fm."), ]

tamba <- lit_data[which(lit_data$site %in% "Tamba"), ]
oldman <- lit_data[which(lit_data$site %in% "Rainy Day Site"), ]



# plot the means by taxon or eco_type

# lower Yellow Cat Mbr, Cedar Mtn Fm, Early K, Utah (Suarez et al., 2014)
ggplot(lYCM, aes(x=mean_d18O, y=eco_type)) + 
  geom_point() +
  ggtitle("lower Yellow Cat Mbr, Cedar Mtn Fm, Early K, Utah (Suarez et al., 2014)")

# upper Yellow Cat Mbr, Cedar Mtn Fm, Early K, Utah (Suarez et al., 2014)
ggplot(uYCM, aes(x=mean_d18O, y=taxon)) + 
  geom_point() +
  ggtitle("upper Yellow Cat Mbr, Cedar Mtn Fm, Early K, Utah (Suarez et al., 2014)")

# CMF, Ruby Ranch Mbr, Aptian-Albian, Utah (Suarez et al., 2014)
ggplot(rubyranch, aes(x=mean_d18O, y=taxon)) + 
  geom_point() +
  ggtitle("CMF, Ruby Ranch Mbr, Aptian-Albian, Utah (Suarez et al., 2014)")

# Mussentuchit Mbr, Cedar Mtn Fm, Cenomanian, Utah (Suarez et al., 2014)
ggplot(MM, aes(x=mean_d18O, y=taxon)) + 
  geom_point() + 
  ggtitle("Mussentuchit Mbr, Cedar Mtn Fm, Cenomanian, Utah (Suarez et al., 2014)")

# Cloverly Little Sheep Mudstone, Barremian-Aptian, Wyoming/Montana (Suarez et al., 2020)
ggplot(cloverlyLSM, aes(x=mean_d18O, y=taxon)) + 
  geom_point() +
  ggtitle("Cloverly Little Sheep Mudstone, Barremian-Aptian, Wyoming/Montana (Suarez et al., 2020)")




bateun <- lit_data[which(lit_data$site %in% "Bateun el Hmaima"), ]
crato <- lit_data[which(lit_data$site %in% "Crato Fm."), ]
hollycrk <- lit_data[which(lit_data$site %in% "Holly Creek"), ]
KhokPhaSuam <- lit_data[which(lit_data$site %in% "Khok Pha Suam"), ]
kitadani <- lit_data[which(lit_data$site %in% "Kitadani Dinosaur Quarry"), ]
kuwajima <- lit_data[which(lit_data$site %in% "Kuwajima Kaseki-kabe"), ]
cloverlyLSM <- lit_data[which(lit_data$site %in% "LSM"), ]
romualdo <- lit_data[which(lit_data$site %in% "Romualdo Fm."), ]
rubyranch <- lit_data[which(lit_data$site %in% "RRM"), ]
tamba <- lit_data[which(lit_data$site %in% "Tamba"), ]
oldman <- lit_data[which(lit_data$site %in% "Rainy Day Site"), ]
MM <- lit_data[which(lit_data$site %in% "MM"), ]
uYCM <- lit_data[which(lit_data$site %in% "uYCM"), ]
lYCM <- lit_data[which(lit_data$site %in% "lYCM"), ]


lit_data_BySpec[which(lit_data_BySpec$SE > 1),]





# Ecology of theropods and sauropods

allSmallThero <- V1075[which(V1075$Eco %in% "Small Theropod"), ]

hist(allSmallThero$d18O..VSMOW., breaks = 6)
table(V1075$Eco)
table(V1075_BySpec$eco_type)

# test for outliers
install.packages("outliers")
library(outliers)
test <- grubbs.test(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Small Theropod")])
test
test <- grubbs.test(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Croc A")])
test
test <- grubbs.test(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Sauropoda")])
test

# calculate range for each V1075 eco_type
diff(range(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Small Theropod")]))
diff(range(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Sauropoda")]))
diff(range(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Fish")]))
diff(range(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Croc A")]))
diff(range(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Croc B")]))
diff(range(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Croc G")]))
diff(range(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Aquatic Turtle")]))
diff(range(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "Terrestrial Turtle")]))


# Explore Data ------------------------------------------------------------

table(all_taxmean$ref)
table(all_taxmean$eco_type[which(all_taxmean$ref %in% "Suarez et al., 2014")])

table(all_taxmean$site)

lYCM_taxmean[which(all_taxmean$ref =)]


max(V1075_BySpec$d18O[which(V1075_BySpec$eco_type %in% "fish")])




# V1075
# for each ecogroup in V1075, calculate mean
# use dplyr to summarize by group, assign to new dataframe 
V1075_ByEco <- group_by(V1075, Eco)
# compute mean and n by specimen
Ecomean <- summarize(V1075_ByEco, d18O = mean(d18O..VSMOW.), n = n()) # remember that n = n() command produces column with sample size for each group
SD <- summarize(V1075_ByEco, SD = sd(d18O..VSMOW.))
# clean and merge
# condense df to one row per ecotype
V1075_ByEco <- V1075_ByEco[!duplicated(V1075_ByEco$Eco), ]
# remove unwanted columns
V1075_ByEco <- dplyr::select(V1075_ByEco, c(-1:-10, -12:-26))
# merge condensed lit_data with summarized data
V1075_ByEco <- merge(V1075_ByEco,Ecomean,by = "Eco")
V1075_ByEco <- merge(V1075_ByEco,SD,by="Eco")
# calculate SE and merge with V1075_BySpec
V1075_ByEco <- mutate(V1075_ByEco, SE = (SD/sqrt(n)))
# rename columns consistent with lit_data dataset
names(V1075_ByEco)[names(V1075_ByEco) == "Eco"] <- "eco_type"

ggplot(V1075_ByEco, aes(x=SE, y=eco_type)) + 
  geom_point() + 
  ggtitle("SE by Eco_type: V1075")


# Create a histogram for each site
ggplot(lit_data_BySpec, aes(x = d18O)) +
  geom_histogram(binwidth = 1, fill = "blue") +
  facet_wrap(~site, ncol = 2) +
  labs(title = "Histogram of d18O by Site",
       x = "d18O",
       y = "Frequency")


ggplot(lit_data_BySpec, aes(x = site, y = d18O, fill = site)) +
  geom_violin() +
  labs(title = "Violin Plot of d18O by Site",
       x = "Site",
       y = "d18O") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels by 45 degrees

summary(V1075$d18O..VSMOW.)



# plot sample size by eco_type and site for lit data

  # Calculate sample size for each eco_type
  sample_size_by_eco <- V1075_BySpec %>%
    group_by(eco_type) %>%
    summarise(Sample_Size = n())
  
  # Create the plot
  ggplot(sample_size_by_eco, aes(x = eco_type, y = Sample_Size)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Sample Size by Eco Type",
         x = "Eco Type",
         y = "Sample Size") +
    theme_minimal()
  
# plot sample size by eco_type and site for 1075 data
  
  # Calculate sample size for each eco_type
  sample_size_by_eco <- V1075_BySpec %>%
    group_by(eco_type) %>%
    summarise(Sample_Size = n())
  
  # Create the plot
  ggplot(sample_size_by_eco, aes(x = eco_type, y = Sample_Size)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Sample Size by Eco Type",
         x = "Eco Type",
         y = "Sample Size") +
    theme_minimal()

# histograms of d18O by eco_type
  # Plotting a panel of histograms
  ggplot(V1075_BySpec, aes(x = d18O, fill = eco_type)) +
    geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
    labs(title = "Histogram of d18O by eco_type",
         x = "d18O",
         y = "Frequency") +
    theme_minimal() +
    facet_wrap(~eco_type, scales = "free")
  
  
  # Perform Shapiro-Wilk test for normality for each eco_type
  shapiro_tests <- by(V1075_BySpec$d18O, V1075_BySpec$eco_type, shapiro.test)
  
  # Display normality test results
  cat("\nNormality Tests:\n")
  print(shapiro_tests)
  
  # Extract p-values from the test results
  p_values <- sapply(shapiro_tests, function(x) x$p.value)
  
  # Highlight non-normally distributed groups (p-value < 0.05)
  non_normal_groups <- names(p_values[p_values < 0.05])
  cat("\nNon-normally distributed groups:", ifelse(length(non_normal_groups) > 0, paste(non_normal_groups, collapse = ", "), "None"), "\n")

# Filter data for "Small Theropod" eco_type
  small_theropod_data <- subset(V1075_BySpec, eco_type == "Small Theropod")
  
# Plotting histogram for "Small Theropod" eco_type
  ggplot(small_theropod_data, aes(x = d18O)) +
    geom_histogram(binwidth = 1, color = "black", fill = "lightblue", alpha = 0.7) +
    labs(title = "Histogram of d18O for Small Theropod",
         x = "d18O",
         y = "Frequency") +
    theme_minimal()
# pub visuals -------------------------------------------------------------

V1075_ByEco


  # Subset data for site = LSM
  subset_data <- subset(lit_data, site == "LSM")
  
  # Make stripchart of d18O by taxon
  stripchart(d18O_phosphate ~ taxon, data = subset_data, 
             xlab = "Taxon", ylab = "d18O", 
             main = "d18O by Taxon at Site LSM",
             method = "stack")

  ggplot(subset_data, aes(x = d18O_phosphate, y = taxon)) +
    geom_point() +
    labs(x = "d18O", y = "Taxon", title = "Taxon by d18O at Site LSM")  

  