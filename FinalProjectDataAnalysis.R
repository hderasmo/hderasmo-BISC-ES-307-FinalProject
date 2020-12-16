#Author: Hope D'Erasmo
#Date last modified: 12/16/2020
#Title: Able to Flourish or No-Longer Needed: Investigating
#Biomass of Fine Roots in Relation to Soil Nutrient Levels in 
#NEON Field Sites

#Load needed libraries
library(neonUtilities)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(corrplot)
library(Hmisc)
library(factoextra)


##Load NEON Site data
neon_sites <- read_csv("NEON-field-sites.csv")

###Load the soils data
soilMegapit_data <- loadByProduct(dpID = "DP1.00096.001", check.size = F)
list2env(soilMegapit_data, .GlobalEnv)

#Filter for regular samples only
#Mutate to add a column for simple horizon name
#Filter for only the A horizon 

mgp_nutrient <- mgp_perbiogeosample %>%
  filter(biogeoSampleType == "Regular", .preserve = FALSE) %>%
  mutate(horizonNameSimple = str_extract(horizonName, "[A-Z]{1}")) %>%
  filter(horizonNameSimple == "A", .preserve = FALSE)

#Left join with NEON site information (NEON-field-sites.csv)
mgp_nutrientSite <- left_join(mgp_nutrient, neon_sites, 
                              by = c("domainID","siteID"))

#Select only the variables of interest
#(siteID, pitNamedLocation, nitrogenTot, pHjelm, sulfurTot) and group by siteID
mgp_nutrientSiteMean <- mgp_nutrientSite %>%
  select(siteID, nitrogenTot, pMjelm, sulfurTot, pitNamedLocation) %>%
  group_by(siteID) %>%
  summarise(.groups = "keep",
              meanSiteN = mean(nitrogenTot), meanSiteP =
              mean(pMjelm), meanSiteS = mean(sulfurTot))

###Load the root data
rootMegapit_data <- loadByProduct(dpID = "DP1.10066.001", check.size = F)
list2env(rootMegapit_data, .GlobalEnv)

#Mutate to add a column for siteID
#Filter only for fine roots (sizeCategory == "<=2mm")  
#Select only variables of interest (siteID, pitNamedLocation, sizeCategory, 
#rootDryMass) 

mpr_fineRoot <- mpr_perrootsample %>%
  mutate(siteID = str_extract(sampleID, "[A-Z]{1,4}")) %>%
  filter(sizeCategory == "<=2mm") %>%
  select(siteID, pitNamedLocation, sizeCategory, rootDryMass)

#Summarize to create a sum of rootDryMass for each megapit

mpr_fineRootTot <- mpr_fineRoot %>%
  group_by(siteID) %>%
  summarise(meanFineRootMass = mean(rootDryMass))

##Full_join the two dataframes by siteID

nutrient_rootDF <- left_join(mpr_fineRootTot, mgp_nutrientSiteMean,
                             by = c("siteID")) %>%
  filter(is.na(meanSiteN) == FALSE)

##Add in a column for dominant ecosystem type 
#This is the NLCD Class listed first in the NEON-filed-sites.csv file

Time <- length(nutrient_rootDF$siteID)
nutrient_rootDF$NLCDClass <- numeric()

for (t in 1:Time){
  if (nutrient_rootDF$siteID[t] == "BART"){
    
    nutrient_rootDF$NLCDClass[t] <- "Deciduous Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "BLAN"){
    
    nutrient_rootDF$NLCDClass[t] <- "Deciduous Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "CLBJ"){
    
    nutrient_rootDF$NLCDClass[t] <- "Deciduous Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "CPER"){
    
    nutrient_rootDF$NLCDClass[t] <- "Emergent Herbaceous Wetlands"
  }
  
  else if (nutrient_rootDF$siteID[t] == "DCFS"){
    
    nutrient_rootDF$NLCDClass[t] <- "Grassland/Herbaceous"
  }
  
  else if (nutrient_rootDF$siteID[t] == "DELA"){
    
    nutrient_rootDF$NLCDClass[t] <- "Evergreen Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "DSNY"){
    
    nutrient_rootDF$NLCDClass[t] <- "Pasture/Hay"
  }
  
  else if (nutrient_rootDF$siteID[t] == "HARV"){
    
    nutrient_rootDF$NLCDClass[t] <- "Deciduous Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "HEAL"){
    
    nutrient_rootDF$NLCDClass[t] <- "Dwarf Scrub"
  }
  
  else if (nutrient_rootDF$siteID[t] == "JERC"){
    
    nutrient_rootDF$NLCDClass[t] <- "Cultivated Crops"
  }
  
  else if (nutrient_rootDF$siteID[t] == "JORN"){
    
    nutrient_rootDF$NLCDClass[t] <- "Shrub/Scrub"
  }
  
  else if (nutrient_rootDF$siteID[t] == "MLBS"){
    
    nutrient_rootDF$NLCDClass[t] <- "Deciduous Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "MOAB"){
    
    nutrient_rootDF$NLCDClass[t] <- "Evergreen Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "NOGP"){
    
    nutrient_rootDF$NLCDClass[t] <- "Grassland/Herbaceous"
  }
  
  else if (nutrient_rootDF$siteID[t] == "OAES"){
    
    nutrient_rootDF$NLCDClass[t] <- "Grassland/Herbaceous"
  }
  
  else if (nutrient_rootDF$siteID[t] == "ORNL"){
    
    nutrient_rootDF$NLCDClass[t] <- "Deciduous Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "OSBS"){
    
    nutrient_rootDF$NLCDClass[t] <- "Emergent Herbaceous Wetlands"
  }
  
  else if (nutrient_rootDF$siteID[t] == "RMNP"){
    
    nutrient_rootDF$NLCDClass[t] <- "Evergreen Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "SCBI"){
    
    nutrient_rootDF$NLCDClass[t] <- "Deciduous Forest"
  }
  
  else if (nutrient_rootDF$siteID[t] == "SJER"){
    
    nutrient_rootDF$NLCDClass[t] <- "Evergreen Forest"
  }
 
  else if (nutrient_rootDF$siteID[t] == "SOAP"){
    
    nutrient_rootDF$NLCDClass[t] <- "Evergreen Forest"
  } 
 
  else if (nutrient_rootDF$siteID[t] == "SRER"){
    
    nutrient_rootDF$NLCDClass[t] <- "Shrub/Scrub"
  }  
  
  else if (nutrient_rootDF$siteID[t] == "TALL"){
    
    nutrient_rootDF$NLCDClass[t] <- "Deciduous Forest"
  }  
  
  else if (nutrient_rootDF$siteID[t] == "TEAK"){
    
    nutrient_rootDF$NLCDClass[t] <- "Evergreen Forest"
  }  
  
  else if (nutrient_rootDF$siteID[t] == "UNDE"){
    
    nutrient_rootDF$NLCDClass[t] <- "Deciduous Forest"
  }   
  
  else if (nutrient_rootDF$siteID[t] == "WOOD"){
    
    nutrient_rootDF$NLCDClass[t] <- "Emergent Herbaceous Wetlands"
  }  
  
  else if (nutrient_rootDF$siteID[t] == "WREF"){
    
    nutrient_rootDF$NLCDClass[t] <- "Evergreen Forest"
  }   
  
  else if (nutrient_rootDF$siteID[t] == "YELL"){
    
    nutrient_rootDF$NLCDClass[t] <- "Evergreen Forest"
  }   
  
}


#Examine data distribution
ggplot(data = nutrient_rootDF) +
  geom_freqpoly(aes(x = meanSiteN))

ggplot(data = nutrient_rootDF) +
  geom_freqpoly(aes(x = meanSiteS))

ggplot(data = nutrient_rootDF) +
  geom_freqpoly(aes(x = meanSiteP))

ggplot(data = nutrient_rootDF) +
  geom_freqpoly(aes(x = meanFineRootMass))


###Making a faceted series of plots that pairwise correlations
###between variables in the dataset

#Select the continuous variables from the dataset
nutrient_rootContin <- nutrient_rootDF %>%
  select(meanFineRootMass, meanSiteN,
         meanSiteP, meanSiteS) %>%
  drop_na()

#Calculate the p-values and correlation matrix for each comparison
nutrient_rootCorrelation <- rcorr(as.matrix(nutrient_rootContin),
                                  type = "pearson")
#Look at correlation coefficients (r)
nutrient_rootCorrelation$r

#Look at p-values for pairwise comparisons
nutrient_rootCorrelation$P

##Make a correlagram to see if the correlations are significant at a 0.05 level
corrplot(nutrient_rootCorrelation$r, type = "upper", 
         tl.col = "black", 
         tl.srt = 45, 
         p.mat = nutrient_rootCorrelation$P, 
         sig.level = 0.05, 
         addCoef.col = "black",
         insig = "blank")


##Principal Components Analysis

#Create a dataframe specifically for PCA 
nutrient_rootPCA <- nutrient_rootDF 
  

#Make NLCD Class its own vector object and then remove it from the PCA dataframe
NLCDClass_vector <- as.vector(nutrient_rootPCA$NLCDClass)
nutrient_rootPCA <- nutrient_rootPCA %>%
  select(-NLCDClass, -siteID)

#Estimate the PCA Model
nutrient_rootpca_est <- prcomp(nutrient_rootPCA, center= TRUE, scale.=TRUE)
summary(nutrient_rootpca_est)

#Examine the contributions of each of the variables to PC1
fviz_contrib(nutrient_rootpca_est, choice = "var", axes = 1, top = 10)
#meanSiteN and meanSiteS contribute an above average contribution
#meanSiteP and sumFineRootMass do not

#Examine the contributions of each variable PC2
fviz_contrib(nutrient_rootpca_est, choice = "var", axes = 2, top = 10)
#sumFineRootMass contributes almost 100% of dimension 2

#Plot the PCA variables in relation to each other on these two axes
fviz_pca_var(nutrient_rootpca_est,
             geom.ind = "point", 
             mean.point = FALSE, 
             addEllipses = FALSE, 
             col.var = "black") + 
  theme_bw()
#the three soil nutrient variables appear to be positively correlated with each other.
#There does not appear to be any correlation with the fine root mass 

##PCA colored by NLCDClass to see if that reveals any trends


fviz_pca_biplot(nutrient_rootpca_est,
                #geom.ind = "point", 
                geom = "point",
                #pointShape = c(),
                col.ind = NLCDClass_vector, 
                mean.point = FALSE, 
                addEllipses = TRUE, 
                col.var = "black", 
                legend.title = "NLCD Class")  +
  scale_shape_manual(values=c(16, 16, 16, 16, 16, 16, 16, 16)) +

  theme_bw()

