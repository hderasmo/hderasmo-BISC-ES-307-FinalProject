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

#Summarize to create an sum of rootDryMass for each megapit

mpr_fineRootTot <- mpr_fineRoot %>%
  group_by(siteID) %>%
  summarise(sumFineRootMass = sum(rootDryMass))

##c.	Full_join the two dataframes by siteID

nutrient_rootDF <- left_join(mpr_fineRootTot, mgp_nutrientSiteMean,
                             by = c("siteID")) %>%
  filter(is.na(meanSiteN) == FALSE)

ggplot(data = nutrient_rootDF) +
  geom_point(aes(x = meanSiteN, y = sumFineRootMass), color = "blue") +
  geom_point(aes(x = meanSiteS, y = sumFineRootMass), color = "red") +
  geom_point(aes(x = meanSiteP, y = sumFineRootMass), color = "green") 

#Examine data distribution
ggplot(data = nutrient_rootDF) +
  geom_freqpoly(aes(x = meanSiteN))

ggplot(data = nutrient_rootDF) +
  geom_freqpoly(aes(x = meanSiteS))

ggplot(data = nutrient_rootDF) +
  geom_freqpoly(aes(x = meanSiteP))

ggplot(data = nutrient_rootDF) +
  geom_freqpoly(aes(x = sumFineRootMass))

#Log-transform data based on skewed distributions
nutrient_rootLog <- nutrient_rootDF %>%
         mutate(logMeanSiteN = log(meanSiteN),
                logMeanSiteP = log(meanSiteP),
                logMeanSiteS = log(meanSiteS),
                logSumFineRootMass = log(sumFineRootMass))

#Examine log data distribution
ggplot(data = nutrient_rootLog) +
  geom_freqpoly(aes(x = logMeanSiteN))

ggplot(data = nutrient_rootLog) +
  geom_freqpoly(aes(x = logMeanSiteS))

ggplot(data = nutrient_rootLog) +
  geom_freqpoly(aes(x = logMeanSiteP))

ggplot(data = nutrient_rootLog) +
  geom_freqpoly(aes(x = logSumFineRootMass))


###Making a faceted series of plots that pairwise correlations
###between variables in the dataset

#Select the continuous variables from the dataset
nutrient_rootContin <- nutrient_rootDF %>%
  select(sumFineRootMass, meanSiteN,
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
  

#Make SiteID its own vector object and then remove it from the PCA dataframe
siteID_vector <- as.vector(nutrient_rootPCA$siteID)
nutrient_rootPCA <- nutrient_rootPCA %>%
  select(-siteID)

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

