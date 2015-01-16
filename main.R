# Michiel Blok and Madeleine van Winkel
# 13 January 2015

# Set working directory
#setwd()

# Import packages
library(sp)
library(raster)

# Source functions
source('R/treeCover.R')

# Then the actual commands
treeCover()

# Answers to the questions from the exercise ------------------------------
# Question 1
  # What can you conclude from this/these plot(s)?
    # There is a negative correlation between the reflectance of all Landsat ETM+ bands, except for band 4, and the Vegetation Continuous Field (VCF).
    # All the plots indicate that a decrease in reflection results in an increase in VCF.
# Question 2
  # Which predictors (bands) are probably most important in predicting tree cover?
    # The bands that are probably the most important in predicting tree cover are band 2, 3 and 5. The RMSE is 1.6189.
    # NB: the sample function can be used to generate all the band combinations to assess the lowest RMSE.
    # E.g. bands <- c("band1", "band2", "band3", "band4", "band5", "band7")
    # sample(bands, count = 3)
# Question 3
  # Are the differences between the predicted and actual tree cover the same for all of the 3 classes we used for the random forest classfication?
    # The differences between the predicted and actual classes are not the same.
    # The RMSE per zone is 5.148 for Crop, 2.185 for Forest and 4.761 for Wetlands.
