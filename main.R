# Michiel Blok and Madeleine van Winkel
# 13 January 2015

# Set working directory
#setwd()

# Import packages
library(raster)
library(sp)

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
    #
