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
<<<<<<< HEAD
    # The bands that are probably the most important in predicting tree cover are band 2, 3 and 5. The RMSE is 1.6189.
    # NB: the sample function can be used to generate all the band combinations to assess the lowest RMSE.
    # E.g. bands <- c("band1", "band2", "band3", "band4", "band5", "band7")
    # sample(bands, count = 3)
# Question 3
  # Are the differences between the predicted and actual tree cover the same for all of the 3 classes we used for the random forest classfication?
    # The differences between the predicted and actual classes are not the same.
    # The RMSE per zone is 5.148 for Crop, 2.185 for Forest and 4.761 for Wetlands.
=======
    # The bands that are probably most important are bands 
    #
    #Coefficients:
    #                 Estimate    Std.Error  t value    Pr(>|t|)    
    #(Intercept)       8.549e+01  5.723e-02 1493.805   <2e-16 ***
    #valuetable$band1  9.291e+02  1.889e+00  491.817   <2e-16 ***
    #valuetable$band2 -1.505e+03  2.353e+00 -639.622   <2e-16 ***
    #valuetable$band3 -2.528e+01  1.698e+00  -14.889   <2e-16 ***
    #valuetable$band4  1.661e+02  2.790e-01  595.378   <2e-16 ***
    #valuetable$band5 -2.006e+02  7.036e-01 -285.119   <2e-16 ***
    #valuetable$band7  2.549e-01  8.969e-01    0.284    0.776    
>>>>>>> a738b97f10a4b09d371718e29e83762a64a7a195
