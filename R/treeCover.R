# Michiel Blok and Madeleine van Winkel
# 13 January 2015

# The function to analyze tree cover --------------------------------------
treeCover <- function() {
  # Preparing the data for further usage
  GewataBrick <- preprocessing()
  visualizeLandsatVCF(GewataBrick)
  VCFlist <- linearModel(GewataBrick)
  RMSEclasses <- calculateDifference(VCFlist)
}

preprocessing <- function() {
  # Download the project data from the repository, if necessary
  #download.file(url = "https://github.com/GeoScripting-WUR/AdvancedRasterAnalysis/tree/gh-pages/data", destfile = "GRS-51806_exercise7", quiet = TRUE, method = 'auto')
  
  load("data/GewataB1.rda")
  load("data/GewataB2.rda")
  load("data/GewataB3.rda")
  load("data/GewataB4.rda")
  load("data/GewataB5.rda")
  load("data/GewataB7.rda")
  load("data/vcfGewata.rda")

  # To remove values greater than 100
  vcfGewata[vcfGewata > 100] <- NA
  plot(vcfGewata, main = "VCF")
  summary(vcfGewata)
  
  # Create a multi-layer raster object using the brick function
  GewataBrick <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
  return(GewataBrick)
}

visualizeLandsatVCF <- function(GewataBrick) {
  #opar <- par(mfrow = c(2, 3))
  #plot(x = GewataBrick[[7]], y = GewataBrick[[1]], data = GewataBrick[[1]], main = "Gewata", pch = ".", col = "blue", xlim = c(0, GewataBrick[[7]]@data@max), ylim = c(0, GewataBrick[[1]]@data@max), xlab = "VCF", ylab = "Landsat ETM+ band 1")
  #plot(x = GewataBrick[[7]], y = GewataBrick[[2]], data = GewataBrick[[2]], main = "Gewata", pch = ".", col = "red", xlim = c(0, GewataBrick[[7]]@data@max), ylim = c(0, GewataBrick[[2]]@data@max), xlab = "VCF", ylab = "Landsat ETM+ band 2")
  #plot(x = GewataBrick[[7]], y = GewataBrick[[3]], data = GewataBrick[[3]], main = "Gewata", pch = ".", col = "green", xlim = c(0, GewataBrick[[7]]@data@max), ylim = c(0, GewataBrick[[3]]@data@max), xlab = "VCF", ylab = "Landsat ETM+ band 3")
  #plot(x = GewataBrick[[7]], y = GewataBrick[[4]], data = GewataBrick[[4]], main = "Gewata", pch = ".", col = "orange", xlim = c(0, GewataBrick[[7]]@data@max), ylim = c(0, GewataBrick[[4]]@data@max), xlab = "VCF", ylab = "Landsat ETM+ band 4")  
  #plot(x = GewataBrick[[7]], y = GewataBrick[[5]], data = GewataBrick[[5]], main = "Gewata", pch = ".", col = "black", xlim = c(0, GewataBrick[[7]]@data@max), ylim = c(0, GewataBrick[[5]]@data@max), xlab = "VCF", ylab = "Landsat ETM+ band 5")
  #plot(x = GewataBrick[[7]], y = GewataBrick[[6]], data = GewataBrick[[6]], main = "Gewata", pch = ".", col = "purple", xlim = c(0, GewataBrick[[7]]@data@max), ylim = c(0, GewataBrick[[6]]@data@max), xlab = "VCF", ylab = "Landsat ETM+ band 7")
  #par(opar)
  
  pairs(GewataBrick)
}

linearModel <- function(GewataBrick) {
  # Rescale the original reflectance values to their original scale
  GewataBands <- calc(GewataBrick[[1:6]], fun = function(x) x / 10000)
  
  # Make a new raster brick of covariates by adding VCF layer
  GewataCovs <- addLayer(GewataBands, GewataBrick[[7]])
  names(GewataCovs) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")
  
  # Extract all values into a matrix
  valuetable <- getValues(GewataCovs)
  
  # Remove the NA-values
  valuetable <- na.omit(valuetable)
  valuetable <- as.data.frame(valuetable)

  # Make a linear model of the original VCF and the Landsat bands (raster layers)
  GewataLM <- lm(formula = VCF ~ band2 + band3 + band5, data = valuetable)
  summary(GewataLM)
  
  # Predict the tree cover using the linear model
  predictTC <- predict(GewataCovs, model = GewataLM, na.rm = TRUE)
  
  predictTC[predictTC < 0] <- 0

  # Plot the predicted tree cover raster and compare it with the original VCF raster
  opar <- par(mfrow = c(1, 2))
  plot(predictTC, main = "Predicted VCF")
  plot(GewataBrick[[7]], main = "Original VCF")
  par(opar)

  # Calculate the RMSE between predicted and actual tree cover values
  Rows <- predictTC@nrows
  Cols <- predictTC@ncols
  RMSE <- sqrt(mean((GewataCovs$VCF[Rows, Cols] - predictTC[Rows, Cols])^2))
  
  # Create a vector to return
  VCFlist <- c("actual" = GewataCovs$VCF, "predict" = predictTC)
  
  return(VCFlist)
}

<<<<<<< HEAD
# Calculate the RMSE for the actual and predicted VCF and the different classes
calculateDifference <- function(VCFlist) {
  # Load the training polygons
  load("data/trainingPoly.rda")

  # Convert training polygon classes into integers
  trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)

  # View the training polygon data
  trainingPoly@data

  # Give raster cells a 'code' value
  classes <- rasterize(trainingPoly, VCFlist$actual, field = 'Code')

  # Create a new rasterbrick for calculations
  VCF <- brick(VCFlist$actual, VCFlist$predict)

  # Calculate the mean of each zone
  VCFzonal <- zonal(VCF, classes, fun = 'mean', digits = 1, na.rm = TRUE)
  VCFdf <- as.data.frame(VCFzonal)
  
  # Calculate the Root mean squared Error for all of the 3 classes
  RMSEclasses <- sqrt((VCFdf$VCF - VCFdf$layer)^2)
  
  names(RMSEclasses) <- c("Crop", "Forest", "Wetlands")
  return(RMSEclasses)
}
=======
# function 
#create training table with names same as raster layers in input brick

#Done

#(1)#produce one or more plots that demonstrate the relationship between the Landsat bands and the VCF tree cover. 
###What can you conclude from this/these plot(s)?
#(2)#create an lm() model and show a summary (e.g. using summary()) of the model object you created.

#To Do
#(2)
###Which predictors (bands) are probably most important in predicting tree cover?
#(3)#plot the predicted tree cover raster and compare with the original VCF raster.
#(4)#compute the RMSE between your predicted and the actual tree cover values (hint )



# Calculate the RMSE ------------------------------------------------------
RMSE <- function (actualTreeCover, predictedTreeCover) {
  RMSE <- sqrt(mean((actualTreeCover-predictedTreeCover)^2))
  return (RMSE)
}

# Calculate the RMSE for the actual and predicted VCF and the different classes
RMSEclasses <- function(VCF_covs) {
   # Load the training polygons
   load("data/trainingPoly.rda")
   
   # Convert training polygon classes into integers
   trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
   
   # View the training polygon data
   trainingPoly@data
   
   # Give raster cells a 'code' value
   classes <- rasterize(trainingPoly, VCF_covs, field='Code')
   
   # Create a new rasterbrick for calculations
   VCF <- brick(VCF_covs, predictedVCF)
   
   # Calculate the mean of each zone
   VCF_zonal <- zonal(VCF, classes, fun = 'mean', digits = 1, na.rm = TRUE)
   VCFdf <- as.data.frame(VCF_zonal)
   # Calculate the Root mean squared Error for all of the 3 classes
   RMSE_classes <- RMSE(VCFdf$vcf, VCFdf$predictedVCF) # kan dit zo terugroepen naar deRMSE functie hierboven?
   names(RMSE_classes) <- c("Crop", "Forest", "Wetlands")
   return (RMSE_classes)
}
   


#(5)#are the differences between the predicted and actual tree cover the same for all of the 3 classes we used for 
###the random forest classfication? Using the training polygons from the random forest classification, 
###calculate the RMSE separately for each of the classes and compare. Hint - see ?zonal().
>>>>>>> a738b97f10a4b09d371718e29e83762a64a7a195
