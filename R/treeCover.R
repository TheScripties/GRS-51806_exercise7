# Michiel Blok and Madeleine van Winkel
# 13 January 2015

# The function to analyze tree cover --------------------------------------
treeCover <- function() {
  # Preparing the data for further usage
  GewataBrick <- preprocessing()
  visualizeLandsatVCF(GewataBrick)
  GewataLM <- linearModel(GewataBrick)
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
  GewataBrick[[7]][GewataBrick[[7]] > 100] <- NA
  summary(GewataBrick[[7]])
  
  GewataBands <- calc(GewataBrick[[1:6]], fun = function(x) x / 10000)
  
  GewataCovs <- addLayer(GewataBands, GewataBrick[[7]])
  names(GewataCovs) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")
  
  # Extract all values into a matrix
  valuetable <- getValues(GewataCovs)
  valuetable <- na.omit(valuetable)
  valuetable <- as.data.frame(valuetable)

  GewataLM <- lm(formula = valuetable$VCF ~ valuetable$band1 + valuetable$band2 + valuetable$band3 + valuetable$band4 + valuetable$band5 + valuetable$band7, data = valuetable)
  summary(GewataLM)
  plot(GewataLM)
  return(GewataLM)
}
