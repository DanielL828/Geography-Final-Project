# Daniel L

# Final Project

# Loading in my Landsat folder from my working directory

setwd("/Users/daniellopez/Desktop/Final Project Images/2009-2011/LT05_L2SP_232066_20090715_20200827_02_T1")

library(raster)

# Stacking my 2009 Landsat image bands together:

"2009 Images"

landsat_20090715 <- stack(
  "LT05_L2SP_232066_20090715_20200827_02_T1_SR_B1.tif",
  "LT05_L2SP_232066_20090715_20200827_02_T1_SR_B2.tif",
  "LT05_L2SP_232066_20090715_20200827_02_T1_SR_B3.tif",
  "LT05_L2SP_232066_20090715_20200827_02_T1_SR_B4.tif",
  "LT05_L2SP_232066_20090715_20200827_02_T1_SR_B5.tif",
  "LT05_L2SP_232066_20090715_20200827_02_T1_ST_B6.tif")

landsat_20090731 <- stack(
  "LT05_L2SP_232066_20090731_20200827_02_T1_SR_B1.tif",
  "LT05_L2SP_232066_20090731_20200827_02_T1_SR_B2.tif",
  "LT05_L2SP_232066_20090731_20200827_02_T1_SR_B3.tif",
  "LT05_L2SP_232066_20090731_20200827_02_T1_SR_B4.tif",
  "LT05_L2SP_232066_20090731_20200827_02_T1_SR_B5.tif",
  "LT05_L2SP_232066_20090731_20200827_02_T1_ST_B6.tif")

landsat_20091003 <- stack(
  "LT05_L2SP_232066_20091003_20200825_02_T1_SR_B1.tif",
  "LT05_L2SP_232066_20091003_20200825_02_T1_SR_B2.tif",
  "LT05_L2SP_232066_20091003_20200825_02_T1_SR_B2.tif",
  "LT05_L2SP_232066_20091003_20200825_02_T1_SR_B4.tif",
  "LT05_L2SP_232066_20091003_20200825_02_T1_SR_B5.tif",
  "LT05_L2SP_232066_20091003_20200825_02_T1_ST_B6.tif")

# Displaying landsat image data
landsat_20090715
landsat_20090731
landsat_20091003

# Setting band names for my Landsat images
names(landsat_20090715) <- c("blue", "green", "red", "NIR", "NIR2", "TIR")
names(landsat_20090731) <- c("blue", "green", "red", "NIR", "NIR2", "TIR")
names(landsat_20091003) <- c("blue", "green", "red", "NIR", "NIR2", "TIR")

# This section is plotting my Landsat images
plotRGB(landsat_20090715, r = 3, g = 2, b = 1, stretch = "lin") # This is plotting the Landsat image in true color
plotRGB(landsat_20090731, r = 3, g = 2, b = 1, stretch = "lin") # This is plotting the Landsat image in true color
plotRGB(landsat_20091003, r = 3, g = 2, b = 1, stretch = "lin") # This is plotting the Landsat image in true color

# Loading in my created shapfile for my reserach area
Shapefile_Brazil <- shapefile("shapefile.shp") # Loading in the Brazil shape file

# Displays Coordinate Reference System:
crs(Shapefile_Brazil)
crs(landsat_20090715)

# Transforming the shapefile coordinate reference system to match the Landsat images
Amazon_utm <- spTransform(Shapefile_Brazil, crs(landsat_20090715)) 

# Displays new coordinate reference system:
crs(Amazon_utm)

# This crops then masks the shape file and Landsat image
landsat_20090715_cropped <- crop(landsat_20090715, extent(Amazon_utm))
landsat_20090731_cropped <- crop(landsat_20090731, extent(Amazon_utm))
landsat_20091003_cropped <- crop(landsat_20091003, extent(Amazon_utm))


landsat_20090715_mask <- mask(landsat_20090715_cropped, Amazon_utm)
landsat_20090731_mask <- mask(landsat_20090731_cropped, Amazon_utm)
landsat_20091003_mask <- mask(landsat_20091003_cropped, Amazon_utm)

# Plotting new Lansat images after cropping and masking
plotRGB(landsat_20090715_mask, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the Landsat data
plotRGB(landsat_20090731_mask, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the Landsat data
plotRGB(landsat_20091003_mask, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the Landsat data


# These two segments reclassify Landsat data 
landsat_20090715_mask_reclass <- reclassify(landsat_20090715_mask, c(-Inf, 7273, NA))
landsat_20090715_mask_reclass <- reclassify(landsat_20090715_mask, c(43636, Inf, NA))

landsat_20090731_mask_reclass <- reclassify(landsat_20090731_mask, c(-Inf, 7273, NA))
landsat_20090731_mask_reclass <- reclassify(landsat_20090731_mask, c(43636, Inf, NA))

landsat_20091003_mask_reclass <- reclassify(landsat_20091003_mask, c(-Inf, 7273, NA))
landsat_20091003_mask_reclass <- reclassify(landsat_20091003_mask, c(43636, Inf, NA))

landsat_20090715_rescaled <- (landsat_20090715_mask_reclass) / 100 # re scaling Landsat image
landsat_20090731_rescaled <- (landsat_20090731_mask_reclass) / 100 # re scaling Landsat image
landsat_20091003_rescaled <- (landsat_20091003_mask_reclass) / 100 # re scaling Landsat image

plotRGB(landsat_20090715_rescaled, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the Landsat image after re scaling
plotRGB(landsat_20090731_rescaled, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the Landsat image after re scaling
plotRGB(landsat_20091003_rescaled, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the Landsat image after re scaling


# Stacking my 2010 Landsat image bands together:

"2010 Images"

landsat_20100702 <- stack(
  "LT05_L2SP_232066_20100702_20200824_02_T1_SR_B1.tif",
  "LT05_L2SP_232066_20100702_20200824_02_T1_SR_B2.tif",
  "LT05_L2SP_232066_20100702_20200824_02_T1_SR_B3.tif",
  "LT05_L2SP_232066_20100702_20200824_02_T1_SR_B4.tif",
  "LT05_L2SP_232066_20100702_20200824_02_T1_SR_B5.tif",
  "LT05_L2SP_232066_20100702_20200824_02_T1_ST_B6.tif")

landsat_20100718 <- stack(
  "LT05_L2SP_232066_20100718_20200823_02_T2_SR_B1.tif",
  "LT05_L2SP_232066_20100718_20200823_02_T2_SR_B2.tif",
  "LT05_L2SP_232066_20100718_20200823_02_T2_SR_B3.tif",
  "LT05_L2SP_232066_20100718_20200823_02_T2_SR_B4.tif",
  "LT05_L2SP_232066_20100718_20200823_02_T2_SR_B5.tif",
  "LT05_L2SP_232066_20100718_20200823_02_T2_ST_B6.tif")

# Renaming 2010 Landsat image bands
names(landsat_20100702) <- c("blue", "green", "red", "NIR", "NI2R", "TIR")
names(landsat_20100718) <- c("blue", "green", "red", "NIR", "NIR2", "TIR")

plotRGB(landsat_20100702, r = 3, g = 2, b = 1, stretch = "lin") # This is plotting the 2010 Landsat image in true color
plotRGB(landsat_20100718, r = 3, g = 2, b = 1, stretch = "lin") # This is plotting the 2010 Landsat image in true color

# This crops then masks the shapefile and Landsat image
landsat_20100702_cropped <- crop(landsat_20100702, extent(Amazon_utm))
landsat_20100718_cropped <- crop(landsat_20100718, extent(Amazon_utm))


landsat_20100702_mask <- mask(landsat_20100702_cropped, Amazon_utm)
landsat_20100718_mask <- mask(landsat_20100718_cropped, Amazon_utm)


plotRGB(landsat_20100702_mask, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the Landsat data after masking and cropping
plotRGB(landsat_20100718_mask, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the Landsat data after masking and cropping


# These two segments reclassify the 2010 Landsat data 
landsat_20100702_mask_reclass <- reclassify(landsat_20100702_mask, c(-Inf, 7273, NA))
landsat_20100702_mask_reclass <- reclassify(landsat_20100702_mask, c(43636, Inf, NA))

landsat_20100718_mask_reclass <- reclassify(landsat_20100718_mask, c(-Inf, 7273, NA))
landsat_20100718_mask_reclass <- reclassify(landsat_20100718_mask, c(43636, Inf, NA))


landsat_20100702_rescaled <- (landsat_20100702_mask_reclass) / 100 # re scaling 2010 Landsat image
landsat_20100718_rescaled <- (landsat_20100718_mask_reclass) / 100 # re scaling 2010 Landsat image


plotRGB(landsat_20100702_rescaled, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2010 Landsat image after re scaling
plotRGB(landsat_20100718_rescaled, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2010 Landsat image after re scaling


# Stacking my 2011 Landsat image bands together:

"2011 Images"

landsat_20110603 <- stack(
  "LT05_L2SP_232066_20110603_20200822_02_T1_SR_B1.tif",
  "LT05_L2SP_232066_20110603_20200822_02_T1_SR_B2.tif",
  "LT05_L2SP_232066_20110603_20200822_02_T1_SR_B3.tif",
  "LT05_L2SP_232066_20110603_20200822_02_T1_SR_B4.tif",
  "LT05_L2SP_232066_20110603_20200822_02_T1_SR_B5.tif",
  "LT05_L2SP_232066_20110603_20200822_02_T1_ST_B6.tif")


landsat_20110705 <- stack(
  "LT05_L2SP_232066_20110705_20200822_02_T1_SR_B1.tif",
  "LT05_L2SP_232066_20110705_20200822_02_T1_SR_B2.tif",
  "LT05_L2SP_232066_20110705_20200822_02_T1_SR_B3.tif",
  "LT05_L2SP_232066_20110705_20200822_02_T1_SR_B4.tif",
  "LT05_L2SP_232066_20110705_20200822_02_T1_SR_B5.tif",
  "LT05_L2SP_232066_20110705_20200822_02_T1_ST_B6.tif")


landsat_20110721 <- stack(
  "LT05_L2SP_232066_20110721_20200822_02_T1_SR_B1.tif",
  "LT05_L2SP_232066_20110721_20200822_02_T1_SR_B2.tif",
  "LT05_L2SP_232066_20110721_20200822_02_T1_SR_B3.tif",
  "LT05_L2SP_232066_20110721_20200822_02_T1_SR_B4.tif",
  "LT05_L2SP_232066_20110721_20200822_02_T1_SR_B5.tif",
  "LT05_L2SP_232066_20110721_20200822_02_T1_ST_B6.tif")

landsat_20110806 <- stack(
  "LT05_L2SP_232066_20110806_20200820_02_T1_SR_B1.tif",
  "LT05_L2SP_232066_20110806_20200820_02_T1_SR_B2.tif",
  "LT05_L2SP_232066_20110806_20200820_02_T1_SR_B3.tif",
  "LT05_L2SP_232066_20110806_20200820_02_T1_SR_B4.tif",
  "LT05_L2SP_232066_20110806_20200820_02_T1_SR_B5.tif",
  "LT05_L2SP_232066_20110806_20200820_02_T1_ST_B6.tif")

# Renaming 2011 Landsat image bands

names(landsat_20110603) <- c("blue", "green", "red", "NIR", "NIR2", "TIR")
names(landsat_20110705) <- c("blue", "green", "red", "NIR", "NIR2", "TIR")
names(landsat_20110721) <- c("blue", "green", "red", "NIR", "NIR2", "TIR")
names(landsat_20110806) <- c("blue", "green", "red", "NIR", "NIR2", "TIR")


plotRGB(landsat_20110603, r = 3, g = 2, b = 1, stretch = "lin") # This is plotting the 2011 Landsat image in true color
plotRGB(landsat_20110705, r = 3, g = 2, b = 1, stretch = "lin") # This is plotting the 2011 Landsat image in true color
plotRGB(landsat_20110721, r = 3, g = 2, b = 1, stretch = "lin") # This is plotting the 2011 Landsat image in true color
plotRGB(landsat_20110806, r = 3, g = 2, b = 1, stretch = "lin") # This is plotting the 2011 Landsat image in true color


# This crops then masks the shapefile and 2011 Landsat image
landsat_20110603_cropped <- crop(landsat_20110603, extent(Amazon_utm))
landsat_20110705_cropped <- crop(landsat_20110705, extent(Amazon_utm))
landsat_20110721_cropped <- crop(landsat_20110721, extent(Amazon_utm))
landsat_20110806_cropped <- crop(landsat_20110806, extent(Amazon_utm))


landsat_20110603_mask <- mask(landsat_20110603_cropped, Amazon_utm)
landsat_20110705_mask <- mask(landsat_20110705_cropped, Amazon_utm)
landsat_20110721_mask <- mask(landsat_20110721_cropped, Amazon_utm)
landsat_20110806_mask <- mask(landsat_20110806_cropped, Amazon_utm)


plotRGB(landsat_20110603_mask, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2011 Landsat mask image
plotRGB(landsat_20110705_mask, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2011 Landsat mask image
plotRGB(landsat_20110721_mask, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2011 Landsat mask image
plotRGB(landsat_20110806_mask, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2011 Landsat mask image


# These two segments reclassify 2011 Landsat data 
landsat_20110603_mask_reclass <- reclassify(landsat_20110603_mask, c(-Inf, 7273, NA))
landsat_20110705_mask_reclass <- reclassify(landsat_20110705_mask, c(43636, Inf, NA))
landsat_20110721_mask_reclass <- reclassify(landsat_20110721_mask, c(-Inf, 7273, NA))
landsat_20110806_mask_reclass <- reclassify(landsat_20110806_mask, c(43636, Inf, NA))


landsat_20110603_mask_reclass <- reclassify(landsat_20110603_mask, c(-Inf, 7273, NA))
landsat_20110705_mask_reclass <- reclassify(landsat_20110705_mask, c(43636, Inf, NA))
landsat_20110721_mask_reclass <- reclassify(landsat_20110721_mask, c(-Inf, 7273, NA))
landsat_20110806_mask_reclass <- reclassify(landsat_20110806_mask, c(43636, Inf, NA))


landsat_20110603_rescaled <- (landsat_20110603_mask_reclass) / 100 # re scaling 2011 Landsat image
landsat_20110705_rescaled <- (landsat_20110705_mask_reclass) / 100 # re scaling 2011 Landsat image
landsat_20110721_rescaled <- (landsat_20110721_mask_reclass) / 100 # re scaling 2011 Landsat image
landsat_20110806_rescaled <- (landsat_20110806_mask_reclass) / 100 # re scaling 2011 Landsat image


plotRGB(landsat_20110603_rescaled, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2011 Landsat image after re scaling
plotRGB(landsat_20110705_rescaled, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2011 Landsat image after re scaling
plotRGB(landsat_20110721_rescaled, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2011 Landsat image after re scaling
plotRGB(landsat_20110806_rescaled, r = 3, g = 2, b = 1, stretch = "lin") # Plotting the 2011 Landsat image after re scaling

# This below is my NDVI function 

"NDVI"

# My NDVI function
NDVI <- function(red, NIR){
  
  value <- (NIR - red)/(NIR + red) # Calling a function fo calculating NDVI
  
  return(value)
  
}

# Calculating NDVI values for Landsat images 2009-2011

# 2009 NDVI 
NDVI_20090715 <- NDVI(landsat_20090715_rescaled$red, landsat_20090715_rescaled$NIR)
NDVI_20090731 <- NDVI(landsat_20090731_rescaled$red, landsat_20090731_rescaled$NIR)
NDVI_20091003 <- NDVI(landsat_20091003_rescaled$red, landsat_20091003_rescaled$NIR)

# 2010 NDVI
NDVI_20100702 <- NDVI(landsat_20100702_rescaled$red, landsat_20100702_rescaled$NIR)
NDVI_20100718 <- NDVI(landsat_20100718_rescaled$red, landsat_20100718_rescaled$NIR)

# 2011 NDVI
NDVI_20110603 <- NDVI(landsat_20110603_rescaled$red, landsat_20110603_rescaled$NIR)
NDVI_20110705 <- NDVI(landsat_20110705_rescaled$red, landsat_20110705_rescaled$NIR)
NDVI_20110721 <- NDVI(landsat_20110721_rescaled$red, landsat_20110721_rescaled$NIR)
NDVI_20110806 <- NDVI(landsat_20110806_rescaled$red, landsat_20110806_rescaled$NIR)


##

#Stacking NDVI images
ALL_NDVI <- stack(
  NDVI_20090715,
  NDVI_20090731,
  NDVI_20091003,
  NDVI_20100702,
  NDVI_20100718,
  NDVI_20110603,
  NDVI_20110705,
  NDVI_20110721,
  NDVI_20110806)

# Exporting  my NDVI images for task needed to be done in QGIS
#writeRaster(NDVI_20090715, "NDVI_20090715", format = "GTiff",overwrite=TRUE)
#writeRaster(NDVI_20090731, "NDVI_20090731", format = "GTiff",overwrite=TRUE)
#writeRaster(NDVI_20091003, "NDVI_20091003", format = "GTiff",overwrite=TRUE)
#writeRaster(NDVI_20100702, "NDVI_20100702", format = "GTiff",overwrite=TRUE)
#writeRaster(NDVI_20100718, "NDVI_20100718", format = "GTiff",overwrite=TRUE)
#writeRaster(NDVI_20110603, "NDVI_20110603", format = "GTiff",overwrite=TRUE)
#writeRaster(NDVI_20110705, "NDVI_20110705", format = "GTiff",overwrite=TRUE)
#writeRaster(NDVI_20110721, "NDVI_20110721", format = "GTiff",overwrite=TRUE)
#writeRaster(NDVI_20110806, "NDVI_20110806", format = "GTiff",overwrite=TRUE)

#Calculating the average NDVI for each year
NDVI_2009 <- (NDVI_20090715+NDVI_20090731+NDVI_20091003)/3
NDVI_2010 <- (NDVI_20100702+NDVI_20100718)/2
NDVI_2011 <- (NDVI_20110603+NDVI_20110705+NDVI_20110721+NDVI_20110806)/4

#Stack the average NDVI image
NDVI_images <- stack(NDVI_2009,
                     NDVI_2010,
                     NDVI_2011)


#Rename the NDVI bands 
names(NDVI_images)<- c("1","2","3")

#Export the two NDVI images 
writeRaster(NDVI_images, "NDVI_image", format = "GTiff",overwrite=TRUE)
writeRaster(ALL_NDVI, "All_NDVI", format = "GTiff",overwrite=TRUE)


#Create data frame for NDVI_images
NDVI_data_frame <- as.data.frame(NDVI_images)

#Renaming the title, which A is for 2009, B is for 2010, and C is for 2011
names(NDVI_data_frame)<- c("1","2","3")
View(NDVI_data_frame)


#Remove all NA value from dataframe
NDVI_data_frame_NA <- na.omit(NDVI_data_frame) 
View(NDVI_data_frame_NA)

#Create a new column, which if the NDVI value for 2019 is larger than for 2021, it will be classified as deforestation, otherwise it belongs to class "other": 
NDVI_data_frame_NA$classification <- print(ifelse(NDVI_data_frame_NA$"1" - NDVI_data_frame_NA$"3" > 0.06, ("deforestation"), ("other")))
View(NDVI_data_frame_NA)


#Calculate the number of deforestation pixel from 2009 to 2011
sum(with(NDVI_data_frame_NA,classification == "deforestation"))

#Calculate the total pixel value in each year
pixel_amount_2009<-sum(NDVI_data_frame_NA$"1")
pixel_amount_2010<-sum(NDVI_data_frame_NA$"2")
pixel_amount_2011<-sum(NDVI_data_frame_NA$"3")

#Calculate the average pixel value for NDVI image in each year. 
avgerage_pixel_amount_2009<-pixel_amount_2009/2881771
avgerage_pixel_amount_2010<-pixel_amount_2010/2881771
avgerage_pixel_amount_2011<-pixel_amount_2011/2881771

# This displays the average pixel data after all calculations:
avgerage_pixel_amount_2009
avgerage_pixel_amount_2010
avgerage_pixel_amount_2011