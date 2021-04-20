################################################################################
# Authos:     James Brueckheimer
# Project:    MB2 - Wetland-detection
# Date:       20. Apr. 2021 
# Descrition: This code serves as data processing tool. It loads the images
#             and calculates needed indices, dataframes and RasterObjects and
#             saves them.There is no need to run ist, as the results are
#             alreaddy safed in "./Brueckheimer_MB2/MB2/Data/MB2_Imagery/
#             2010-2020_indices/2010-2020_fitted_indices". So... 
#
#             !!!DO NOT RUN THIS!!!
#
#             The raw data was acquired and preselected via Google Earth Engine
#             and contains Level 1 TOA imagery from Landsat 5, 7 and 8 with
#             30m resolution (actually containing 95 images). The area of 
#             intereset lies in bangladesh south-east to Vermara Bazar and 
#             covers aprox. 90 km². This area gets constantly flooded 
#             after/during rainy season between September and early spring.


#Used packages (Install if you havent alreaddy)
# install.packages("raster");
# install.packages("rgdal");
# install.packages("ggplot2");
# install.packages("readr");
# install.packages("tidyverse");
# install.packages("magick");
# install.packages("devtools");
# install.packages("gganimate");
# install.packages("shiny");
# install.packages("shinydashboard");
# install.packages("leaflet");
#install.packages("RColorBrewer");



#devtools::install_github("MBalthasar/rHarmonics")

library(raster);
library(rgdal);
library(ggplot2);
library(readr);
library(tidyverse);
library(magick);
library(rHarmonics);
library(gganimate);
library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)



#load metadata from images
meta <- data.frame(read.csv(
  "./Data/MB2_Imagery/Metadata_20102020.csv"))
#View(meta)


#get file paths of images and store it as a list()
filenames <- list.files(path = "./Data/MB2_Imagery/2010-2020_raw",
                        pattern = "*.tif", full.names = T);
#filenames

#create dataframe with all extracted file information
fileinfo <- image_info(image_read(filenames));
#fileinfo

#extenc fileinfo by filenames
fileinfo$file_path <- filenames
#View(fileinfo)

#combine information about metadata and fileinfo
df_files <- cbind.data.frame(meta, fileinfo)
#View(df_files)

#create dates
dates <- format(seq(as.Date("2010-01-01"), as.Date("2020-12-31"), 
                    by ="months"), format="%Y-%m")
#dates


#create sequence of months according to investigation period as a dataframe
df_Seq <- data.frame(system.index = sequence(length(dates), from =0));

#create dataframe with one entry for all dates 
#(also missing one, which will be filled later)
df_files_complete <- merge(df_Seq, df_files, by ="system.index", all.x = T)
#View(df_files_complete)


#add missing dates
df_files_complete$Date <- dates
#View(df_files_complete)


#loading sample image (anyone of the downloaded raw images would do the job), 
#to get raster extends for sample image, so we can later on create empty raster
sample_image <- brick(df_files_complete[df_files_complete$Date=="2010-01",
                                        "file_path"]);

#create empty raster of all existing and later on needed layers
B <- G <- R <- NIR <- SWIR1 <- SWIR2 <- MNDWI <- LSWI <- NDBI  <- raster(
                                               ncol = ncol(sample_image),
                                               nrow = nrow(sample_image),
                                               xmn = xmin(sample_image),
                                               xmx = xmax(sample_image),
                                               ymn = ymin(sample_image),
                                               ymx = ymax(sample_image));

#fill all layers with NA for later purpose
values(B) <- values(G) <- values(R) <- values(NIR) <- values(SWIR1) <- NA
values(SWIR2) <- values(MNDWI) <- values(LSWI) <- values(NDBI) <- NA

#create brick of all layers equal to bands
blank <- brick(B,G,R,NIR,SWIR1,SWIR2,MNDWI,LSWI,NDBI);
names(blank) <- c("B","G","R","NIR","SWIR1","SWIR2","MNDWI", "LSWI", "NDBI")
#blank

#Create a single empty image for every month within the investigation period
setwd("./Data/MB2_Imagery/2010-2020_indices")
for(i in df_files_complete$Date){

  name <- paste(df_files_complete[df_files_complete$Date==i,"Date"], 
                "_indices",".tif", sep="")

  print(name)
  writeRaster(blank, name, overwrite = T)
}


#replace empty images with raw images which matchs the given date. It also
#calculates MNDWI, LSWI and NDBI who are needed for further analyses.
setwd("./Data/MB2_Imagery/2010-2020_indices")
for(i in df_files$file_path){
  #get the raw image and safe it as a RasterBrick
  im <- brick(i)
  #drop pixel_qa band, which isn´t needed anymore
  im <- dropLayer(im, "pixel_qa") 

  #Calculating indices
  mndwi <- (im$G - im$SWIR1)/(im$G + im$SWIR1);
  lswi <- (im$NIR - im$SWIR1)/(im$NIR + im$SWIR1);
  ndbi <- (im$SWIR1 - im$NIR)/(im$SWIR1 + im$NIR);

  #Stack indices with raw bands
  im <- stack(im, mndwi, lswi, ndbi)
  #rename Layer by bands
  names(im) <- c("B","G","R","NIR","SWIR1","SWIR2","MNDWI", "LSWI", "NDBI")

  #create file name based on image date
  name <- paste(df_files[df_files$file_path==i,"Date"], 
                "_indices",".tif", sep="")
  
  #save image as raster
  writeRaster(im, name, overwrite = T)
  
}

#Checking if it worked
# image <- brick("2020-11.tif")
# plot(image$X2020.11.8)
# plotRGB(sample_image, r = 3, g = 2, b = 1, stretch = "lin")


#get all filenames of images within the 2010-2020_indices folder ending on .tif 
filenames_indices <- list.files(path = "./Data/MB2_Imagery/2010-2020_indices",
                        pattern = "*.tif", full.names = T);

#Create empy RasterStacks for each index
mndwi_stack <- stack()
lswi_stack <- stack()
ndbi_stack <- stack()

#stack all indices of each image in a specific RasterStack
for(i in filenames_indices){
  im <- stack(i)
  
  names(im) <- c("B","G","R","NIR","SWIR1","SWIR2","MNDWI", "LSWI", "NDBI")
  
  mndwi_stack <- stack(mndwi_stack, im$MNDWI)
  lswi_stack <- stack(lswi_stack, im$LSWI)
  ndbi_stack <- stack(ndbi_stack, im$NDBI)
}




#This Function should harmonize missing Data (Gaps in Images or entire missing
#months) paralell, but somehow didn´t worked and forced me to harmonize
#every Index-Stack single handed
# fitting_fun <- function(s1,s2,s3){
#   
#   fitted_mndwi <- harmonics_fun(s1, user_dates = as.Date(paste(dates,"01", 
#                  sep="-")),
#                 harmonic_deg = 3)
#   
#   fitted_lswi <- harmonics_fun(s2, user_dates = as.Date(paste(dates,"01", 
#                  sep="-")),
#                 harmonic_deg = 3)
#   
#   fitted_ndbi <- harmonics_fun(s3, user_dates = as.Date(paste(dates,"01", 
#                  sep="-")),
#                 harmonic_deg = 3)
#   
#   return(list(fitted_mndwi,fitted_lswi,fitted_ndbi))
# }
# 
# fitting_stacks <- fitting_fun(mndwi_stack,lswi_stack,ndbi_stack);


#harmonize mndwi Layer. It will fill the gaps in the Data or even entire missing
#months, so an estimation of flooded ares can be made, with out gaps.
#The harmonics function was provided by Philipp, M. B. (2020): rHarmonics V.1.0.
#Zenodo. https://doi.org/10.5281/zenodo.3994381.
#It took ages... So if you want to run it by yourself, grap a coffee... The
#biggest one you can get and go for a walk.
fitted_mndwi <- raster::calc(mndwi_stack,
                            function(x){
                              harmonics_fun(x, 
                              user_dates = as.Date(paste(dates,"01", sep="-")),
                              harmonic_deg = 3)
                            })
#rename Layer by date taken
names(fitted_mndwi) <- dates
setwd("./Data/MB2_Imagery/2010-2020_indices/2010-2020_fitted_indices")
#save as .tif file
writeRaster(fitted_mndwi, "2010-2020_fitted_index_mndwi.tif", Overwrite = T)


#see above
fitted_lswi <- raster::calc(lswi_stack,
                             function(x){
                               harmonics_fun(x, 
                               user_dates = as.Date(paste(dates,"01", sep="-")),
                               harmonic_deg = 3)
                             })
names(fitted_lswi) <- dates
setwd("./Data/MB2_Imagery/2010-2020_indices/2010-2020_fitted_indices")
writeRaster(fitted_lswi, "2010-2020_fitted_index_lswi.tif", Overwrite = T)


#see above
fitted_ndbi <- raster::calc(ndbi_stack,
                             function(x){
                               harmonics_fun(x, 
                               user_dates = as.Date(paste(dates,"01", sep="-")),
                               harmonic_deg = 3)
                             })
names(fitted_ndbi) <- dates
setwd("./Data/MB2_Imagery/2010-2020_indices/2010-2020_fitted_indices")
writeRaster(fitted_lswi, "2010-2020_fitted_index_ndbi.tif", Overwrite = T)

#create empty RasterStack for wetland (flooded area) calculation
wetland_stack <- stack()

#creating a rule based wetland mask out of MNDWI, LSWI and NDBI.
for(i in seq(1,nlayers(fitted_mndwi))){
  
  mndwi <- fitted_mndwi[[i]]
  lswi <- fitted_lswi[[i]]
  ndbi <- fitted_ndbi[[i]]
  
  #Identifying pixel with MNDWI >= 0.1, NDBI < 0 and LSWI >= 0.2. This rule
  #based classification should give us a good idea about which areas are 
  #flooded.
  wetland <- mndwi>=0.1 & ndbi<0 & lswi>=0.2
  
  #Stack all wetland layers
  wetland_stack <- stack(wetland_stack, wetland)
  

}

#rename wetland_stack layers according to the date taken
names(wetland_stack) <- dates
#save wetland_stack as .tif file
writeRaster(wetland_stack, "2010-2020_fitted_mask_wetland.tif", Overwrite = T)

#create empty df_wetland data frame
df_wetland <- data.frame()

#calculate area size and the flooded area in percentage
for(i in 1:length(dates)){
  #call wetland_stack layer based on date taken
  r <- wetland_stack[[i]]

  #print(r)
  
  #calculate raster area using the area() function. It calculates the are for
  #each individual pixel
  cell_size <- area(r, na.rm=T, weights=F)
  cell_size <- cell_size[!is.na(cell_size)]
  raster_area <- length(cell_size)*median(cell_size)
  #print(raster_area)
  
  #set all values below 1 to NA to get the flooded pixels
  r[r<1] <- NA
  #print(r)
  #calculate are of flooded pixels
  cell_size <- area(r, na.rm=T, weights=F)
  cell_size <- cell_size[!is.na(r)]
  wetland_area <- length(cell_size)*median(cell_size)
  #print(wetland_area)
  
  #covert flooded pixel area into percentage area flooded
  area_covered <- (wetland_area/raster_area)*100
  #print(area_covered)

  #add new rows to df_wetland with the information or area_covered (by flood)
  df_wetland <- rbind(df_wetland,area_covered)

}

#add date column to df_wetland
df_wetland$Date <- as.Date(paste(dates,"01", sep="-"))
#rename columns
names(df_wetland) <-c("Area_covered", "Date")
#save df_wetland as .csv
write.csv2(df_wetland, "df_wetland.csv")


