library(data.table); library(dplyr); library(terra)

## SAMPLE LVIS .TXT FILES DOWNLOADED FROM:
# https://nsidc.org/data/lvisf2/versions/1

##  10x10m gridded data 
## "LVISF2_ABoVE2019_0715_R2003_071843.txt"
## "LVISF2_ABoVE2019_0715_R2003_071996.txt"

## Convert text to spatial points
## Extract fields 
## Aggregate to raster of desired resolution using "maximum" focal function

# Set parameters
fields = c("RH95", "RH98") # Fields to extract
res = 30 # Spatial resolution of output raster in meters
proj = "EPSG:32610" # projection "WGS84: UTM Zone 10"

# Column names 
names = c("LFID","SHOTNUMBER","TIME", "GLON","GLAT","ZG","HLON","HLAT","ZH",
          "TLON","TLAT","ZT","RH10","RH15","RH20","RH25",'RH30',"RH35","RH40",
          "RH45","RH50","RH55","RH60","RH65","RH70","RH75","RH80","RH85","RH90",
          "RH95","RH96","RH97","RH98","RH99","RH100","AZIMUTH","INCIDENTANGLE",
          "RANGE","COMPLEXITY","SENSITIVITY","CHANNEL_ZT","CHANNEL_ZG","CHANNEL_RH")

# list of input txt files
lvis.list = list.files(path = "_input",
                       pattern = ".txt$",
                       full.names = TRUE)


## Process every text file in the list
## Gridded raster output 
for (file in lvis.list) {
  
  print(file)
  
  df = fread(input = file, col.names = names, skip = 21)
  df$TLON = df$TLON - 360
  
  # creat spatvector in desired projection
  v = vect(df, geom = c('TLON', 'TLAT'), keepgeom = TRUE, crs = 'EPSG:4269') %>%
    project(., proj)
  # create a template raster for gridded points
  r = rast(ext = ext(v), resolution = 10, crs = proj)
  
  for (f in fields) {
    
    print(f)
    
    # strings for output raster name
    folder = "outputs/"
    file.out = gsub("_input/", "", file)
    file.out = gsub(".txt", ".tif", file.out)
    
    # aggregate to desired resolution and write as a projected raster
    rast = rasterize(v, r, field = f) %>% 
      terra::aggregate(., fact = res/10, fun = "max", na.rm = TRUE)%>%
      writeRaster(.,  filename = paste0(folder, f , "_", file.out), overwrite=T) 
  }
}

## Buffer each point 5m to correspond with 10m sampling diameter
## output shapefile of buffers with associated metadata and height information as attributes
for (file in lvis.list) {
  
  print(file)
  
  df = fread(input = file, col.names = names, skip = 21)
  df$TLON = df$TLON - 360
  
  # creat spatvector in desired projection
  v = vect(df, geom = c('TLON', 'TLAT'), keepgeom = TRUE, crs = 'EPSG:4269') %>%
    project(., proj) %>% 
    terra::buffer(., 5)
  
  # strings for output raster name
  folder = "outputs/"
  file.out = gsub("_input/", "", file)
  file.out = gsub(".txt", ".shp", file.out)
  
  writeVector(v, filename = paste0(folder, "_", file.out), overwrite=T)
}
