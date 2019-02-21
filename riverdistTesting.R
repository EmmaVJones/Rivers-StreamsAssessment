library(riverdist)

MyRivernetwork <- line2network(path=".", layer="MyShapefile")

# Re-projecting in Alaska Albers Equal Area projection:
AKalbers <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154
    +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

MyRivernetwork <- line2network(path=".", layer="MyShapefile", reproject=AKalbers)


data(fakefish)
fakefish_riv <- xy2segvert(x=fakefish$x, y=fakefish$y, rivers=Gulk)
head(fakefish_riv)  # a look at the first few rows of the output
hist(fakefish_riv$snapdist, main="snapping distance (m)")


segvert_from_shp <- pointshp2segvert(path=".", layer="MyPointShapefile", 
                                     rivers=MyRivernetwork)


# Dont have that data so test with other dataset
WQSsp <- readOGR("GIS/WQS2018_BRRO", 'WQS2018_BRRO_albers')
new1 <- readOGR('GIS/WQS2018_BRRO','2016stationsTEST_new') 
new1 <- spTransform(new1, CRS=' +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
+units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')

identical(new1@proj4string,WQSsp@proj4string)


# Try with WQS mini layer (too big ot bring in an clean whole thing), 
# have to project to Albers Equal Area to work with riverdist
WQSmini <- line2network(path="GIS/WQS2018_BRRO", layer='WQS2018_BRRO_albers_mini',
                      reproject = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") # too big to read in?


# check topology (nodes)
topologydots(rivers=WQSmini)

# do a clean up:
rivs_fixed <- cleanup(rivers = WQSmini)
saveRDS(rivs_fixed,'GIS/rivs_fixes.RDS')

load("data/GIS/rivs_fixes.RDS")

# now plot the old vs. the new
par(mfrow=c(1,2))
topologydots(rivers=WQSmini)
graphics::title("Raw River Topology", family="Roboto Condensed")
topologydots(rivs_fixed)
graphics::title("Clean River Topology", family="Roboto Condensed")

#Bring in test point data for snapping
testPoints <- st_read('GIS/2016stationsTEST_new_mini.shp') 

# Need in Albers equal area to work with riverdist
testPoints <- testPoints %>% 
  st_transform(crs = 102003) # convert to Albers Equal Area

# add COORDS in projected form (UTM)
testPoints$X <- st_coordinates(testPoints)[,1]
testPoints$Y <- st_coordinates(testPoints)[,2]

# run this to snap points to line (and see distrib)
cdec_riv <- xy2segvert(x=testPoints$X, y=testPoints$Y, rivers=rivs_fixed)
head(cdec_riv)

hist(cdec_riv$snapdist, breaks=50,main="Point Snapping distance (CDEC to Flowline)", col="skyblue3", xlab="Snapping Distance (m)", family="Roboto Condensed")

# add vertices
df_locs_NFA <- bind_cols(testPoints, cdec_riv)

# MAP IT
plot(x=rivs_fixed, color = FALSE)
points(testPoints$X, testPoints$Y, pch=16, col="red") # raw coords
riverpoints(seg=cdec_riv$seg, vert=cdec_riv$vert, rivers=rivs_fixed, pch=22, cex=1.5,
            bg="forestgreen") # snapped coords
text(testPoints$X, testPoints$Y, labels=testPoints$vert, pos = 4, family="Roboto Condensed")

# save out new snapped points to add back to gis for checking
st_write(testPoints, "riverdist_result.shp")
