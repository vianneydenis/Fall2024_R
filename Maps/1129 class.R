name <- LETTERS[1:10]
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5,
               -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9,
              36.2, 39, 41.6, 36.9)
stations <- cbind(longitude, latitude)
# Simulated rainfall data
set.seed(0)
precip <- round((runif(length(latitude))*10)^3)

psize <- 1 + precip/500
plot(stations, cex=psize, pch=20, col='red', main='Precipitation')
# add names to plot
text(stations, name, pos=4)
# add a legend
breaks <- c(100, 250, 500, 1000)
legend.psize <- 1+breaks/500
legend("topright", legend=breaks, pch=20, pt.cex=legend.psize, col='red', bg='gray')

lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
x <- cbind(lon, lat)
plot(stations, main='Precipitation')
polygon(x, col='blue', border='light blue')
lines(stations, lwd=3, col='red')
points(x, cex=2, pch=20)
points(stations, cex=psize, pch=20, col='red', main='Precipitation')

wst <- data.frame(longitude, latitude, name, precip)
wst

# example using .shp file
install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
filename1 <- system.file("ex/lux.shp", package="terra")
basename(filename1)

# example using .shp file
s1 <- vect(filename1)
s1

install.packages("geodata")
library (geodata)
TWN <- gadm(country="TWN", level=1, path=tempdir())
TWN <- gadm(country="TWN", level=1, path="./data")
TWN <- vect('data/gadm/gadm41_TWN_1_pk.rds')
TWN
# check for ?gadm
# The RDS format is a binary file format, native to R. It has been part of R for many years, 
# and provides a convenient method for saving R objects, including data sets. R also has two 
# native data formats—Rdata (sometimes shortened to Rda) and Rds. These formats are used when 
# R objects are saved for later use. Rdata is used to save multiple R objects, while Rds is 
# used to save a single R object.

TWN <- vect('data/gadm/gadm41_TWN_1_pk.rds')
outfile1 <- "data/shp_TWN.shp"
writeVector(TWN, outfile1, overwrite=TRUE)

f <- system.file("ex/logo.tif", package="terra")
basename(f)

r <- rast(f)
r
library(geodata)
ele <-elevation_30s("TWN", path=tempdir())
ele

x <- writeRaster(ele, "data/ele.tif", overwrite=TRUE)
x
s1
crs(s1)
ss <- s1
crs(ss) <- ""
crs(ss)


newcrs <- "+proj=robin +datum=WGS84"
rob <- terra::project(s1, newcrs)
rob
p2 <- terra::project(rob, "+proj=longlat +datum=WGS84")
r <- rast(xmin=-110, xmax=-90, ymin=40, ymax=60, ncols=40, nrows=40)
values(r) <- 1:ncell(r)
r
plot (r)
newcrs
pr1 <- terra::project(r, newcrs)
crs(pr1)
plot(pr1)

x <- rast(pr1)
# Set the cell size
res(x) <- 200000
pr3 <- terra::project(r, x)
pr3
plot(pr3)

TWN <- vect('data/gadm/gadm41_TWN_1_pk.rds')
plot(TWN, "NAME_1")

url <- 'https://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=72874C55-884D-4CEA-B7D6-F60B0BE85AB0'
path1 <- tempfile(fileext = ".zip")
if (file.exists(path1))  'file alredy exists' else download.file(url, path1, mode="wb")
zip::unzip(zipfile = path1,exdir = 'data')
Taiwan <- "data/COUNTY_MOI_1130718.shp"
Taiwan <-vect(Taiwan)
plot(Taiwan, "COUNTYENG")





install.packages("sp")
install.packages("rgbif")
install.packages("leaflet")
library(terra)
library(rgbif)
library(sp)
library(leaflet)

species_data <- occ_search(scientificName = "Kurixalus idiootocus", limit = 500)
occurrences <- species_data$data
coordinates <- occurrences[, c("decimalLongitude", "decimalLatitude")]
species_points <- vect(coordinates, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")

taiwan <- vect('data/gadm/gadm41_TWN_1_pk.rds')
plot(taiwan, col = "lightgray", xlim=c(119,123), ylim=c(21,26), main = "Distribution of Moltrecht’s Tree Frog")
plot(species_points, col = "blue", pch = 20, cex = 0.5, add = TRUE) # 'add = TRUE' to overlay points


plot(species_points, col = "blue", pch = 20, cex = 0.5, main = "Distribution of Moltrecht’s Tree Frog")


# Create a basic leaflet map
leaflet() %>%
  addTiles() %>%
  setView(lng = 121.0, lat = 23.5, zoom = 7) # Center the map on Taiwan

# Adding points for Moltrecht’s Tree Frog occurrences
leaflet() %>%
  addTiles() %>%
  setView(lng = 121.0, lat = 23.5, zoom = 7) %>%
  addCircleMarkers(data = occurrences,
                   lng = ~decimalLongitude, 
                   lat = ~decimalLatitude,
                   color = "purple",
                   radius = 3,
                   stroke = FALSE, 
                   fillOpacity = 0.5,
                   popup = ~paste("Longitude: ", decimalLongitude, "<br>", "Latitude: ", decimalLatitude))

