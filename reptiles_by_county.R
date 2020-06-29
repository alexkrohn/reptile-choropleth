### Plot all squamate occurence data from GBIF data from iNat
library(choroplethr)
library(choroplethrMaps)
library(tidyverse)
library(USAboundaries)
library(sf)

# May need to install as devtools::install_github("ropensci/USAboundariesData")

data("county.regions")

# Good tutorial here: https://arilamstein.com/blog/2016/03/21/mapping-election-results-r-choroplethr/

# The data were downloaded from the GBIF website (APIs have a hard limit of 200k observations). The dataset has ~357000 inat observation, ~668000 preserved specimens https://doi.org/10.15468/dl.8nheu2

# Search criteria were squamates, with basis of record as human observation, material sample, preserved specimen or unknown within a square of the US (Geometry POLYGON((-129.03717 23.74387,-63.53119 23.74387,-63.53119 49.9415,-129.03717 49.9415,-129.03717 23.74387))), that have coordinates, and where Has geospatial issue false

# Loading the data takes a while, time it. 
# From previous loads, the majority of observations have no county data, or very poorly regulated names ("Coconino", "Coconino County", "coconino", "Cocconino" etc.). All have at least lat long, so let's just focus on those.
# Just take the needed columns to save memory
system.time(all.the.squamates <- read_tsv(file = "~/Documents/UCSC/reptiles_by_month/0008570-200613084148143/occurrence.txt", col_names = TRUE, na = c("NA", NULL), col_types = cols_only(decimalLatitude = col_double(), decimalLongitude = col_double(), month = col_integer()))) # 602,518 observations total. Immediately lost ~400,000 records upon import!! 

# Remove all NAs (for some reason this works, while dplyr::filter() still leaves some NAs)
squamates.no.na <- all.the.squamates[is.na(all.the.squamates$decimalLatitude) == FALSE ,]
squamates.no.na <- squamates.no.na[is.na(squamates.no.na$decimalLongitude) == FALSE,]
# 602,000 left

# Convert lat long data to county name
# Credit to Josh O'Brien for the steps below to convert to county (updated to sf in 2020, and modified for counties)
counties <- USAboundaries::us_counties()
pointsDF <- data.frame(x = squamates.no.na$decimalLongitude , y = squamates.no.na$decimalLatitude)


## Convert points data.frame to an sf POINTS object
pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
## Transform spatial data to some planar coordinate system
## (e.g. Web Mercator) as required for geometric operations
counties <- st_transform(counties, crs = 3857)
pts <- st_transform(pts, crs = 3857)
  
## Find names of state (if any) intersected by each point
county_names <- counties[,c("name","state_name")]
ii <- as.numeric(as.character(st_intersects(pts, counties))) # Can't find a better way to keep the NAs

# Index the intersects against the list of counties
intersecting.counties <- county_names[ii,]

# Drop the geometry column
counties.and.states <- st_drop_geometry(intersecting.counties)

nrow(filter(counties.and.states, is.na(name) == TRUE))/nrow(counties.and.states) # 18% are not in the US or not found in the above intersect function

# Add month back in, and uncapitalize the counties and states
df.with.months <- tibble(county.name = tolower(counties.and.states$name), state.name = tolower(counties.and.states$state_name), month =squamates.no.na$month)

# Remove the counties with NAs (outside a US county, or bad lat longs)
final.df <- df.with.months %>%
  filter(is.na(county.name) == FALSE) %>%
  filter(is.na(state.name) == FALSE) %>%
  filter(is.na(month) == FALSE)
# left with a total of 447142 observations

# Check for any other regions that are NA (i.e. counties from USAboundaries that don't match the names from county.regions)
final.df %>% 
  count(county.name, state.name, name= "value") %>%
  left_join(county.regions, by = c("state.name", "county.name")) %>%
  filter(is.na(region) == TRUE)

# Correct those few examples
final.df$county.name[final.df$county.name == "lasalle" & final.df$state.name == "louisiana"] <- "la salle"
final.df$county.name[final.df$county.name == "doña ana" & final.df$state.name == "new mexico"] <- "dona ana"

# county.regions uses the colonizer name (shannon) for the county of oglala lakota
final.df$county.name[final.df$county.name == "oglala lakota" & final.df$state.name == "south dakota"] <- "shannon"


# Change the numeric months to words (in the right order) and join with county.regions to get the region number
pleth.dataset <- final.df %>%
  mutate(month.names = month.name[month]) %>%
  arrange(factor(month.names, levels = c("January", "February", "March", "April", "May","June", "July", "August", "September", "October", "November", "December"))) %>%
  count(county.name, state.name, month.names, name= "value") %>%
  full_join(county.regions, by = c("state.name", "county.name")) 


animation::saveGIF(
  for (i in month.name) {
 pleth <-  pleth.dataset %>%
    filter(month.names == i) %>%
    full_join(county.regions, by = c("region")) %>% # add in all the missing regions
   mutate(value = log(value)) # put it on a log scale
 pleth$value[is.na(pleth$value)] <- 0 # Replace the NAs for those missing regions with 0
 pleth.plot <- county_choropleth(legend = "Log Number of \nReptiles Observed", pleth, title = i, state_zoom = unique(final.df$state.name), num_colors = 1)
   # scale_fill_stepsn('Reptiles Observed',colors=RColorBrewer::brewer.pal(5,'Spectral'),breaks = c(0,500,1000,2000)) can add custom legends or colors here, but it doesn't work that well
 print(pleth.plot)
  }
)

