
## Install libraries for maps
#install.packages("ggmap")
#install.packages("maptools")
#install.packages("maps")
library(ggmap)
library(maptools)
library(maps)
library(stringr)
library(sp)
library(maps)
library(maptools)
# Create a function to extract the city name
getCityName <- function(x){
  ret <- ''
  splitVector<- strsplit(x,',')
  tolower(splitVector[[1]][1])
}

# Load the twitter datset into memory this will give us the user dataset
load(url("https://github.com/goodwillyoga/E107project/raw/master/pulkit/twitter.RData"))

# attempt to get the city names in lower case
loc<- filter(users,!is.na(user_loc)) %>% select(user_loc)
result <- lapply(loc$user_loc,getCityName) %>% unlist()

# Filter out city names that have more than 10 entries. Google limits the use of maps api to access only 2500 records in a day
filteredList <- as.data.frame(table(result), stringsAsFactors =FALSE) %>% filter(Freq > 8 ) 
#Make a character vector of city names
cities <- c(filteredList$result)

# Call the google api to get the latitude, longitudes
latLong <- geocode(cities)

png("/code/CSCIE-107/E107project/pulkit/userpresence.png", width=10, height=6, units="in", res=300)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(latLong$lon,latLong$lat, col="red", pch=16)
title("Tweets User distribution")
dev.off()

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}
# Make a dataset of longitude and latitude only
US_statePoints <- data.frame(x =latLong$lon, y = latLong$lat)
# Get a list of state points based on longitude/latitude for the cities
state_freq<- latlong2state(US_statePoints)
# Sum up number of users in each state
joined_df <- data.frame(region = state_freq, freq = filteredList$Freq) %>% 
  group_by(region) %>% 
  summarise(count = sum(freq))

# Get all states data
all_states <- map_data("state")
mergedDataset <- merge(all_states, joined_df, by="region")
# remove district of columbia from the dataset
mergedDataset <- mergedDataset[mergedDataset$region!="district of columbia",]

png("/code/CSCIE-107/E107project/pulkit/stateuserdistribution.png", width=10, height=6, units="in", res=300)

ggplot(data=mergedDataset, aes(x=long, y=lat, group=group, fill=count)) + 
  scale_fill_gradientn("",colours=brewer.pal(15,"YlOrRd"))+
  geom_polygon()+coord_map()+
  labs(fill="State wise distribuiton of Users",
       title="State wise distribuiton of Users",x="",y="")+theme_bw()

dev.off()