# https://github.com/FlowmapBlue/flowmapblue.R 
library("flowmapblue")
locations <- read.csv('https://gist.githubusercontent.com/ilyabo/a7b9701424257146b571149d92a14926/raw/2e9e1e9bcf64cf0090781b451037229ccb78e1b1/locations.csv')
flows <- read.csv('https://gist.githubusercontent.com/ilyabo/a7b9701424257146b571149d92a14926/raw/2e9e1e9bcf64cf0090781b451037229ccb78e1b1/flows.csv')
mapboxAccessToken <- 'YOUR_MAPBOX_ACCESS_TOKEN'
flowmapblue(locations, flows, mapboxAccessToken, clustering=TRUE, darkMode=TRUE, animation=FALSE)

# other ideas
# https://jcheshire.com/visualisation/mapping-flows/

# https://www.r-bloggers.com/2021/10/smooth-flow-maps-and-a-new-edge-bundling-algorithm/

#