library(XML)
library(leaflet)

# change directory to this location before running
# setwd("/path/to/files")

gpx_parsed <- htmlTreeParse("marathon.gpx", useInternalNodes = TRUE)

coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)

df <- data.frame(
  lat = as.numeric(coords["lat", ]),
  lon = as.numeric(coords["lon", ])
)

# natural map background + white trace
leaflet(options = leafletOptions(minZoom = 12.5, maxZoom = 12.5, attributionControl=FALSE, zoomControl = FALSE)) %>% addTiles() %>% addProviderTiles("CartoDB.Voyager") %>% addPolylines(data = df, lat = ~lat, lng = ~lon, color = "#166416", opacity = 1, weight = 1.25)
