library(XML)
library(leaflet)

# change directory to this location before running
# setwd("/path/to/files")

gpx_parsed <- htmlTreeParse("sg.gpx", useInternalNodes = TRUE)

coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)

df <- data.frame(
  lat = as.numeric(coords["lat", ]),
  lon = as.numeric(coords["lon", ])
)

# minimal black theme + white trace
# CartoDB.PositronNoLabels for white background
leaflet(options = leafletOptions(attributionControl=FALSE, zoomControl = FALSE)) %>% addTiles() %>% addProviderTiles("CartoDB.DarkMatterNoLabels") %>% addPolylines(data = df, lat = ~lat, lng = ~lon, color = "#FFFFFF", opacity = 1, weight = 1.25)
