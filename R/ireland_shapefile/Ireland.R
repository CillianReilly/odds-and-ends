library(sf)
library(dplyr)
library(ggplot2)

# change directory to this location before running
# setwd("/path/to/files")

ROI_shp <- st_read("geoBoundaries-IRL-ADM0-all/geoBoundaries-IRL-ADM0.shp")
NI_shp <- st_read("OSNI_Open_Data_-_Largescale_Boundaries_-_NI_Outline/OSNI_Open_Data_-_Largescale_Boundaries_-_NI_Outline.shp")

remove_holes <- function(sf_object) {
  sf_object %>%
    st_cast("MULTIPOLYGON") %>%  # ensure consistent geometry type
    mutate(geometry = st_sfc(lapply(geometry, function(multi_poly) {
      # For each polygon in the multipolygon
      filtered_polys <- lapply(multi_poly, function(single_poly) {
        # Keep only the exterior ring (first one)
        exterior <- single_poly[1]

        # Ensure the ring is closed
        if (!all(exterior[[1]][1, ] == exterior[[1]][nrow(exterior[[1]]), ])) {
          exterior[[1]] <- rbind(exterior[[1]], exterior[[1]][1, ])
        }

        st_polygon(exterior)
      })
      st_multipolygon(filtered_polys)
    }), crs = st_crs(sf_object)))
}


sf_use_s2(FALSE)
IE_shp=st_union(ROI_shp, NI_shp)
IE_shp <- remove_holes(IE_shp)
IE_shp <- st_buffer(IE_shp, dist = 0)

# dummy co-ordinates
points <- data.frame(lon=-7.9168497,lat=53.4347678)
points_sf <- st_as_sf(points,coords=c("lon", "lat"), crs = 4326)

print(ggplot() +  geom_sf(data = IE_shp, fill="white", colour="black", linewidth=0.5) +  geom_sf(data = points_sf, size = 0.5) + theme_void())

# write to pdf
# pdf("plot.pdf")
# ggplot() +  geom_sf(data = shp_clean, fill="white", colour="black", linewidth=0.25) +  geom_sf(data = points_sf, size = 0.1) + theme_void()
# dev.off()
