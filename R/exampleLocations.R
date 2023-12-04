exampleLocations <- function(studyArea, RTM){
  # Points as a SpatVector with the locations of the bird point counts. Needs to match projection
  # from disturbance raster. This is used to extract the covariates from the original rasters.
  # Make sure all points are inside the studyArea and proj matches RTM
  DT <- fread(input = "/home/tmichele/projects/attribution/modules/attributionCovars/data/Minidataset_master.csv")
  dtLoc <- unique(DT[, c("X", "Y")])
  pnts <- vect(dtLoc, geom = c("X", "Y"), crs = "epsg:4326", keepgeom=FALSE)
  pntsReproj <- project(pnts, sim$studyArea)
  browser() # Crop out points that are not inside study area
  # return object
}
