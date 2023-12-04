defineModule(sim, list(
  name = "attributionCovars",
  description = paste0("This module gets data and prepares the covariates for the models described ",
                       " in the paper attributing the percentage of boreal songbird loss to forestry",
                       " in breeding grounds, deforestation in wintering grounds and climate change."),
  keywords = c("attributions", "causes for decline", "boreal songbirds", "big data", "rasters"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", 
                                role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", comment = NULL)), 
                      class = "person"),
  childModules = character(0),
  version = list(attributionCovars = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(1985, 2015)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "attributionCovars.Rmd"),
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 2.0.3.9000)",
                  "terra", "PredictiveEcology/reproducible@modsForLargeArchives (>= 2.0.10.9003)", 
                  "data.table", "googledrive"),
  parameters = bindrows(
    defineParameter("cleanScratch", "character", NULL, NA, NA,
                    paste0("If the scratch dir should be claned after each focal operation, pass",
                           "the directory here")),
    defineParameter("classesToExcludeInLCC", "numeric", c(0, 20, 31, 32, 33, 40, 50, 80, 81, 100), NA, NA,
                    paste0("The landcover classes to convert to 0 in the RTM (so the focal does not)",
                           "consider these as habitat for the birds")),
    defineParameter("nx", "numeric", 2, 1, NA, 
                    paste0("The number of tiles to split raster into, along horizontal axis")),
    defineParameter("ny", "numeric", 2, 1, NA, 
                    paste0("the number of tiles to split raster into, along vertical axis")),
    defineParameter("rType", "character", "INT1U", NA, NA, 
                    paste0("pixel data type for splitRaster")),
    defineParameter("buffer", "numeric", 35, 0, NA, 
                    paste0("the number of cells to buffer tiles ",
                           "during splitRaster. Measured in cells (pixels), not distance")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, 
                    paste0("Should this entire module be ",
                           "run with caching activated ? ",
                           "This is generally intended for ",
                           "data - type modules, where ",
                           "stochasticity and time are ",
                           "not relevant")),
    defineParameter("useParallel", "logical", FALSE, NA, NA, 
                    "Should we parallelize tile processing?"),
    defineParameter("focalDistance", "list", list(local = c(1, 100), 
                                                  neighborhood = c(100, 500), 
                                                  landscape = c(500, 1000)), NA, NA, 
                    paste0("A list over all the distances at which to compute focal statistics,",
                           "in units of the input rastesr CRS. This will be used to ", 
                           "create a matrix with circular weights summing to 1)", 
                           "It is important to note that if the element in the list is a length 2",
                           "vector, the first element will be the annulus of the second")),
    defineParameter("recoverTime", "numeric", 30, NA, NA, 
                    paste0("How long are we considering a forest to be unsuitable for birds?")),
    defineParameter("resampledRes", "numeric", 30, NA, NA, 
                    paste0("Normally, the module will get this info from the main raster, but to check",
                           " if the data already exists, we need to pass the information.")),
    defineParameter("yearsForFullRasters", "numeric", c(1985, 2015), NA, NA, 
                    paste0("The years for which full predictions should be done.",
                           " In this case, 1985 and 2015, which match Rosenberg et al., 2019")),
    defineParameter(".studyAreaName", "character", "boreal", NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "studyArea", objectClass = "", 
                 desc = paste0("Shapefile of the study area.",
                               "It is needed only if the disturbance raster is not provided.",
                               "Used in .inputObjects but not in the simuation")),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer", 
                 desc = paste0("Raster that has only 1's inside the study area and NA's outside.",
                               "This is the study area shapefile rasterized.",
                               "This object is needed for ",
                               "fixing the disturbance layers if these only have values when the",
                               " pixel has been disturbed, and NA's otherwise")),
    expectsInput(objectName = "locationsToExtract", objectClass = "SpatVector", 
                 desc = paste0("Points as a SpatVector with the locations of the bird point counts.",
                               " This is used to extract the covariates from the original rasters."))
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "disturbanceBreedingGrounds", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one for a year of ",
                                "focal disturbances (in a stack if more than one focal distance)")),
    createsOutput(objectName = "climateRaster", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one for a year of ",
                                "climate variables (in a stack if more than one variable)")),
    createsOutput(objectName = "LandCoverRaster", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one for a year of ",
                                "landcover rasters")),
    createsOutput(objectName = "disturbanceWinteringGrounds", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one for a year of ",
                                "focal disturbances")),
    createsOutput(objectName = "covariatesTable", objectClass = "data.table", 
                  desc = paste0("This is a data table containing all the following information: ",
                                "location (x), location (y), variable, value, year, where variable ",
                                "may be: MAP, MAT, NFFD, PPT_wt, Tave_sm, landcover, BCR, Province,
                                species, counts, offsets, focal_0, focal_100, focal_500, focal_1000, 
                                disturbanceWintering")),
    createsOutput(objectName = "covariatesStack", objectClass = "SpatRaster", 
                  desc = paste0("This is a raster stack in terra containing all the following layers: ",
                                "MAP, MAT, NFFD, PPT_wt, Tave_sm, landcover, BCR, Province, focal_0, 
                                focal_100, focal_500, focal_1000, disturbanceWintering. These are ",
                                "'new data' for forecasting."))
  )
))

doEvent.attributionCovars = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # Example function to check if all data is already available. Maybe worth adding in the end?
      # sim$focalYearList <- checkForFinalObject(years = time(sim), 
      #                                          focalDistances = P(sim)$focalDistance, 
      #                                          resampledRes = P(sim)$resampledRes, 
      #                                          pathData = dataPath(sim))
      
      # Schedule the events
      sim <- scheduleEvent(sim, start(sim), "attributionCovars", "prepareRTMandLocationData")
      sim <- scheduleEvent(sim, start(sim), "attributionCovars", "getData")
      sim <- scheduleEvent(sim, start(sim), "attributionCovars", "prepareDisturbanceBreedingGrounds")
      sim <- scheduleEvent(sim, start(sim), "attributionCovars", "prepareClimate")
      sim <- scheduleEvent(sim, start(sim), "attributionCovars", "prepareLandcover")
      sim <- scheduleEvent(sim, start(sim), "attributionCovars", "prepareDisturbanceWinteringGrounds")
      sim <- scheduleEvent(sim, start(sim), "attributionCovars", "prepareCovariatesTableAndRasters")
    },
    prepareRTMandLocationData = {
      cacheTags <- c(currentModule(sim), "function:prepareRTMandLocationData")
      
      # # Prepare RTM (split)
      mod$splittedRTM <- tryCatch({
        Cache(SpaDES.tools::splitRaster,
              r = sim$rasterToMatch,
              nx = P(sim)$nx,
              ny = P(sim)$ny,
              buffer = P(sim)$buffer,
              rType = P(sim)$rType,
              path = dataPath(sim),
              userTags = paste0("splitRTM_x", P(sim)$nx, "y",
                                P(sim)$ny))
      }, error = function(e){
        message(paste0("Caching of ", paste0("splitRTM_x", P(sim)$nx, "y",
                                             P(sim)$ny), " failed. Trying to manually ",
                       "recover the file..."))
        TAG <- paste0("splitRTM_x", P(sim)$nx, "y",
                      P(sim)$ny)
        ch <- setkey(showCache(userTag = TAG), "createdDate")
        cacheID <- ch[NROW(ch), cacheId]
        fl <- list.files(Paths$cachePath, recursive = TRUE,
                         full.names = TRUE, pattern = cacheID)
        qs::qread(fl)
      })
      
      # Prepare Location data matching RTM's splitted rasters. This needs to be a vector of 
      # points (terra::vect object) to extract the rasters' information from. 
      mod$splittedLocations <- splitLocations(splittedRTM = mod$splittedRTM,
                                              locations = sim$locationsToExtract) # Crop the points to lists to match the extent of the raster.
      },
    getData = {
      cacheTags <- c(currentModule(sim), "function:getData")
      # Check for each dataset. If existing, no need to get the data again. 
      
      mod$disturbanceBreeding <- Cache(prepInputs, url = paste0(
        "https://opendata.nfis.org/downloads/",
        "forest_change/CA_forest_harvest_mask",
        "_year_1985_2015.zip"),
        rasterToMatch = sim$rasterToMatch,
        studyArea = sim$studyArea,
        userTags = c("objectName:disturbanceBreeding",
                     cacheTags,
                     "outFun:Cache", "goal:prepDisturbanceBreeding"),
        omitArgs = c("overwrite", "destinationPath"),
        targetFile = "CA_harvest_year_1985_2015.tif",
        destinationPath = getOption("reproducible.inputPaths"))
      
      mod$LandCoverRaster <- Cache(prepInputs, url = paste0(
        "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", time(sim),".zip"),
        rasterToMatch = sim$rasterToMatch,
        studyArea = sim$studyArea,
        userTags = c(paste0("objectName:landcover_", time(sim)),
                     cacheTags,
                     "outFun:Cache", "goal:prepLandcover"),
        omitArgs = c("overwrite", "destinationPath"),
        targetFile = paste0("CA_forest_VLCE2_", time(sim),".tif"),
        destinationPath = getOption("reproducible.inputPaths"))
      
      mod$climateRaster <- Cache(prepInputs, url = paste0(
        "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", time(sim),".zip"),
        rasterToMatch = sim$rasterToMatch,
        studyArea = sim$studyArea,
        userTags = c(paste0("objectName:landcover_", time(sim)),
                     cacheTags,
                     "outFun:Cache", "goal:prepLandcover"),
        omitArgs = c("overwrite", "destinationPath"),
        targetFile = paste0("CA_forest_VLCE2_", time(sim),".tif"),
        destinationPath = getOption("reproducible.inputPaths"))
      
    },
    prepareDisturbanceBreedingGrounds = {
      
      # yearsForFullRasters # MAKE SURE THIS GETS CHECKED AND THE FULL RASTERS ARE CREATED ONLY FOR THESE 
      # locationsToExtract # Make sure to use this
      
      browser() 
      mod$splittedDisturbanceBreeding <- tryCatch({
        Cache(SpaDES.tools::splitRaster,
              r = mod$disturbanceBreeding,
              nx = P(sim)$nx,
              ny = P(sim)$ny,
              buffer = P(sim)$buffer,
              rType = P(sim)$rType,
              path = dataPath(sim),
              userTags = paste0("splitDisturbance_x", P(sim)$nx, "y",
                                P(sim)$ny))
      }, error = function(e){
        TAG <- paste0("splitDisturbance_x", P(sim)$nx, "y", 
                      P(sim)$ny)
        ch <- setkey(showCache(userTag = TAG), "createdDate")
        cacheID <- ch[NROW(ch), cacheId]
        fl <- list.files(Paths$cachePath, recursive = TRUE, 
                         full.names = TRUE, pattern = cacheID)
        qs::qread(fl)
      })
      
      sim$focalYearList[[paste0("Year", time(sim))]] <- applyFocalToTiles(listTilesDisturbance = sim$splittedDisturbanceBreeding,
                                                                          listTilesLCC = sim$splittedLCC,
                                                                          listTilesRTM = sim$splittedRTM,
                                                                          pathCache = Paths$cachePath,
                                                                          focalDistance = P(sim)$focalDistance,
                                                                          recoverTime = P(sim)$recoverTime,
                                                                          pathData = dataPath(sim),
                                                                          year = time(sim),
                                                                          classesToExcludeInLCC = P(sim)$classesToExcludeInLCC,
                                                                          doAssertions = P(sim)$doAssertions,
                                                                          cleanScratch = P(sim)$cleanScratch)
      
    
      sim <- scheduleEvent(sim, time(sim)+1, "attributionCovars", "prepareDisturbanceBreedingGrounds")
    },
    prepareClimate = {
      
      # yearsForFullRasters # MAKE SURE THIS GETS CHECKED AND THE FULL RASTERS ARE CREATED ONLY FOR THESE 
      # locationsToExtract # Make sure to use this
      
      browser() 
      mod$splittedClimate <- tryCatch({
        Cache(SpaDES.tools::splitRaster, 
              r = sim$climateRaster,
              nx = P(sim)$nx,
              ny = P(sim)$ny,
              buffer = P(sim)$buffer,
              rType = P(sim)$rType,
              path = dataPath(sim),
              userTags = paste0("splitLCC_x", P(sim)$nx, "y",
                                P(sim)$ny))
      }, error = function(e){
        TAG <- paste0("splitClim_x", P(sim)$nx, "y", 
                      P(sim)$ny)
        ch <- setkey(showCache(userTag = TAG), "createdDate")
        cacheID <- ch[NROW(ch), cacheId]
        fl <- list.files(Paths$cachePath, recursive = TRUE, 
                         full.names = TRUE, pattern = cacheID)
        qs::qread(fl)
      })
      
      sim <- scheduleEvent(sim, time(sim)+1, "attributionCovars", "prepareClimate")
    },
    prepareLandcover = {
      
      # yearsForFullRasters # MAKE SURE THIS GETS CHECKED AND THE FULL RASTERS ARE CREATED ONLY FOR THESE 
      # locationsToExtract # Make sure to use this
      # "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_1984.zip"
      browser() 
      mod$splittedLCC <- tryCatch({
        Cache(SpaDES.tools::splitRaster, 
              r = sim$LandCoverRaster,
              nx = P(sim)$nx,
              ny = P(sim)$ny,
              buffer = P(sim)$buffer,
              rType = P(sim)$rType,
              path = dataPath(sim),
              userTags = paste0("splitLCC_x", P(sim)$nx, "y",
                                P(sim)$ny))
      }, error = function(e){
        TAG <- paste0("splitLCC_x", P(sim)$nx, "y", 
                      P(sim)$ny)
        ch <- setkey(showCache(userTag = TAG), "createdDate")
        cacheID <- ch[NROW(ch), cacheId]
        fl <- list.files(Paths$cachePath, recursive = TRUE, 
                         full.names = TRUE, pattern = cacheID)
        qs::qread(fl)
      })      
      
      sim <- scheduleEvent(sim, time(sim)+1, "attributionCovars", "prepareLandcover")
    },
    prepareDisturbanceWinteringGrounds = {
      
      # yearsForFullRasters # MAKE SURE THIS GETS CHECKED AND THE FULL RASTERS ARE CREATED ONLY FOR THESE 
      # locationsToExtract # Make sure to use this
      browser()
      sim <- scheduleEvent(sim, time(sim)+1, "attributionCovars", "prepareDisturbanceWinteringGrounds")
    },
    prepareCovariatesTableAndRasters = {
      
      # Get it all together in one table and a raster stack
      
      # sim$covariatesTable: location (x), location (y), variable, value, year, where variable MAP, 
      #                      MAT, NFFD, PPT_wt, Tave_sm, landcover, BCR, Province, focal_0, 
      #                      focal_100, focal_500, focal_1000, disturbanceWintering
      # sim$covariatesStack: MAP, MAT, NFFD, PPT_wt, Tave_sm, landcover, BCR, Province, focal_0, 
      #                      focal_100, focal_500, focal_1000, disturbanceWintering
       
      browser() # Get all data needed for each year
      
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!any(all(suppliedElsewhere("studyArea", sim), suppliedElsewhere("rasterToMatch", sim)),
           all(!suppliedElsewhere("studyArea", sim), !suppliedElsewhere("rasterToMatch", sim))))
    stop("Please, either supply both studyArea and rasterToMatch, or none to use defaults (Canadian Boreal)")
  
  if (!suppliedElsewhere("studyArea", sim)){
    sim$studyArea <- Cache(prepInputs, url = "https://drive.google.com/file/d/1of3bIlPnLMDmumerLX-thILHtAoJpeiC", 
                           targetFile = "studyArea.shp", 
                           archive = "studyArea.zip", 
                           alsoExtract = "similar", 
                           destinationPath = getOption("reproducible.inputPaths"), 
                           fun = "terra::vect",
                           userTags = c("objectName:studyArea",
                                        cacheTags,
                                        "goal:sA"),
                           omitArgs = c("destinationPath"))
    
  }

  if (!suppliedElsewhere("rasterToMatch", sim)){
    sim$rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/file/d/1hSn2Rdiyou9znGRfmJlsUJ-p2fcHX6Uy",
                               targetFile = "processedRTM.tif",
                               fun = "terra::rast",
                               archive = "processedRTM.zip",
                               destinationPath = getOption("reproducible.inputPaths"),
                               userTags = c("originPoint:global", "objectName:rasterToMatch"),
                               omitArgs = "destinationPath")
  }
  
  if (!suppliedElsewhere("locationsToExtract", sim)){
  # Make a module to get this data once BAM makes it available
  # In the meantime, loading a local potentially compatible dataset (from Alberto's work)
  sim$locationsToExtract <- exampleLocations()
  }

  return(invisible(sim))
}

