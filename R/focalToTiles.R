focalToTiles <- function(tiles,
                         totalTiles,
                         orderedRasterList,
                         pathData,
                         focalDistance,
                         recoverTime,
                         resampledRes,
                         currentYear,
                         classesToExcludeInLCC,
                         doAssertions = TRUE,
                         cleanScratch = NULL){

  doLCC <- TRUE
  
  finalRasFilePathForestedIndex <- file.path(pathData, paste0("processed/isForest_Tile",
                                                          tiles, "Res", resampledRes, "m.tif"))
  finalRasFilePathForested <- file.path(pathData, paste0("processed/proportionOfForest_Tile",
                                                         tiles, "Res", resampledRes, "mFocal", 
                                                         max(focalDistance),"m.tif"))
  # Disturbance side
  finalRasFilePathForested30YPlusIndex <- file.path(pathData, paste0("processed/is30YPlusForest_Tile",
                                                              tiles, "Year", currentYear, 
                                                              "Res", resampledRes, "m.tif"))
  if (all(file.exists(finalRasFilePathForested),
          file.exists(finalRasFilePathForestedIndex))){
    message(crayon::green(paste0("Forested and ForestedIndex rasters from ", paste0("Tile ", tiles),
                                  " already exist. Saving time by skipping...")))
    doLCC <- FALSE
  }
  
  # Testing for final raster. If it exists, return. Otherwise, create
  finalRasFilePath <- file.path(pathData, paste0("processed/proportionOf30YPlusForest_Tile",
                                                 tiles, "Year", currentYear,
                                                 "Res", resampledRes, "mFocal",
                                                 max(focalDistance),"m.tif"))
  if (all(file.exists(finalRasFilePath), !doLCC)){
    message(crayon::green(paste0("Raster from ", paste0("Tile ", tiles),
                                  " for year ", currentYear, " and focal distance ", 
                                 max(focalDistance),
                                  " already exists. Returning saved object...")))
    proportionOfForest <- raster::raster(finalRasFilePathForested)
    proportionOf30YPlusForest <- raster::raster(finalRasFilePath)
    isForest <- raster::raster(finalRasFilePathForestedIndex)
    is30YPlusForest <- raster::raster(finalRasFilePathForested30YPlusIndex)
    gc()
    return(list("proportionOfForest" = proportionOfForest,
                "proportionOf30YPlusForest" = proportionOf30YPlusForest,
                "isForest" = isForest,
                "is30YPlusForest" = is30YPlusForest))
  } else {
    
    # Sanity check: we can't have the disturbance done and not have the LCC
    if (all(file.exists(finalRasFilePath), doLCC))
      stop("The Forested or ForestedIndex doesn't exist, but ", finalRasFilePath, 
           " does. Something went wrong. Debug focalToTiles.R function")
    
    if (doLCC){
      LCC <- orderedRasterList[[tiles]][[2]]
    } else {
      LCC <- raster::raster(finalRasFilePathForestedIndex)
    }

    ######################################################################## 
    #                                                                      #
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ isForest  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    #                                                                      #
    ######################################################################## 

    message(crayon::yellow(paste0("Bringing LCC for ", 
                                  paste0("tile ", tiles),
                                  " to memory for faster processing...")))
    LCC[] <- LCC[] # Bring raster to memory, faster processing 
    storage.mode(LCC[]) <- "integer" # Reducing size of raster by converting it to a real integer
    gc()
    
    if (any(LCC@data@max == 0, all(is.na(LCC[])) == TRUE)) {
      
      message(crayon::red(paste0("Year ", currentYear, " for Tile ", tiles, 
                                 " was skipped as it doesn't have data")))

      return(list("proportionOfForest" = orderedRasterList[[tiles]][[1]],
                  "proportionOf30YPlusForest" = orderedRasterList[[tiles]][[2]],
                  "isForest" = orderedRasterList[[tiles]][[1]],
                  "is30YPlusForest" = orderedRasterList[[tiles]][[1]]))
    }
      # 4. create focal matrix (for each distance parameter). 
      if(length(focalDistance)> 1) {
        # If you pass two focalDistances, it will make an annulus (you should only have 2!).
        # make inner matrix
        focalMatrices <- Cache(focalWindowAnnulus,
                               focalDistance = focalDistance,
                               ras = LCC,
                               userTags = paste0("focalWindowAnnulus",
                                                 max(focalDistance), "Res",
                                                 resampledRes, "m"))
        
      } else {
        focalMatrices <- Cache(focalWindow, 
                               x = LCC, 
                               d = focalDistance,
                               userTags = paste0("focalWindow_Focal", 
                                                max(focalDistance), "Res", 
                                                resampledRes, "m"))
      }
    
    # Bring the disturbance raster for the current year
    disturbanceRas <- orderedRasterList[[tiles]][[1]]
    disturbanceRas[] <- disturbanceRas[]
    storage.mode(disturbanceRas[]) <- "integer" # Reducing size of raster by converting it to a real integer
    gc()
    
    #  We will need to change the NA's to zeros. The disturbance raster 
    #  has NA's instead of zeros where forestry does not occur, not only 
    #  outside of the study area as LCC. This will mess up the focal calculation
    #  if not fixed.
    disturbanceRas[is.na(disturbanceRas[])] <- 0
    
    # 2. Select which years to mask for
    if (currentYear > 1000){
      currentYear <- currentYear - 1900
    }
    
    maskValue <- c((currentYear - recoverTime) : currentYear)
    
    # Replace values of interest for big ones (999)
    message(crayon::white(paste0("Masking disturbance in year ", currentYear,
                                  " as ", paste(currentYear - recoverTime, currentYear, sep = ":"),
                                  " for focal distance ", max(focalDistance),
                                  " to forest -- Tile ", tiles,
                                  " of ", totalTiles, " tiles (Time: "
                                  , Sys.time(), ")")))
    
    disturbanceRas[disturbanceRas[] %in% maskValue] <- 999
    
    # 3. Converting raster to binary to select only disturbances of the specific year
    disturbanceRas[disturbanceRas[] != 999] <- 0
    disturbanceRas[disturbanceRas[] == 999] <- 1
    storage.mode(disturbanceRas[]) <- "integer" # Reducing size of raster
    gc()
    
    RTM <- orderedRasterList[[tiles]][[3]]
    RTM[] <- RTM[]
    storage.mode(RTM[]) <- "integer" # Reducing size of raster by converting it to a real integer
    
      if (doLCC){
        # 0. Here we need to convert the LCC classes that are not forest into 0 so our focal result  
        # is the focal of the forested area (i.e. the proportionOfForest_XXX)
        
        if (!file.exists(finalRasFilePathForestedIndex)){
          
          if (!is.null(classesToExcludeInLCC)){
            LCC[LCC[] %in% classesToExcludeInLCC] <- 0
          }
          valsLCC <- raster::getValues(LCC)
          valsLCC[valsLCC != 0] <- 1
          LCC <- raster::setValues(raster(LCC), valsLCC)
          storage.mode(LCC[]) <- "integer" # Reducing size of raster by converting it to a real integer
          # LCC[LCC[] != 0] <- 1 # Failing to plot, not necessarily to work. Will do above JIC
          gc()
          
          if (doAssertions){
            testthat::expect_true(all(all(!is.na(LCC[])), all(LCC[] %in% c(0, 1))))
          }
          
          # Because disturbance happens BEFORE the Landcover snapshot, we need to include in
          # 1985 isForest and proportionForest the forest that is cut in 1985. So we need 1985
          # disturbance layer. Wherever disturbance happened, we had forest!
          LCC[disturbanceRas[] == 1] <- 1
          
          LCC@data@names <- paste0("isForest_Tile", tiles)
          writeRaster(LCC, filename = finalRasFilePathForestedIndex, 
                      format = "GTiff",
                      datatype = "INT1U")
          gc()
        } else {
          LCC <- raster::raster(finalRasFilePathForestedIndex)
          LCC[] <- LCC[]
          storage.mode(LCC[]) <- "integer" # Reducing size of raster by converting it to a real integer
          gc()
        }
      
        ######################################################################## 
        #                                                                      #
        # ~~~~~~~~~~~~~~~~~~~~~~~~ proportionOfForest  ~~~~~~~~~~~~~~~~~~~~~~~ #
        #                                                                      #
        ######################################################################## 

        message(crayon::yellow(paste0("Applying focal operation on Forested pixels for Tile ", 
                                      tiles, " of ", totalTiles, " tiles (Time: "
                                      , Sys.time(), ")")))
        
        # Here I get the *sum* of the forested pixels over the entire focal window:
        proportionOfForest <- individualFocal(ras = LCC,
                               weightMatrix = focalMatrices)
        gc()
        # Here I get the *proportion* of the forested pixels over the entire focal window:
        proportionOfForest <- proportionOfForest/sum(focalMatrices)
        
        # I can't mask the proportionOfForest it in the individualFocal, otherwise I
        # end up with places that are NOT forest having ZERO proportion of forest around it and,
        # therefore, we would not have the correct density of birds (we would be underestimating 
        # it)!
        # So to maks out the outside of the study area, I need to use the RTM
        proportionOfForest[RTM[] == 0] <- 0
        gc()
        
        # Writing available pixels' raster
        proportionOfForest@data@names <- paste0("proportionOfForest_Tile", tiles,
                                 "Focal", max(focalDistance), "m")
        writeRaster(proportionOfForest, filename = finalRasFilePathForested, 
                    format = "GTiff",
                    datatype = "FLT4S")
      }
      
    ######################################################################## 
    #                                                                      #
    # ~~~~~~~~~~~~~~~~~~~~~~~ isForest30Year+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    #                                                                      #
    ######################################################################## 

      if (!file.exists(finalRasFilePathForested30YPlusIndex)){
        
        # This raster now has 1's everywhere, except the pixels that were disturbed. 
        # So we need to mask it to whatever is forest. What is NOT forest CANNOT be 
        # 30YPlus forest

        LCC[] <- LCC[] # Bring raster to memory, faster processing 
        storage.mode(LCC[]) <- "integer" # Reducing size of raster by converting it to a real integer
        gc()
        is30YPlusForest <- LCC - disturbanceRas
        # NOTE: Here we have LCC, but this is basically the raster showing which LCC are forest (1),
        # and which ones are not (0). What is NOT forest CANNOT be 30YPlus forest
        
        # Writing available pixels' raster
        is30YPlusForest@data@names <- paste0("is30YPlusForest_Tile", tiles, "Year", currentYear)
        writeRaster(is30YPlusForest, filename = finalRasFilePathForested30YPlusIndex, 
                    format = "GTiff",
                    datatype = "INT1U")
      } else {
        is30YPlusForest <- raster::raster(finalRasFilePathForested30YPlusIndex)
        is30YPlusForest[] <- is30YPlusForest[] # Bring raster to memory, faster processing 
        storage.mode(is30YPlusForest[]) <- "integer" # Reducing size of raster by converting it to a real integer
        gc()
      }
      
      # =============== F O C A L   C A L C U L A T I O N S =============== #

      # 3. calculate focal statistics with each matrix
      message(crayon::yellow(paste0("Applying focal operation on distrubed pixels for ",
                                    " focal distance ", max(focalDistance),
                                    " for Tile ", 
                                    tiles, " of ", totalTiles, " tiles (Time: "
                                    , Sys.time(), ")")))

      # Apply focal
      proportionOf30YPlusForestTile <- individualFocal(ras = is30YPlusForest,
                                                       weightMatrix = focalMatrices)
      gc()
      # Here I get the *proportion* of the disturbed pixels over the entire focal window:
      proportionOf30YPlusForestTile <- proportionOf30YPlusForestTile/sum(focalMatrices)
      
      # I can't mask the proportionOfForestPlus it in the individualFocal, otherwise I
      # end up with places that are NOT forest having ZERO proportion of forest around it and,
      # therefore, we would not have the correct density of birds (we would be underestimating it)!
      # So to maks out the outside of the study area, I need to use the RTM
      proportionOf30YPlusForestTile[RTM[] == 0] <- 0
      
      # Writing available pixels' raster
      proportionOf30YPlusForestTile@data@names <- paste0("proportionOf30YPlusForest_Tile",
                                                         tiles, "Year", currentYear, 
                                                         "Focal", max(focalDistance),"m")
      
      writeRaster(proportionOf30YPlusForestTile, filename = finalRasFilePath,
                  format = "GTiff",
                  datatype = "FLT4S")
      rm(disturbanceRas)
      gc()
      
      proportionOfForest <- raster::raster(finalRasFilePathForested)
      gc()
      proportionOf30YPlusForest <- raster::raster(finalRasFilePath)
      gc()
      isForest <- raster::raster(finalRasFilePathForestedIndex)
      gc()
      is30YPlusForest <- raster::raster(finalRasFilePathForested30YPlusIndex)
      gc()
      
      message(crayon::green(paste0("Focal operations for focal distance ", max(focalDistance),
                                   " for Tile ", tiles, " of ", totalTiles, 
                                   " tiles has been completed (Time: "
                                    , Sys.time(), ")")))
      if (!is.null(cleanScratch))
        unlink(cleanScratch, recursive = TRUE)
        
      return(list("proportionOfForest" = proportionOfForest,
                  "proportionOf30YPlusForest" = proportionOf30YPlusForest,
                  "isForest" = isForest,
                  "is30YPlusForest" = is30YPlusForest))
  }
}