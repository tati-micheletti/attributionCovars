applyFocalToTiles <- function(listTilesDisturbance,
                              listTilesLCC,
                              listTilesRTM,
                              pathCache,
                              focalDistance,
                              recoverTime,
                              pathData,
                              year,
                              classesToExcludeInLCC,
                              doAssertions = TRUE,
                              cleanScratch = NULL){
  
  # If the file is cached, then get only the rasters!
  if ("cacheRaster" %in% names(listTilesDisturbance)){
    listTilesDisturbance <- listTilesDisturbance[[1]]
  }
  if ("cacheRaster" %in% names(listTilesLCC)){
    listTilesLCC <- listTilesLCC[[1]]
  }
  if ("cacheRaster" %in% names(listTilesRTM)){
    listTilesRTM <- listTilesRTM[[1]]
  }
  # Subset matching tiles
  listRasters <- list(disturbance = listTilesDisturbance,
                      LCC = listTilesLCC,
                      RTM = listTilesRTM)
  totalTiles <- unique(lengths(listRasters))
  lengthVector <- 1:totalTiles
  orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
    sbset <- unlist(lapply(listRasters, `[[`, index), use.names = FALSE)
    return(sbset)
  })
  message(crayon::green("Tiles organized..."))
  
  names(orderedRasterList) <- paste0("tile", lengthVector)
  
  message(crayon::red(paste0("Starting focal operations for year ", year," (Time: "
                             , Sys.time(), ")")))
  
  invisible(checkPath(file.path(pathData, "processed"), create = TRUE))
  resampledRes <- unique(res(orderedRasterList[[1]][[1]]))
  
    allFocalDistance <- lapply(focalDistance, function(FDISTANCE){

      proportionOfForest <- file.path(pathData, "merged", paste0("merged_proportionOfForest_Res",
                                                       resampledRes, 
                                                       "mFocal", max(FDISTANCE), 
                                                       "m.tif"))
      proportionOf30YPlusForest <- file.path(pathData, "merged", paste0("merged_proportionOf30YPlusForest_Res", 
                                                              resampledRes, "mFocal", max(FDISTANCE), 
                                                              "mYear", year, ".tif"))
      isForest <- file.path(pathData, "merged", paste0("merged_isForest_Res", 
                                             resampledRes, "m.tif"))
      is30YPlusForest <- file.path(pathData, "merged", paste0("merged_is30YPlusForest_Res", 
                                                    resampledRes, "mYear", year, ".tif"))
      
  if (all(file.exists(proportionOf30YPlusForest),
          file.exists(proportionOfForest),
          file.exists(isForest),
          file.exists(is30YPlusForest))) {
    
    message(crayon::green(paste0("Year ",
                                 year, " has already been processed and is being loaded.",
                                 " Skipping to next year...")))
    
    return(list("proportionOfForest" = proportionOfForest,
                "proportionOf30YPlusForest" = proportionOf30YPlusForest,
                "isForest" = isForest,
                "is30YPlusForest" = is30YPlusForest))
    
  } else {

      # Entering each tile group
      # focalTilesToMerge is a list of 2 elements: disturbanceRaster and LCC already tiles and ordered
      focalTilesToMerge <- lapply(X = lengthVector, FUN = focalToTiles, 
                                  totalTiles = totalTiles,
                                  orderedRasterList = orderedRasterList,
                                  pathData = pathData,
                                  focalDistance = FDISTANCE,
                                  recoverTime = recoverTime,
                                  resampledRes = resampledRes,
                                  currentYear = year,
                                  classesToExcludeInLCC = classesToExcludeInLCC,
                                  cleanScratch = cleanScratch)
    gc()
    # Organize the tiles: focalTilesToMerge is a list of tiles with 4 elements:
    # proportionOfForest, proportionOf30YPlusForest, isForest, is30YPlusForest 
    # Need to organize to merge them
    totalTiles <- unique(length(focalTilesToMerge))
    lengthVector <- 1:totalTiles
    orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
      sbset <- unlist(lapply(focalTilesToMerge, `[[`, index), 
                      use.names = FALSE)
      return(sbset)
    })
    names(orderedRasterList) <- c("proportionOfForest", 
                                  "proportionOf30YPlusForest",
                                  "isForest",
                                  "is30YPlusForest")
    if (!is.null(cleanScratch))
      unlink(cleanScratch, recursive = TRUE)
    
      if (!file.exists(proportionOfForest)){
        message(crayon::blue(paste0("Saving merged: ", proportionOfForest)))
        rasMosaicArgs <- orderedRasterList[["proportionOfForest"]]
        rasMosaicArgs$fun <- max
        mergedFocalTiles <- do.call(what = raster::mosaic, args = rasMosaicArgs)
        gc()
        # To save space, the merged proportions are multiplied by 10,000!!
        # This means the proportion has 2 digits: XX,XX%
        mergedFocalTiles <- mergedFocalTiles*10000 
        raster::writeRaster(x = mergedFocalTiles, filename = proportionOfForest,
                            format = "GTiff",
                            datatype = "INT2U")
        rm(mergedFocalTiles)
        gc()
        if (!is.null(cleanScratch))
          unlink(cleanScratch, recursive = TRUE)
      }
      if (!file.exists(proportionOf30YPlusForest)){
        message(crayon::blue(paste0("Saving merged: ", proportionOf30YPlusForest)))
        rasMosaicArgs <- orderedRasterList[["proportionOf30YPlusForest"]]
        # To save space, the merged proportions are multiplied by 10,000!!
        # This means the proportion has 2 digits: XX,XX%
        rasMosaicArgs$fun <- max
        mergedFocalTiles <- do.call(what = raster::mosaic, args = rasMosaicArgs)
        gc()
        mergedFocalTiles <- mergedFocalTiles*10000
        raster::writeRaster(x = mergedFocalTiles, filename = proportionOf30YPlusForest,
                            format = "GTiff",
                            datatype = "INT2U")
        rm(mergedFocalTiles)
        gc()
        if (!is.null(cleanScratch))
          unlink(cleanScratch, recursive = TRUE)
      }
    if (!file.exists(isForest)){
      message(crayon::blue(paste0("Saving merged: ", isForest)))
      rasMosaicArgs <- orderedRasterList[["isForest"]]
      rasMosaicArgs$fun <- max
      mergedFocalTiles <- do.call(what = raster::mosaic, args = rasMosaicArgs)
      gc()
      raster::writeRaster(x = mergedFocalTiles, filename = isForest, 
                          format = "GTiff",
                          datatype = "INT1U")
      rm(mergedFocalTiles)
      gc()
      if (!is.null(cleanScratch))
        unlink(cleanScratch, recursive = TRUE)
    }
    if (!file.exists(is30YPlusForest)){
      message(crayon::blue(paste0("Saving merged: ", is30YPlusForest)))
      rasMosaicArgs <- orderedRasterList[["is30YPlusForest"]]
      rasMosaicArgs$fun <- max
      mergedFocalTiles <- do.call(what = raster::mosaic, args = rasMosaicArgs)
      gc()
      raster::writeRaster(x = mergedFocalTiles, filename = is30YPlusForest,
                          format = "GTiff",
                          datatype = "INT1U")
      rm(mergedFocalTiles)
      gc()
      if (!is.null(cleanScratch))
        unlink(cleanScratch, recursive = TRUE)
    }
    
    return(list("proportionOfForest" = proportionOfForest,
                "proportionOf30YPlusForest" = proportionOf30YPlusForest,
                "isForest" = isForest,
                "is30YPlusForest" = is30YPlusForest))
  } # End of ifelse for checking for the full merged file
  
  })
}