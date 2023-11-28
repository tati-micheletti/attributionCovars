checkForFinalObject <- function(years, 
                                focalDistances, 
                                resampledRes,
                                pathData){
  # Need to loop through: year, FDISTANCE
  # resampledRes: make sure this is an argument that can be passed from outside. 
  # The resampledRes for this project is: 30 (the resolution of the disturbance layer)
  
  folderExists <- tryCatch({
    invisible(checkPath(file.path(pathData, "merged")))
    TRUE
  }, error = function(e){
    return(FALSE)
  })
  
  if (!folderExists){
    cat(crayon::red("The folder where the results should be doesn't exist. The module will run..."))
    return(NULL)
  }

  allFocalDistanceAllYears <- lapply(years, function(Y){ 

    allFocalDistance <- lapply(focalDistances, function(FDISTANCE){
    browser() # Double check the returns! Might be doing it wrong with the distance!

      proportionOfForest <- file.path(pathData, "merged", paste0("merged_proportionOfForest_Res",
                                                               resampledRes, 
                                                               "mFocal", max(FDISTANCE), 
                                                               "m.tif"))
      proportionOf30YPlusForest <- file.path(pathData, "merged", paste0("merged_proportionOf30YPlusForest_Res", 
                                                                      resampledRes, "mFocal", max(FDISTANCE), 
                                                                      "mYear", Y, ".tif"))
      isForest <- file.path(pathData, "merged", paste0("merged_isForest_Res", 
                                                     resampledRes, "m.tif"))
      is30YPlusForest <- file.path(pathData, "merged", paste0("merged_is30YPlusForest_Res", 
                                                            resampledRes, "mYear", Y, ".tif"))

    if (all(file.exists(proportionOf30YPlusForest),
            file.exists(proportionOfForest),
            file.exists(isForest),
            file.exists(is30YPlusForest))) {
      
      message(crayon::green(paste0("Year ",
                                   Y, " has already been processed for ", max(FDISTANCE),"m focal distance and is being loaded.",
                                   " Skipping to next year...")))
      
      return(list("proportionOfForest" = proportionOfForest,
                  "proportionOf30YPlusForest" = proportionOf30YPlusForest,
                  "isForest" = isForest,
                  "is30YPlusForest" = is30YPlusForest))
      
    } else {
      return(NULL)
    }
  })
    browser()
    names(allFocalDistance) <- paste0("Year", Y)
    
    return(allFocalDistance)
    })
  
  browser()
}