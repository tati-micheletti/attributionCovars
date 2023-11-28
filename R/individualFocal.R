individualFocal <- function(ras, 
                            weightMatrix, 
                            useInteger = FALSE,
                            maskTo = NULL, # length 2+ vector of c(maskWhat, ..., ToWhat)
                            ...) { 
  # denominatorRaster == raster to devide the focal by
  ras[] <- ras[]
  focalRas <- raster::focal(x = ras,
                            w = weightMatrix,
                            na.rm = TRUE,
                            fun = sum,
                             pad = TRUE)
  if (!is.null(maskTo)){
    maskWhat <- maskTo[-length(maskTo)]
    ToWhat <- maskTo[length(maskTo)]
    if (any(is.na(maskWhat))){
      focalRas[is.na(ras[])] <- ToWhat
      if (!all(is.na(maskWhat))){
        focalRas[ras[] %in% !is.na(maskWhat)] <- ToWhat
      }
    } else {
      focalRas[ras[] %in% maskWhat] <- ToWhat
    }
  }
  
  if (useInteger){
    storage.mode(focalRas[]) <- "integer" # Reducing size of raster by converting it to a real binary
    gc()
  }
  
  dots <- list(...)
  if (!is.null(dots[["denominatorRaster"]])){
    focalRas <- focalRas/dots[["denominatorRaster"]]
  }
  
  return(focalRas)
}
