focalWindowAnnulus <- function(focalDistance, ras){
  inMat <- focalWindow(x = ras, d = min(focalDistance))
  outMat <- focalWindow(x = ras, d = max(focalDistance))
  #inverse the inner matrix
  inMat[inMat == 0] <- -1
  inMat[inMat == 1] <- 0
  inMat[inMat == -1] <- 1
  innerDim <- floor(dim(inMat)[1] / 2)
  outerDim <- ceiling(dim(outMat)[1] / 2)
  # Merge the two matrices
  outMat[(outerDim - innerDim):(outerDim + innerDim),
         (outerDim - innerDim):(outerDim + innerDim)] <- inMat
  
  # 23SEP20 TM ~ This below is not making
  # sense anymore as we need the matrix to be 1's and 0's
  # 
  # #Recalculate the matrix value as 1/sum of non-zero values
  # outMat[outMat > 0] <- (1 / length(outMat[outMat > 0]))
  # focalMatrices <- outMat
  return(outMat)
}
