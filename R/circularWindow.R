circularWindow <- function (rs, d){
  nx <- 1 + 2 * floor(d/rs[1])
  ny <- 1 + 2 * floor(d/rs[2])
  m <- matrix(ncol = nx, nrow = ny)
  m[ceiling(ny/2), ceiling(nx/2)] <- 1
  if (nx == 1 & ny == 1) {
    return(m)
  }
  else {
    x <- raster(m, xmn = 0, xmx = nx * rs[1], ymn = 0, ymx = ny * 
                  rs[2], crs = "+proj=utm +zone=1 +datum=WGS84")
    d <- matrix(raster::distance(x), ncol = raster::ncol(x), nrow = raster::nrow(x)) <= d
    m <- matrix(as.numeric(d), ncol = base::ncol(d), nrow = base::nrow(d))
    return(m)
  }
}