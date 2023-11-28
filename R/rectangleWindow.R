rectangleWindow <- function (rs, d){
  d <- rep(d, length.out = 2)
  nx <- 1 + 2 * floor(d[1]/rs[1])
  ny <- 1 + 2 * floor(d[2]/rs[2])
  m <- matrix(1, ncol = nx, nrow = ny)
  return(m)
}