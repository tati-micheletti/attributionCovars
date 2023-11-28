focalWindow <- function (x, d, type = "circle"){ # or "rectangle"
  type <- match.arg(type)
  x <- res(x)
  if (type == "circle") {
    circularWindow(x, d)
  } else {
    rectangleWindow(x, d)
  }
}
