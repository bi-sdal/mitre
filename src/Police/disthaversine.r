# local function of disthaversine
disthaversine <- function (p1, p2, r = 6378137) 
{
  toRad <- pi/180
  p1 <- .pointsToMatrix(p1) * toRad
  if (missing(p2)) {
    p2 <- p1[-1, ]
    p1 <- p1[-nrow(p1), ]
  }
  else {
    p2 <- .pointsToMatrix(p2) * toRad
  }
  p = cbind(p1[, 1], p1[, 2], p2[, 1], p2[, 2], as.vector(r))
  dLat <- p[, 4] - p[, 2]
  dLon <- p[, 3] - p[, 1]
  a <- sin(dLat/2) * sin(dLat/2) + cos(p[, 2]) * cos(p[, 4]) * 
    sin(dLon/2) * sin(dLon/2)
  a <- pmin(a, 1)
  dist <- 2 * atan2(sqrt(a), sqrt(1 - a)) * p[, 5]
  return(as.vector(dist))
}