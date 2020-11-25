spatial_dist <- function(dt, dts) {
  dm <- dim(dts)
  n <- dm[1]
  d <- dm[2]
  s1 <- rep(0, d)
  for (i in 1:n) {
    temp <- dt - dts[i, ]
    normtemp <- sqrt(sum(temp ^ 2))
    if (normtemp != 0.0)
      temp <- temp / normtemp
    s1 <- s1 + temp
  }
  return(sqrt(sum((s1 / n) ^ 2)))
}

rank_dist <- function(dt, dts) {
  y <- spatial_dist(dt, dt)
  x0 <- spatial_dist(dt, dts)
  return(sum(r1 < rx) / n) # r1 snd rx not defined here.
}
