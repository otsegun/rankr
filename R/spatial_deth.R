spatial_depth <- function(dt, dts) {
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
  return(1 - sqrt(sum((s1 / n) ^ 2)))
}
