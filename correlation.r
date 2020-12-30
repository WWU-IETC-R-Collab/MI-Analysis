correlation.matrix <- function(f, ...) {
  n <- ncol(f)
  probs <- matrix(NA, n, n)
  nums <- matrix(NA, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (j > i) {
        val <- cor.test(f[,i], f[,j], ...)
        probs[i,j] <- signif(val$p.value, 3)
        nums[i,j] <- signif(val$estimate, 3)
      } else if (i == j) {
        probs[i,j] <- 0.0;
        nums[i,j] <- Inf;
      } else {
        probs[i,j] <- probs[j,i]
        nums[i,j] <- nums[j,i]
      }
    }
  }
  attr(nums, "dimnames") <- list(names(f), names(f))
  attr(probs, "dimnames") <- list(names(f), names(f))
  ans <- list(nums, probs)
  names(ans) <- c("statistics", "p.values")
  ans
}

##### Source correlation.r, then type the following
##### my.data.cor <- correlation.matrix(my.data)
##### my.data.cor$statistics ##### produces correlation matrix
##### my.data.cor$p.values ####### produces significance matrix
