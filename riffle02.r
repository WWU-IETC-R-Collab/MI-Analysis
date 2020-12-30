#  ----------------Riffle clustering----------------
#  Proportional reduction in error:
# if shared libraries are available, use this:
#fastpre <- function (x, y, numclus) {
#  temp <- 0
#  z <- .C("pre", as.double(temp),
#                 as.integer(x), as.integer(y), as.integer(length(x)),
#                 as.integer(numclus) )
#  z[[1]]
#}
#
# otherwise, use this (slower by a factor of 10 or so):
pre <- function (x, y) {
  tab <- table(x, y)
  n <- sum(tab)
  ms1 <- max(apply(tab, 1, sum))
  ms2 <- max(apply(tab, 2, sum))
  sm1 <- sum(apply(tab, 1, max))
  sm2 <- sum(apply(tab, 2, max))
  pre1 <- (sm1 - ms2)/(n - ms2)
  pre2 <- (sm2 - ms1)/(n - ms1)
  mean(c(pre1, pre2))
}

datapre <- function(splitdata, clus, numclus) {
  sapply(splitdata,  function(datum) { pre(datum, clus) })
#  sapply(splitdata,  function(datum) { fastpre(datum, clus, numclus) })
}

# Equal weight and equal size bins:
# TODO:
# if any items are NA, rank assigns them the highest ranks,
# so we need to adjust for that in the table functions
ewbin <- function(x, n) {
  binfactors <- cut(rank(x), n)
  match(binfactors, levels(binfactors))
}
esbin <- function(x, n) { 
  binfactors <- cut(x, n)
  match(binfactors, levels(binfactors))
}

randomclusters <- function(numpts, numclusters) {
  sample(1:numclusters, numpts, replace=TRUE)
}

avebestn <- function(s, n) {
  ss <- sort(s)
  len <- length(ss)
  lim <- max(1, len-n+1)
  mean(ss[lim:len])
}

reorder <- function(x) {
  match(x, order(tapply(seq(along=x), list(x), mean)))
}

riffle.values <- function(data, numclus, clusters) {
  splitdata <- lapply(data, function(datum) { ewbin(datum, numclus) } )
  datapre(splitdata, clusters, numclus)
}

riffle  <- function (data, numclus=2, numbest=min(4, ncol(data)), numreps=5) {
  best.value <- -999;
  for (i in seq(numreps)) {
    result <- riffle.once(data, numclus, numbest);
    value <- avebestn(as.numeric(result$values), numbest)
    cat(paste("Riffle run number: ",i,"/",numreps," value: ",value,"\n"));
    if (value > best.value) {
      cat("  Saving result.\n");
      best.result <- result
      best.value <- value
    }
  }
  cat(paste("Returning clustering with best value found:", best.value, "\n"));
  return(best.result)
}

riffle.once <- function (data, numclus, numbest) {
  splitdata <- lapply(data, function(datum){ewbin(datum, numclus)})
  clusters <- randomclusters(length(splitdata[[1]]), numclus)
  oldvalue <- -1
  value <- avebestn(datapre(splitdata, clusters, numclus), numbest)
  while (value > oldvalue) {
    oldvalue <- value
    for (i in seq(clusters)) {
      oldclus <- clusters[i]
      for (newclus in 1:numclus) {
        if (newclus != oldclus) {
          clusters[i] <- newclus
          newvalue <- avebestn(datapre(splitdata, clusters, numclus), numbest)
          if (newvalue > value) {
            value <- newvalue
            oldclus <- clusters[i]
          } else {
            clusters[i] <- oldclus
          }
        }
      }
    }
  }
  clusters <- reorder(clusters)
  dpre <- t(datapre(splitdata, clusters, numclus))
  parameter.scores <- data.frame(dpre)
  names(parameter.scores) <- names(data)
  answer <- list(clusters,
                 parameter.scores,
                 numclus,
                 numbest)

  names(answer) <- c("cluster", 
                      "values",
                      "number.of.clusters",
                      "number.of.column.correlations")
  answer
}






