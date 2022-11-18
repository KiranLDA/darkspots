function (x, q = 0, datatype = "abundance", size = NULL,
          endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 50)
{
  TYPE <- c("abundance", "incidence", "incidence_freq",
            "incidence_raw")
  if (is.na(pmatch(datatype, TYPE)))
    stop("invalid datatype")
  if (pmatch(datatype, TYPE) == -1)
    stop("ambiguous datatype")
  datatype <- match.arg(datatype, TYPE)
  class_x <- class(x)[1]
  if (datatype == "incidence") {
    stop("datatype=\"incidence\" was no longer supported after v2.0.8, \n         please try datatype=\"incidence_freq\".")
  }
  if (datatype == "incidence_freq")
    datatype <- "incidence"
  if (datatype == "incidence_freq")
    datatype <- "incidence"
  if (datatype == "incidence_raw") {
    if (class_x == "list") {
      x <- lapply(x, as.incfreq)
    }
    else {
      x <- as.incfreq(x)
    }
    datatype <- "incidence"
  }
  Fun <- function(x, q) {
    x <- as.numeric(unlist(x))
    if (datatype == "abundance") {
      if (sum(x) == 0)
        stop("Zero abundance counts in one or more sample sites")
      out <- iNEXT.Ind(Spec = x, q = q, m = size, endpoint = ifelse(is.null(endpoint),
                                                                    2 * sum(x), endpoint), knots = knots, se = se,
                       nboot = nboot, conf = conf)
    }
    if (datatype == "incidence") {
      t <- x[1]
      y <- x[-1]
      if (t > sum(y)) {
        warning("Insufficient data to provide reliable estimators and associated s.e.")
      }
      if (sum(x) == 0)
        stop("Zero incidence frequencies in one or more sample sites")
      out <- iNEXT.Sam(Spec = x, q = q, t = size, endpoint = ifelse(is.null(endpoint),
                                                                    2 * max(x), endpoint), knots = knots, se = se,
                       nboot = nboot, conf = conf)
    }
    out
  }
  if (class(q) != "numeric")
    stop("invlid class of order q, q should be a postive value/vector of numeric object")
  if (min(q) < 0) {
    warning("ambigous of order q, we only compute postive q")
    q <- q[q >= 0]
  }
  if (class_x == "numeric" | class_x == "integer" |
      class_x == "double") {
    out <- do.call("rbind", lapply(q, function(q) Fun(x,
                                                      q)))
    out[, -(1:3)] <- round(out[, -(1:3)], 3)
    index <- rbind(as.matrix(ChaoSpecies(x, datatype, conf)),
                   as.matrix(ChaoEntropy(x, datatype, transform = TRUE,
                                         conf)), as.matrix(EstSimpson(x, datatype, transform = TRUE,
                                                                      conf)))
    rownames(index) <- c("Species Richness", "Shannon diversity",
                         "Simpson diversity")
  }
  else if (class_x == "matrix" | class_x == "data.frame") {
    out <- apply(as.matrix(x), 2, function(x) {
      tmp <- do.call("rbind", lapply(q, function(q) Fun(x,
                                                        q)))
      tmp[, -(1:3)] <- round(tmp[, -(1:3)], 3)
      tmp
    })
    arr <- array(0, dim = c(3, 5, ncol(x)))
    arr[1, , ] <- t(as.matrix(ChaoSpecies(x, datatype, conf)))
    arr[2, , ] <- t(as.matrix(ChaoEntropy(x, datatype, transform = TRUE,
                                          conf)))
    arr[3, , ] <- t(as.matrix(EstSimpson(x, datatype, transform = TRUE,
                                         conf)))
    dimnames(arr)[[3]] <- names(x)
    dimnames(arr)[[1]] <- c("Species richness", "Shannon diversity",
                            "Simpson diversity")
    dimnames(arr)[[2]] <- c("Observed", "Estimator",
                            "Est_s.e.", "Lower_CI", "Upper_CI")
    index <- ftable(arr, row.vars = c(3, 1))
    index <- dcast(as.data.frame(index), formula = Var1 +
                     Var2 ~ Var3, value.var = "Freq")
    colnames(index) <- c("Site", "Diversity",
                         "Observed", "Estimator", "s.e.",
                         "LCL", "UCL")
  }
  else if (class_x == "list") {
    out <- lapply(x, function(x) {
      tmp <- do.call("rbind", lapply(q, function(q) Fun(x,
                                                        q)))
      tmp[, -(1:3)] <- round(tmp[, -(1:3)], 3)
      tmp
    })
    arr <- array(0, dim = c(3, 5, length(x)))
    arr[1, , ] <- t(as.matrix(ChaoSpecies(x, datatype, conf)))
    arr[2, , ] <- t(as.matrix(ChaoEntropy(x, datatype, transform = TRUE,
                                          conf)))
    arr[3, , ] <- t(as.matrix(EstSimpson(x, datatype, transform = TRUE,
                                         conf)))
    dimnames(arr)[[3]] <- names(x)
    dimnames(arr)[[1]] <- c("Species richness", "Shannon diversity",
                            "Simpson diversity")
    dimnames(arr)[[2]] <- c("Observed", "Estimator",
                            "Est_s.e.", "Lower_CI", "Upper_CI")
    index <- ftable(arr, row.vars = c(3, 1))
    index <- dcast(as.data.frame(index), formula = Var1 +
                     Var2 ~ Var3, value.var = "Freq")
    colnames(index) <- c("Site", "Diversity",
                         "Observed", "Estimator", "s.e.",
                         "LCL", "UCL")
  }
  else {
    stop("invalid class of x, x should be a object of numeric, matrix, data.frame, or list")
  }
  info <- DataInfo(x, datatype)
  z <- list(DataInfo = info, iNextEst = out, AsyEst = index)
  class(z) <- c("iNEXT")
  return(z)
}
<bytecode: 0x0000022ba8e7e118>
  <environment: namespace:iNEXT>
