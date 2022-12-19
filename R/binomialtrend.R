#' @name binomialtrend
#' @title Calculates The Statistical Significance Of A Teend In A Set Of Measurements
#' @description The package calculates whether there is a statistically significant trend
#' in the date provided by the user. This is based on the a signed test based on the binomial
#' distribution. The package returns a trend test value, T, and also a p-value. A T value
#' close to 1 indicates a rising trend, whereas a T value close to -1 indicates a decreasing
#' trend. A T value close to 0 indicates no trend. There is also a command to create a
#' heatmap visualizing the trend. A test data set called gtsa_data is also available, which
#' has global mean temperatures for January, April, July, and October for the years 1851 to 2022.
#'
#' Version 0.0.0.3
#' Author: Dr. Matthew Cserhati
#' Email: csmatyi@protonmail.com
#' December 16, 2022
#'
#' @importFrom grDevices colorRampPalette dev.off jpeg
#' @importFrom graphics box legend
#' @importFrom stats binom.test
#'
#' @param data a data frame with the measurement values
#' @return The p-value and trend value of the data
#'
#' @references Walpole, Myers, Myers, Ye. (2007) Probability & Statistics for Engineers and Scientists. Upper Saddle River, NJ, Pearson Prentice Hall.
#'
#' @examples
#' meas <- c(1.1,4.5,7.8,5.9,10.2)
#' binomialtrend(meas)
#' binomialtrend(c(1,2,3,4,2,4,5,6,8,5,4,7,10,11))
#'
#'@export
utils::globalVariables(c("exit"))
binomialtrend <- function(data) {
  if (!is.numeric(data)) {
    stop("Data frame contains non-numeric values!")
  }
  if (is.null(data)) {
    stop("Data frame is empty!")
  }
  n <- dim(as.matrix(data))[1]
  N <- n*(n-1)/2
  sum <- 0
  nn <- n-1
  npos <- 0
  for (i in 1:nn) {
    ii <- i+1
    for (j in ii:n) {
      if (data[j] >= data[i]) {npos <- npos + 1}
      sum <- sum + sign(data[j]-data[i])
    }
  }
  trend <- signif(sum/N,3)
  p <- signif(binom.test(npos,N,0.5)$p.value,4)

  res <- list(method = "Binomial Trend Test", data.name=paste(data,collapse=" "), p.value = p, parameter = c(T = trend))
  class(res) <- "htest"
  return(res)
}
