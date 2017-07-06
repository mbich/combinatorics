#' plot
#'
#' @param x Combinatorics model
#' @param limit index
#' @param TargetDivLimit y axis
#' @param ... other arguments
#'
#' @examples
#'   out <- combinatorics.calculate(combinatorics.example.Model1())
#'   plot(out, type="o")
#'   plot(out, type="o", TargetDivLimit=TRUE)
#'
#' @export
plot.combinatorics.result <- function(x, limit=1, TargetDivLimit=FALSE, ...){
  data <- x$data
  if (is.null(data)){
    stop("data not include result calculate")
  }
  xData <- lapply(data$limit, function(l){l[[limit]]})
  xlab <-x$limit[[limit]]@name
  if (TargetDivLimit){
    yData <- lapply(data$tl, function(tl){tl[[limit]]})
    ylab <- c("T/L")
  } else {
    ylab <- x$target@name
    yData <- data$target;
  }

  plot(x=xData, y=yData,  ylab = ylab, xlab = xlab, ...)
}
