#' Generic X-Y Plotting the result of calculating the combinatorics model
#'
#' @description
#' Отображение графика зависимости целевого показателя или отношения "целевого показателя к
#' ограниченному ресурсу" от ограниченного ресурса.
#'
#' @param x объект, содержаший результаты расчёта комбинаторной модели на максимум
#'  или минимум
#' @param TargetDivLimit объект класса \code{"boolean"}, содержащий флаг использования
#' в значении оси y отношение целевого показателя к ограниченному ресурсу (ЦП/ОР)
#' @param ... дополнительные аргументы, передаваемые другим методам или получаемые от
#' них, см. аргумент ... функции \code{\link{plot}}
#'
#' @examples
#'   out <- combinatorics.calculate(combinatorics.example.Model1())
#'   plot(out, type="o")
#'   plot(out, type="o", TargetDivLimit=TRUE)
#'
#' @seealso \code{\link{combinatorics.calculate}}
#'
#' @export
plot.combinatorics.result <- function(x, TargetDivLimit=FALSE, ...){
  limit <- 1
  data <- x$data
  if (is.null(data)){
    stop("data not include result calculate")
  }
  if (nrow(data)<2){
    stop("A small amount of data as a result of the combinatorics.calculate")
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
