#' @include combinatorics.model.R
NULL

#' print
#'
#' @param x Combinatorics model
#' @param nameArrangment boolean
#' @param digits integer value
#' @param ... other arguments
#'
#' @examples
#'   out <- combinatorics.calculate(combinatorics.example.Model1())
#'   print(out)
#'   print(out, nameArrangment = TRUE)
#' @export
print.combinatorics.result <- function(x, nameArrangment= FALSE, digits = max(3L, getOption("digits") - 3L), ...){
  m <- as(x$model, "Combinatorics")
  cat("\nModel: ",m@name, "(",class(x$model)[1], ")", "\n", sep = "")
  cat("Calculate from: ", x$kind, "\n", sep = "")
  if (length(x$data)){
    cat ("Target parameter: ", x$target@name , "\n", sep="")
    lalimitname <- lapply(x$limit, function(x){x@name})
    lalimitvalue <- lapply(x$limit, function(x){x@value})
    z <- cbind (as.numeric(lalimitvalue))
    dimnames(z) <- list(as.character(lalimitname), c("value"))
    cat ("Limit parameter:\n")
    print.default(z, print.gap = 2L, quote = FALSE)
    #Состав мероприятий
    z <- x$data
    lalimitname <- paste(lapply(x$limit, function(x){x@name}), sep = ",")
    if (nameArrangment){
      laname <- lapply(m@arrangements, function (a){a@name})
      for (index in 1:nrow(z)) {
        anames <- as.character(laname[as.numeric(z$arrangements[[index]])])
        z$anames[index] <-paste(anames, collapse = ", ")
      }
    } else {
      for (index in 1:nrow(z)) {
          z$indexes[index] <-paste(as.numeric(z$arrangements[[index]]), collapse = ", ")
        }
    }
    z$arrangements <- NULL;
    rownames(z) <- as.character(c(1:nrow(x$data)))
    colnames(z) <- c(x$target@name, lalimitname, "T/L", "Arrangements")
    cat("Optimal combination of arrangements!\n")
    print(z, print.gap = 2L)
  } else {
    cat("No optimal combination of arrangements!")
  }
  cat("\n")
  invisible(x)
}
