# Модель Сombinatorics
#'Combinatorics Model
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{parameters}:}{Object of class \code{"list"}, containing data from of resources and targets}
#'    \item{\code{arrangements}:}{Object of class \code{"list"}, containing data from of arrangements.}
#'    \item{\code{logicconnections}:}{Object of class \code{"list"}, containing data logic network.}
#'    \item{\code{name}:}{Object of class \code{"character"}, containing data that needs to go in name.}
#'  }
#'
#' @name Combinatorics-class
#' @rdname Combinatorics-class
#'
#' @exportClass Combinatorics
setClass("Combinatorics",

  representation(
    #Список параметров/ресурсов. Элементы combinatorics.parameter
    parameters = "list",
    #Список мероприятий. Элементы combinatorics.arrangement
    arrangements= "list",
    #Список логических связей. Элементы combinatorics.logicconnection
    logicconnections= "list",
    #Наименование модели
    name = "character"
  ),

  prototype (

  ),

  validity=function(object)
  {
    return(TRUE)
  }
)

# @name initializeCombinatorics
# @rdname Combinatorics-class
#
# @param .Object Combinatorics model
# @param ... constructor arguments
# @param data matrix
# @param arrayLogicalConnections character vector
#
# @return Combinatorics model
setMethod("initialize", signature(.Object = "Combinatorics"),
          function(.Object, ... , data=matrix(), arrayLogicalConnections=character()){
            .Object <- callNextMethod(.Object, ...)
            if (!(sum(is.na(data))==length(data))){
              #Если не заданы мероприятия и ресурсы
              if((length(.Object@arrangements)+length(.Object@parameters))>0){
                warning("Not convert values to arrangements and parameters. Length arrangements and parameters not zero.")
              } else {
                if (is.null(colnames(data))){
                  stop("Parameters name not set!")
                }
                for(col in 1:length(colnames(data))){
                  .Object <- combinatorics.add.parameter(.Object, colnames(data)[col])
                }
                if (is.null(rownames(data))){
                  stop("Arrangements name not set!")
                }
                for(row in 1:nrow(data)){
                  .Object <- combinatorics.add.arrangement(.Object, rownames(data)[row], data[row, ])
                }
              }
            }
            if (length(arrayLogicalConnections)){
              if(length(.Object@logicconnections)){
                warning("Not convert arrayLogicalConnections to logicconnections. Length logicconnections not zero.")
              } else {
                for(index in 1:length(arrayLogicalConnections)){
                  .Object <- combinatorics.add.logicconnection(.Object, arrayLogicalConnections[index])
                }
              }
            }
            .Object
          })

# Используйте combinatorics.model для создания нового объекта Combinatorics
#' Use combinatorics.model for create new Combinatoric sobject
#' @name Combinatorics
#' @rdname Combinatorics-class
#' @param ... constructor arguments
#' @examples
#'   model <- combinatorics.model(name="Example")
#' @export
combinatorics.model <- function(...) new ("Combinatorics", ...)


#Возваращает информацию о модели
setMethod("show", "Combinatorics", function(object) {
  cat("\nCombinatorics model: ",object@name, "(",class(object)[1], ")", "\n", sep = "")
  if (length(object@arrangements)){
    input <- matrix (do.call("c", lapply(object@arrangements, function (c){c@value})), nrow = length(object@arrangements), byrow = TRUE)
    aNames <- c(do.call("c",lapply(object@arrangements, function (c){c@name})))
    pNames <- c(
      do.call("c",lapply(object@parameters, function (c){c(
        paste(c@name, c@dimension ,sep=", "))}))
    )
    dimnames(input) <- list(aNames, pNames)
    cat("INITIAL DATA\n")
    print.default(input, print.gap = 2L, quote = FALSE)
  } else{
    cat("No arrangements\n")
  }
  limit <- list()
  other <- list()
  target <- FALSE
  if (length(object@parameters)){
    for (index in 1:length(object@parameters)) {
      parameter <- object@parameters[[index]]
      if (parameter@isTarget){
        target <- TRUE
        cat ("\nTarget parameter: ", parameter@name , "\n", sep="")
      }
      if (parameter@isLimited){
        limit <- append(limit, list(parameter))
      } else {
        if (!parameter@isTarget)
          other <- c(other, parameter)
      }
    }
    if(length(limit)){
      cat ("Limit parameter:\n")
      limitValues <- cbind(do.call("c",lapply(limit, function (c){c@value})))
      lNames <- do.call("c", lapply(limit, function (c){
        paste(c@name, c@dimension, sep=", ")}))
      dimnames(limitValues) <- list(lNames, c("value"))
      print.default(limitValues, print.gap = 2L, quote = FALSE)
    }else {
      cat("No limit parameters\n")
    }
  }
  if(length(other)){
    cat ("Other resources:\n")
    lNames <- do.call("c", lapply(other, function (c){
      paste(c@name, c@dimension, sep=", ")}))
    print.default(as.character(lNames), print.gap = 2L, quote = FALSE)
  }

  if (length(object@logicconnections)){
    lNames <- do.call("c", lapply(object@logicconnections, function (c){c@value}))
    cat("\nLogical connections:\n\t")
    cat(paste(lNames, collapse = "\n\t"))
  } else {
    cat("\nThere are no logical connections\n")

  }
})

# Вспомогательные классы. Пользователи используют для манипулацией моделью
#
# combinatorics.add.arrangement
# combinatorics.add.logicconnection
# combinatorics.add.parameter
# combinatorics.remove.arrangement
# combinatorics.remove.logicconnection
# combinatorics.remove.parameter
# combinatorics.set.arrangement
# combinatorics.set.limit
# combinatorics.set.name
# combinatorics.set.target

combinatorics.parameter <- setClass(

  "CombinatoricsParameter",

  representation(
    name  = "character",
    dimension = "character",
    #Является ли целевым параметром TRUE - да
    isTarget = "logical",
    #Является ли ограниченным ресурсом
    isLimited = "logical",
    #Значение ограниченного ресурса
    value = "numeric"
  ),

  prototype = list(
    name = "Parameter",
    dimension = "",
    isTarget = FALSE,
    isLimited = FALSE,
    value = 0
  )
)

combinatorics.arrangement <- setClass(
  "CombinatoricsArrangement",

  representation(
    #Название мероприятий
    name  = "character",
    #Значения притоков и отоков
    value = "vector"
  ),

  prototype = list(
    value = numeric(),
    name = "Arrangement"
  )
)

#Логические связи
combinatorics.logicconnection <- setClass(
  "LogicConnection",

  representation(
    value = "vector"
  ),

  prototype = list(
    value = "character"
  )

)
