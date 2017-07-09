#' @include combinatorics.model.R
NULL

##Модификация модели. Добавления нового мероприятия
#
#' Method add arrangement
#' @description
#'   Добавляет мероприятие в комбинаторную модель.
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param name объект класса \code{"character"}, содержащий наименование добавляемого
#'   мероприятия
#' @param values объект класса \code{"vector"}, содержащий содержащий числовые значения показателей. Количество
#'   количество значений в векторе должно быть равно колчеству показателей в комбинаторной модели
#'
#' @return возвращает объект класса \code{"Combinatorics"} с внесёнными изменениями
#'
#' @name combinatorics.add.arrangement
#' @rdname combinatorics.add.arrangement
#' @exportMethod combinatorics.add.arrangement
setGeneric(name="combinatorics.add.arrangement",
           def=function(model, name, values){
             standardGeneric("combinatorics.add.arrangement")
           }
)

#' @rdname combinatorics.add.arrangement
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.add.arrangement(model, 'M25', c(10.1, 20.2, 30.3, 40.4, 50.5, 60.6))
setMethod(f="combinatorics.add.arrangement",
          signature=c("Combinatorics","character", "vector"),
          definition=function(model, name, values)
          {
            if (length(values)!= length(model@parameters)){
              stop(sprintf("Size vector values not equals parameters count. Size parameters is %g", length(model@parameters)))
            }
            arrangement <- combinatorics.arrangement(name=name, value=as.numeric(values))
            model@arrangements <- c(model@arrangements, arrangement)
            return(model)
          }
)


#
#
#
#
#
##Модификация модели. Добавления новой логической связи
#' Method add logicconnection in model.
#' @description
#'   Добавляет логическую связь в логическую сеть комбинаторной модели.
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param logicconnection объект класса \code{"character"}, содержащий логическую формулу,
#'   которой представленна логическая связь
#'
#' @return возвращает объект класса \code{"Combinatorics"} с внесёнными изменениями
#'
#' @name combinatorics.add.logicconnection
#' @rdname combinatorics.add.logicconnection
#' @exportMethod combinatorics.add.logicconnection
setGeneric(name="combinatorics.add.logicconnection",
           def=function(model, logicconnection){
             standardGeneric("combinatorics.add.logicconnection")
           }
)

#' @rdname combinatorics.add.logicconnection
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.add.logicconnection(model, "(1)->(7,18,22,24)")
setMethod(f="combinatorics.add.logicconnection",
          signature=c("Combinatorics","character"),
          definition=function(model, logicconnection)
          {
            lc <- combinatorics.logicconnection(value=logicconnection)
            model@logicconnections <- c(model@logicconnections, lc)
            return(model)
          }
)

#
#
#
#
#
##Модификация модели. Добавления параметров/ресурсов
#' @name combinatorics.add.parameter
#' @title Add parameter in model
#' @description
#'   Добавляет показатель в комбинаторную модель. При указании значения в парамете \code{"value"}
#'   показатель используется  в качестве как ограниченного ресурса
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param name объект класса \code{"character"}, содержащий наименование добавляемого
#'   показателя
#' @param dimension объект класса \code{"character"}, содержащий размернойть добавляемого
#'   показателя
#' @param value объект класса \code{"numeric"}, содержащий значение огранченного ресурса.
#' @param arrangementValues объект класса \code{"vector"}, содержащий числовые значения показателя для.
#'  всех мероприятий в комбинаторной модели. Количество значений в векторе должно быть равно количеству
#'  мероприятий в комбинаторной модели
#'
#' @return возвращает объект класса \code{"Combinatorics"} с внесёнными изменениями
#'
#' @exportMethod combinatorics.add.parameter
setGeneric(name="combinatorics.add.parameter",
           def=function(model, name, dimension, value, arrangementValues){
             standardGeneric("combinatorics.add.parameter")
           }
)

# Так как задан лимит, то очевидно ограниченный ресурс
#' @describeIn combinatorics.add.parameter добавление ограниченного ресурса в комбинаторную модель
#'   не содержащей мероприятий
#' @examples
#'
#'   model <- combinatorics.model()
#'   combinatorics.add.parameter (model, "R1", "pc", 10)
#' @export
setMethod(f="combinatorics.add.parameter",
          signature=c("Combinatorics","character", "character","numeric", "missing"),
          definition=function(model, name, dimension, value, arrangementValues)
          {
            if (length(model@arrangements)){
              stop("If there is an event, you must specify the parameter / resource values for each event")
            }
            if (value<0){
              stop("Limit value less 0!")
            }
            limit <- combinatorics.parameter(name=name, dimension=dimension, isTarget=FALSE, isLimited=TRUE, value=value)
            model@parameters <- c(model@parameters, limit)
            return(model)
          }
)

# Так как задан лимит, то очевидно ограниченный ресурс
#' @describeIn combinatorics.add.parameter добавление ограниченного ресурса без указания его размерности
#'   в комбинаторную модель не содержащей мероприятий
#' @examples
#'
#'   model <- combinatorics.model()
#'   combinatorics.add.parameter (model, "R1", value=10)
#' @export
setMethod(f="combinatorics.add.parameter",
          signature=c("Combinatorics","character", "missing","numeric", "missing"),
          definition=function(model, name, dimension, value, arrangementValues)
          {
            if (length(model@arrangements)){
              stop("If there is an event, you must specify the parameter / resource values for each event")
            }
            if (value<0){
              stop("Limit value less 0!")
            }
            limit <- combinatorics.parameter(name=name, isTarget=FALSE, isLimited=TRUE, value=value)
            model@parameters <- c(model@parameters, limit)
            return(model)
          }
)

#' @describeIn combinatorics.add.parameter добавление показателя в комбинаторную модель не
#'  содержащей мероприятий
#' @examples
#'
#'   model <- combinatorics.model()
#'   combinatorics.add.parameter (model, "R1", "pc.")
#' @export
setMethod(f="combinatorics.add.parameter",
          signature=c("Combinatorics","character", "character","missing", "missing"),
          definition=function(model, name, dimension, value, arrangementValues)
          {
            if (length(model@arrangements)){
              stop("If there is an event, you must specify the parameter / resource values for each event")
            }
            parameter <- combinatorics.parameter(name=name, dimension=dimension)
            model@parameters <- c(model@parameters, parameter)
            return(model)
          }
)

#' @describeIn combinatorics.add.parameter добавление показателя без указания его размерности
#'   в комбинаторную модель не содержащей мероприятий
#' @examples
#'
#'   model <- combinatorics.model()
#'   combinatorics.add.parameter (model, "R1")
#' @export
setMethod(f="combinatorics.add.parameter",
          signature=c("Combinatorics","character", "missing","missing", "missing"),
          definition=function(model, name, dimension, value, arrangementValues)
          {
            if (length(model@arrangements)){
              stop("If there is an event, you must specify the parameter / resource values for each event")
            }
            parameter <- combinatorics.parameter(name=name)
            model@parameters <- c(model@parameters, parameter)
            return(model)
          }
)

# Так как задан лимит, то очевидно ограниченный ресурс
#' @describeIn combinatorics.add.parameter добавление ограниченного ресурса в комбинаторную модель
#' @examples
#'
#'   model <- combinatorics.example.Model2()
#'   combinatorics.add.parameter (model, "Electricity", "Thousand kWh/hour", 75,
#'      c(39, 24.3, 17.7, 26.7, 11.1, 10.8, 18.3, 12.3))
#' @export
setMethod(f="combinatorics.add.parameter",
          signature=c("Combinatorics","character", "character","numeric","vector"),
          definition=function(model, name, dimension, value, arrangementValues)
          {
            if (value<0){
              stop("Limit value less 0!")
            }
            count <- length(model@arrangements)
            if (count!= length(arrangementValues)){
              stop("Count arrangementValues not equal arrangements!")

            }
            limit <- combinatorics.parameter(name=name, dimension=dimension, isTarget=FALSE, isLimited=TRUE, value=value)
            model@parameters <- c(model@parameters, limit)
            if (count>0)
              for (index in 1:count) {
                arrangement<- model@arrangements[[index]]
                model@arrangements[[index]]@value <- c(arrangement@value, as.numeric(arrangementValues[index]))
              }
            return(model)
          }
)

# Так как задан лимит, то очевидно ограниченный ресурс
#' @describeIn combinatorics.add.parameter добавление показателя в комбинаторную модель
#' @examples
#'
#'   model <- combinatorics.example.Model2()
#'   combinatorics.add.parameter (model, "Electricity", "Thousand kWh/hour",
#'      arrangementValues=c(39, 24.3, 17.7, 26.7, 11.1, 10.8, 18.3, 12.3))
#' @export
setMethod(f="combinatorics.add.parameter",
          signature=c("Combinatorics","character", "character","missing","vector"),
          definition=function(model, name, dimension, value, arrangementValues)
          {
            count <- length(model@arrangements)
            if (count!= length(arrangementValues)){
              stop("Count arrangementValues not equal arrangements!")
            }
            parameter <- combinatorics.parameter(name=name, dimension=dimension)
            model@parameters <- c(model@parameters, parameter)
            if (count>0)
              for (index in 1:count) {
                arrangement<- model@arrangements[[index]]
                model@arrangements[[index]]@value <- c(arrangement@value, as.numeric(arrangementValues[index]))
              }
            return(model)
          }
)


# Так как задан лимит, то очевидно ограниченный ресурс
#' @describeIn combinatorics.add.parameter добавление ограниченного ресурса без указания его размерности
#'   в комбинаторную модель
#' @examples
#'
#'   model <- combinatorics.example.Model2()
#'   combinatorics.add.parameter (model, "Electricity", value=75,
#'      arrangementValues=c(39, 24.3, 17.7, 26.7, 11.1, 10.8, 18.3, 12.3))
#' @export
setMethod(f="combinatorics.add.parameter",
          signature=c("Combinatorics","character", "missing","numeric","vector"),
          definition=function(model, name, dimension, value, arrangementValues)
          {
            count <- length(model@arrangements)
            if (count!= length(arrangementValues)){
              stop("Count arrangementValues not equal arrangements!")
            }
            if (value<0){
              stop("Limit value less 0!")
            }
            limit <- combinatorics.parameter(name=name, dimension="", isTarget=FALSE, isLimited=TRUE, value=value)
            model@parameters <- c(model@parameters, limit)
            if (count>0)
              for (index in 1:count) {
                arrangement<- model@arrangements[[index]]
                model@arrangements[[index]]@value <- c(arrangement@value, as.numeric(arrangementValues[index]))
              }
            return(model)
          }
)

# Добавить новый ограниченный ресурс
combinatorics.add.limit <- function(model, name, value, dimension=NULL, arrangementValues){
  if (!isClass(model)){
    stop("Model not CombinatoricsModel")
  }
  if (value<0){
    stop("Limit value less 0!")
  }
  m <- as(model, "Combinatorics")
  count <- length(m@arrangements)
  if (count!= length(arrangementValues)){
    stop("Count arrangementValues not equal arrangements!")

  }
  np <- combinatorics.parameter(name=name, dimension=dimension, isTarget=FALSE, isLimited=TRUE, value=value)
  m@parameters <- append(m@parameters, np)
  for (index in 1:count) {
    arrangement<- m@arrangements[[index]]
    m@arrangements[[index]]@value <- c(arrangement@value, as.numeric(arrangementValues[index]))
  }
  return(m)
}

