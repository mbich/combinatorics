#' @include combinatorics.model.R
NULL

##Модификация модели удаление мероприятия
#' @name combinatorics.remove.arrangement
#' @title Remove arrangement
#' @description
#'   Исключает мероприятие из комбинаторной модели
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param arrangement объект класса \code{"character"} или \code{"numeric"}, содержащий
#'   наименование мероприятия или индекс мероприятия в списке мероприятий
#'
#' @return возвращает объект класса \code{"Combinatorics"} с внесёнными изменениями
#'
#' @seealso \code{\link{combinatorics.remove.logicconnection}},
#' \code{\link{combinatorics.remove.parameter}}
setGeneric(name="combinatorics.remove.arrangement",
           def=function(model, arrangement){
             standardGeneric("combinatorics.remove.arrangement")
           }
)

#' @describeIn combinatorics.remove.arrangement поиск мероприятия по наименованию и исключение его
#'
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.remove.arrangement(model, 'M1')
#' @export
setMethod(f="combinatorics.remove.arrangement",
          signature=c("Combinatorics","character"),
          definition=function(model, arrangement)
          {
            if (length(model@arrangements)){
              for (index in 1:length(model@arrangements)) {
                if (model@arrangements[[index]]@name==arrangement){
                  model@arrangements[[index]] <- NULL
                  return(model)
                }
              }
            }
            return(model)
          }
)

#' @describeIn combinatorics.remove.arrangement поиск мероприятия по индексe в списке мероприятий
#'   и исключение его
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.remove.arrangement(model, 1)
#' @export
setMethod(f="combinatorics.remove.arrangement",
          signature=c("Combinatorics","numeric"),
          definition=function(model, arrangement)
          {
            if (length(model@arrangements)){
              if (is.null(model@arrangements[arrangement])){
                stop("Index arrangement not found")
              } else{
                model@arrangements[[arrangement]] <- NULL
              }
            }
            return(model)
          }
)
##Модификация модели. Удаление новой логической связи
#' @name combinatorics.remove.logicconnection
#' @title Remove logicconnection
#' @description
#'   Исключает логическую связь из логической сети комбинаторной модели
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param logicconnection объект класса \code{"character"} или \code{"numeric"}, содержащий
#'   логическую формулу или формулы в логической сети
#'
#' @return возвращает объект класса \code{"Combinatorics"} с внесёнными изменениями
setGeneric(name="combinatorics.remove.logicconnection",
           def=function(model, logicconnection){
             standardGeneric("combinatorics.remove.logicconnection")
           }
)

#' @describeIn combinatorics.remove.logicconnection исключает логическую связь по её логической
#'   формуле
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.remove.logicconnection(model, "[1,4,5]")
#' @export
setMethod(f="combinatorics.remove.logicconnection",
          signature=c("Combinatorics","character"),
          definition=function(model, logicconnection)
          {
            if (length(model@logicconnections)){
              for (index in 1:length(model@logicconnections)) {
                if (model@logicconnections[[index]]@value==logicconnection){
                  model@logicconnections[[index]] <- NULL
                  return(model)
                }
              }
            }
            return(model)
          }
)

#' @describeIn combinatorics.remove.logicconnection исключает логическую связь по индексу её логической
#'   формулы в логической сети (в списке логических связей)
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.remove.logicconnection(model, 4)
#' @export
setMethod(f="combinatorics.remove.logicconnection",
          signature=c("Combinatorics","numeric"),
          definition=function(model, logicconnection)
          {
            if (length(model@logicconnections)){
              if (is.null(model@logicconnections[logicconnection])){
                stop("Index arrangement not found")
              } else{
                model@logicconnections[[logicconnection]] <- NULL
              }
            }
            return(model)
          }
)

##Модификация модели. Удаление параметра
#' @name combinatorics.remove.parameter
#' @title Remove parameter
#' @description
#'   Исключает показатель в мероприятиях из комбинаторной модели. При исключении показатель
#'   исключается из каждого мероприятия.
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param parameter объект класса \code{"character"} или \code{"numeric"}, содержащий
#'   наименование показателя или номер показателя в списке показателей комбинаторной модели
#'
#' @return возвращает объект класса \code{"Combinatorics"} с внесёнными изменениями
setGeneric(name="combinatorics.remove.parameter",
           def=function(model, parameter){
             standardGeneric("combinatorics.remove.parameter")
           }
)

#' @describeIn combinatorics.remove.parameter исключает показатель по его наименованию
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.remove.parameter(model, "R1")
#' @export
setMethod(f="combinatorics.remove.parameter",
          signature=c("Combinatorics","character"),
          definition=function(model, parameter)
          {
            if (length(model@parameters)){
              for (index in 1:length(model@parameters)) {
                if (model@parameters[[index]]@name==parameter){
                  model@parameters[[index]] <- NULL
                  #Удалим значения из массива мероприятий
                  if (length(model@arrangements)){
                    for (aindex in 1:length(model@arrangements)) {
                      arrangement<- model@arrangements[[aindex]]
                      model@arrangements[[aindex]]@value <- arrangement@value[-index]
                    }
                  }
                  return(model)
                }
              }
            }
            return(model)
          }
)

#' @describeIn combinatorics.remove.parameter исключает показатель по индексу в списке
#'   показателей комбинаторной модели
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.remove.parameter(model, 4)
#' @export
setMethod(f="combinatorics.remove.parameter",
          signature=c("Combinatorics","numeric"),
          definition=function(model, parameter)
          {
            if (length(model@parameters)){
              if (is.null(model@parameters[parameter])){
                stop("Index parametes not found")
              } else{
                model@parameters[[parameter]] <- NULL
                #Удалим значения из массива мероприятий
                if (length(model@arrangements)){
                  for (aindex in 1:length(model@arrangements)) {
                    arrangement<- model@arrangements[[aindex]]
                    model@arrangements[[aindex]]@value <- arrangement@value[-parameter]
                  }
                }
              }
            }
            return(model)
          }
)
