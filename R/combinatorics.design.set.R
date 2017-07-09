#' @include combinatorics.model.R
NULL

##Модификация модели. Изменение названия модели
#
#' Method set model name
#'
#' @description
#'   Присваивает название комбинаторной модели
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param name объект класса \code{"character"}, содержащий название модели
#'
#' @return возвращает объект класса \code{"Combinatorics"} с изменённым названием модели
#'
#' @name combinatorics.set.name
#' @rdname combinatorics.set.name
#' @exportMethod combinatorics.set.name
setGeneric(name="combinatorics.set.name",
           def=function(model, name){
             standardGeneric("combinatorics.set.name")
           }
)
#' @rdname combinatorics.set.name
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   model <- combinatorics.set.name (model, 'Demo Name')
#'   model@name
setMethod(f="combinatorics.set.name",
          signature=c("Combinatorics","character"),
          definition=function(model, name)
          {
            model@name <- name
            return(model)
          }
)

##Указание показателя как ресурса как целевого
#' @name combinatorics.set.target
#' @title Set target in model
#' @description
#'   Присваивает одному из показателей комбинаторной модели флаг использования его в качестве
#'   целевого показателя или снимает снимает флаг. В комбинаторной модели при расчёте может быть
#'   только один целевой показатель.
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param parameter объект класса \code{"character"} или \code{"numeric"}, содержащий
#'  наименование показателя или индекс показателя в списке
#' @param value объект класса \code{"boolean"}, содержащий флаг использования показателя
#'  как целевого показателя. TRUE - показатель используется как целевой показатель.
#'
#' @return возвращает объект класса \code{"Combinatorics"} с внесёнными изменениями
setGeneric(name="combinatorics.set.target",
           def=function(model, parameter, value){
             standardGeneric("combinatorics.set.target")
           }
)
# @name combinatorics.set.target
#' @describeIn combinatorics.set.target поиск показателя по наименованию и установка флага
#'  его использования в качестве целевого показателя.
#'
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.set.target (model, 'R1', TRUE)
#' @export
setMethod(f="combinatorics.set.target",
          signature=c("Combinatorics","character", "logical"),
          definition=function(model, parameter, value)
          {
            if (length(model@parameters)){
              for (index in 1:length(model@parameters)) {
                if (model@parameters[[index]]@name==parameter){
                  #Уберём остальные показателя как целевые
                  if (value){
                    for(j in 1:length(model@parameters)){
                      model@parameters[[j]]@isTarget <- FALSE
                    }
                  }
                  model@parameters[[index]]@isTarget <- value
                  return(model)
                }
              }
            }
            stop(sprintf("Parameter %s not found in model!", parameter))
            return(model)
          }
)

# @name combinatorics.set.target
#' @describeIn combinatorics.set.target поиск показателя по индексу в списке показателей и установка флага
#'  его использования в качестве целевого показателя.
#'
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.set.target (model, 4, TRUE)
#' @export
setMethod(f="combinatorics.set.target",
          signature=c("Combinatorics","numeric", "logical"),
          definition=function(model, parameter, value)
          {
            if (length(model@parameters)){
              if (is.null(model@parameters[parameter])){
                stop("Index parametes not found")
              } else{
                #Уберём остальные показателя как целевые
                if (value){
                  for(j in 1:length(model@parameters)){
                    model@parameters[[j]]@isTarget <- FALSE
                  }
                }
                model@parameters[[parameter]]@isTarget <- value
                return(model)
              }
            }
            return(model)
          }
)

#
#
#
#
##Указание как ресурса как ограниченного
#' @name combinatorics.set.limit
#'
#' @title Set limit in model
#'
#' @description
#'   Присваивает одному из показателей комбинаторной модели флаг использования его в качестве
#'   ограниченного ресурса. В комбинаторной модели при расчёте может быть один или несколько
#'   целевых ресурсов.
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param parameter объект класса \code{"character"} или \code{"numeric"}, содержащий
#'   наименование показателя или индекс показателя в списке показателей
#' @param value объект класса \code{"numeric"}, содержащий значение огранченного ресурса.
#'
#' @note
#'   При наличии более одного ограниченного ресурса поиск оптимума автоматически производится с
#'   применением следующего утверждения: \emph{решение, оптимальное по одному ограниченному ресурсу
#'   (называемому, исходным) и допустимое по остальным, является оптимальным по всем ресурсным
#'   ограничениям}. При этом выбирается то из оптимальных по исходному ограниченному ресурсу
#'   решений, которое удовлетворяет всем ресурсным ограничениям и требует затрат хотя бы одного
#'   из ограниченных ресурсов  в максимально возмоэной степени (этот ресурс называется наиболее
#'   дефицитным).
#'
#'   \strong{Примечание:} Если исходный ограниченный ресурс и наиболее дефицитный ограниченный ресурс не
#'   совпадают, то возможны, ситуации, когда описанный алгоритм игнорирует строго оптимальное
#'   решение, а выбирает  решение  близкое к оптимальному.  При этом  необходим  дальнейший
#'   поиск  в  предположении, что  наиболее дефицитный ресурс выбирается в качестве исходного
#'   ограниченного ресурса. В этом   случае    процесс   оптимизации   становится  многоэтапным.
#'   Многоэтапный  поиск  производится атоматически.
#'
#' @return возвращает объект класса \code{"Combinatorics"} с внесёнными изменениями
#'
setGeneric(name="combinatorics.set.limit",
           def=function(model, parameter, value){
             standardGeneric("combinatorics.set.limit")
           }
)
# @name combinatorics.set.limit
#' @describeIn combinatorics.set.limit поиск показателя по наименованию и установка его как
#' ограниченного ресурса, с указанием лимита ограниченного ресурса
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.set.limit (model, 'R1', 100)
#' @export
setMethod(f="combinatorics.set.limit",
          signature=c("Combinatorics","character", "numeric"),
          definition=function(model, parameter, value)
          {
            if (length(model@parameters)){
              for (index in 1:length(model@parameters)) {
                if (model@parameters[[index]]@name==parameter){
                  model@parameters[[index]]@isLimited <- TRUE
                  model@parameters[[index]]@value <- value
                  return(model)
                }
              }
            }
            stop(sprintf("Parameter %s not found in model!", parameter))
            return(model)
          }
)

# @name combinatorics.set.limit
#' @describeIn combinatorics.set.limit поиск показателя по индексу в списке показателей и
#' установка его как ограниченного ресурса, с указанием лимита ограниченного ресурса
#'
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.set.limit (model, 4, 100)
#' @export
setMethod(f="combinatorics.set.limit",
          signature=c("Combinatorics","numeric", "numeric"),
          definition=function(model, parameter, value)
          {
            if (length(model@parameters)){
              if (is.null(model@parameters[parameter])){
                stop("Index parametes not found")
              } else{
                model@parameters[[parameter]]@isLimited <- TRUE
                model@parameters[[parameter]]@value <- value
                return(model)
              }
            }
            stop(sprintf("Parameter %s not found in model!", parameter))
            return(model)
          }
)


#
#
#
#
#
##Модификация модели. Изменение характеристик мероприятий
#' @name combinatorics.set.arrangement
#' @title Set arrangement values
#' @description
#'   Присваивает одному из мероприятий в комбинаторной модели значения показателей.
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель
#' @param arrangement объект класса \code{"character"} или \code{"numeric"}, содержащий
#'   наименование мероприятия или индекс мероприятия в списке мероприятий
#' @param values объект класса \code{"vector"}, содержащий числовые значения показателей.
#'   Количество значений в векторе должно быть равно колчеству показателей в комбинаторной модели
#'
#' @return возвращает объект класса \code{"Combinatorics"} с внесёнными изменениями
setGeneric(name="combinatorics.set.arrangement",
           def=function(model, arrangement, values){
             standardGeneric("combinatorics.set.arrangement")
           }
)

# @name combinatorics.set.arrangement
#' @describeIn combinatorics.set.arrangement поиск мероприятия по наименованию и установка новых
#'  значений показателей.
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.set.arrangement(model, "M0", c(10, 20, 30, 40, 50, 60.6))
#' @export
setMethod(f="combinatorics.set.arrangement",
          signature=c("Combinatorics","character", "vector"),
          definition=function(model, arrangement, values)
          {
            if (length(values)!= length(model@parameters)){
              stop(sprintf("Size vector values not equals parameters count. Size parameters is %g", length(model@parameters)))
            }
            if (length(model@arrangements)){
              for (index in 1:length(model@arrangements)) {
                if (model@arrangements[[index]]@name==arrangement){
                  model@arrangements[[index]]@value <- values
                  return(model)
                }
              }
            }
            return(model)
          }
)

# @name combinatorics.set.arrangement
#' @describeIn combinatorics.set.arrangement поиск мероприятия по индексe в списке мероприятий
#'  и установка новых значений показателей.
#' @examples
#'
#'   model <- combinatorics.example.Model3()
#'   combinatorics.set.arrangement(model, 1, c(10.1, 20.2, 30.3, 40.4, 50.5, 60.6))
#' @export
setMethod(f="combinatorics.set.arrangement",
          signature=c("Combinatorics","numeric", "vector"),
          definition=function(model, arrangement, values)
          {
            if (length(values)!= length(model@parameters)){
              stop(sprintf("Size vector values not equals parameters count. Size parameters is %g", length(model@parameters)))
            }
            if (length(model@arrangements)){
              if (is.null(model@arrangements[arrangement])){
                stop("Index arrangement not found")
              } else{
                model@arrangements[[arrangement]]@value <- values
              }
            }
            return(model)
          }
)
