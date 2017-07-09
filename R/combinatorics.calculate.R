#' @include combinatorics.model.R
NULL

#' Calculation of the combinatorics model
#'
#' @description
#' Расчёт комбинаторной модели на максимум, т.е. максимизируется значение целевого показателя - «притока»
#' или на минимум, т.е. предел допустимого совокупного «притока». Смысл расчёта на миниму в том, что
#' Установив заранее желаемую цель, можной найти те минимально необходимые затраты, которые
#' обеспечат достижение этой цели.
#'
#' Для выполнения расчёта должны быть указаные значения пределов ограниченных ресурсов и выбран
#' целевой показатель.
#'
#' @param model объект класса \code{"Combinatorics"}, содержащий комбинаторную модель задачи
#' @param maximum объект класса \code{"boolean"}, содержащий флаг указания расчёта на максими или
#' минимум.
#' @param ... дополнительные аргументы, передаваемые другим методам или получаемые от них.
#'
#' @return
#'  объект, содержаший результаты расчёта комбинаторной модели на максимум
#'  или минимум
#'
#' @examples
#'   combinatorics.calculate(combinatorics.example.Model1())
#'
#'   combinatorics.calculate(combinatorics.example.Model2())
#'
#'   result <- combinatorics.calculate(combinatorics.example.Model1(), maximum=FALSE)
#'   print(result, nameArrangment=TRUE)
#'   plot(result, type="o")
#' @export
combinatorics.calculate <- function(model, maximum=TRUE, ...){
  if (!isClass(model)){
    stop("Model not CombinatoricsModel")
  }
  m <- as(model, "Combinatorics")
  #Формируем описание Оттоков и Притоков
  parameters <- character()
  for (i in 1:length(m@parameters)) {
    parameter<- m@parameters[[i]]
    parameters[i] <- paste(parameter@name, tolower(parameter@isTarget), tolower(parameter@isLimited), parameter@value, sep=",")
  }
  parameters <- .jarray(parameters)
  #Формирование описания мероприятий
  arrangements <- character()
  for (i in 1:length(m@arrangements)) {
    arrangement<- m@arrangements[[i]]
    arrangements[i] <- arrangement@name
  }
  arrangements<- .jarray(arrangements)
  #Формирование описания оттоков/притоков для мероприятий
  cells <- list()
  for (i in 1:length(m@arrangements)) {
    arrangement<- m@arrangements[[i]]
    cells[[i]] <- as.character(arrangement@value)
  }
  cell <- .jarray(lapply(cells, .jarray))

  #Формирование логических связей
  lc <- character()
  for (i in 1:length(m@logicconnections)) {
    item<- m@logicconnections[[i]]
    lc[i] <- item@value
  }
  lc<- .jarray(lc)

  hjw <- .jnew("ru/decision/combinatorics/R")
  out <- .jcall(hjw, "V", "setMaximum", maximum)
  out <- .jcall(hjw, "V", "setShiftArrangementInLogicConnection", as.integer(1))

  out <- .jcall(hjw, "[Ljava/lang/String;", "calculate", parameters, arrangements, cell, lc, evalString = FALSE)
  #out <- .jcall(hjw, "[Ljava/lang/String;", "calculateEcho", parameters, arrangements, cell, lc, evalString = FALSE)
  result <- list(model=model, kind=if(maximum)"maximun" else "minimum")
  class(result) <- c("combinatorics.result")
  #Заполним список с комбинацией мероприятий
  #Первый столбце всего целевой параметр
  #Затем идут ограниченные ресурсы
  #Затем вектор с номерами мероприятиф
  count <- .jcall(hjw, "I", "getResultArrangementCount")
  if (count){
    data <- data.frame(target=numeric(), limit=I(list()), tl=I(list()), arrangements=I(list()))
    for (index in 1:count) {
      target <- .jcall(hjw, "D", "getResultTraget", as.integer(index-1), evalString = FALSE)
      limit <- .jcall(hjw, "[D", "getResultLimit", as.integer(index-1), evalString = FALSE)
      elements <- .jcall(hjw, "[Ljava/lang/String;", "getResultArrangements", as.integer(index-1), evalString = FALSE)
      data <- rbind(data, data.frame("target"=target, "limit"=I(list(limit)),
                                     "tl"=I(list(round(target/limit, digits=5))), "arrangements"=I(list(elements))))
    }
    #Индекс целевого параметра для первой комбинации мероприятий.
    targetIndex <- .jcall(hjw, "I", "getResultTragetParameterIndex", 0L, evalString = FALSE)
    result$target <- m@parameters[[targetIndex+1]]
    #Индексы ограниченных ресурсов  для первой комбинации мероприятий.
    limitIndexes <- .jcall(hjw, "[I", "getResultLimitParameterIndex", 0L, evalString = FALSE)
    result$limit <- m@parameters[c(limitIndexes)+1]
    result$data <- data
  }
  return(result)
}
