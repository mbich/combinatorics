# Модель Сombinatorics
#'@title Combinatorics model
#'
#'@description Комбинаторные модели - это модели поиска оптимальных решений при наличии ресурсных
#' ограничений, логических связей и целей. Примерами моделей в частности является:
#' * Компоновка инженерно-экономического проекта;
#' * Построение системы, оптимальной по надежности;
#' * Управление работами;
#' * Управление портфелем ценных бумаг;
#' * Предупреждение чрезвычайных ситуаций.
#'
#'@note
#' Во всех случаях, когда имеются варианты мероприятий, ресурсные ограничения, логические
#' связи и цели возникает комбинаторная задача. Практика свидетельствует о наличии соотношения
#' 1:10:100:1000, где один доллар - «экономия» на принятии упрощенного решения на стадии
#' его формирования, а 10, 100, 1000 - потери на последующих стадиях жизненного цикла решения.
#'
#' **Мероприятие** - составная часть инвестиционного,  технического, правового или  иного проекта,
#' требующая затрат,  приносящая результаты и увязанная с другими мероприятиями логическими
#' связями. Мероприятие, включенное в логическую сеть, называется  элементом. Мероприятие так же
#' означает элемент множества. Решение задачи булевого программирования представляет собой
#' набор мероприятий (элементов), не противоречащий сети логических связей, требующий затрат
#' ограниченного ресурса в заданном пределе и доставляющий максимум (или минимум) целевому
#' показателю.
#'
#' **Показатели** - характеристика мероприятия в комбинаторной модели, имеющая количественное
#' выражение. Предполагается, что показатели аддитивны, то есть, что для любого набора мероприятий
#' соответствующие показатели можно складывать. Среди показателей один или несколько
#' выбирается в качестве ограниченного ресурса и один в качестве целевого показателя.
#' Ограниченный ресурс - являющется дефицитным или  лимитированным.
#' Целевые показатели - принимаемые в качестве критерия при дискретной оптимизации.
#'
#' **Логическая связь** (ЛС) - совместимость, обязательная последовательность осуществления или
#' обязательная необходимость  осуществления мероприятий, вытекающая из технологических,
#' технических, организационных или правовых условий их осуществления (в математической логике
#' используется термин "логический оператор"). Логическя связь записываются в виде логической формулы.
#'
#' **Логическая формула** - логическая связь между мероприятий, выраженная одним из следующих способов:
#'  * `[…,…,…]` – в квадратных скобках перечислены номера мероприятий, разделенные запятой, которые
#' исключают друг друга (логическая связь «или»);
#'  * `(…,…,…)->(…,…,…)`, - в скобках, слева от стрелки, дан перечень номеров мероприятий,
#' разделенных запятой, присутствие каждого из которых в решении является обязательным условием
#' для того, чтобы решение могло содержать все или некоторые из мероприятий, номера которых
#' перечислены в скобках, справа от стрелки (логическая связь «если…то…»);
#'  * `{…,…,…}` – в фигурных скобках перечислены номера мероприятий, разделенные запятой, среди
#' которых хотя бы одно непременно должно быть включено в решение
#' (логическая связь «должен присутствовать хотя бы один из…»;
#'  * `…` - номер мероприятия в отдельной строке означает мероприятие, логически независимое от
#'  остальных («Независимое мероприятие»)
#'
#'  **Логическая сеть** - совокупность мероприятий  с наложенными на них логическими связями.
#'  Логическая сеть вводится и редактируется как совокупность логических связей, включающих,
#'  в том числе, независимые мероприятия (иначе  говоря,  мероприятия,  не имеющие логических
#'  связей с другими мероприятиями).
#'@md
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{parameters}:}{объект класса \code{"list"}, содержащий показатели модели}
#'    \item{\code{arrangements}:}{объект класса  \code{"list"}, содержащий варианты мероприятий модели}
#'    \item{\code{logicconnections}:}{объект класса \code{"list"}, содержащий логическую сеть модели}
#'    \item{\code{name}:}{объект класса \code{"character"}, содержащий наименование модели}
#'  }
#'
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

#' @rdname Combinatorics-class
#'
#' @param .Object Комбинаторная модель
#' @param ... аргументы конструктура
#' @param data объект класса \code{"matrix"}, содержащий матрицу, где в строках указаны
#'   мероприятия, а в столбцах показатели мероприятий (ресурсные ограничение и целевые
#'   показатели).
#' @param arrayLogicalConnections объект класса \code{"vector"}, содержащий
#'    перечень логических связей в виде логических формул.
#
#' @return Комбинаторная модель
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

#' Используйте метод combinatorics.model для создания новой комбинаторной модели.
#' @name combinatorics.model
#' @rdname Combinatorics-class
#'
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
