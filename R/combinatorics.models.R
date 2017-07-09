#Демонтрационные модели
#
#' Combinatorial model of the simplest problem
#'
#' @description
#'  Пример простейшей комбинаторной модели, для задачи в которой фирме предполагает купить станок.
#'  При этом возникают варианты мероприятий, представленные в следующей таблице:
#'  \tabular{cllrr}{
#'  № \tab Название мероприятия \tab Описание мероприятия \tab Затраты, тыс.руб \tab Прибыль в год, тыс.руб. \cr
#'  1 \tab Станок - А       \tab Покупка станка марки «А» \tab 350 \tab 117 \cr
#'  2 \tab Станок - Б       \tab Покупка станка марки «Б» \tab 420 \tab 125 \cr
#'  3 \tab Аренда - А       \tab Аренда станка марки «А»  \tab 330 \tab 105 \cr
#'  4 \tab Оборудование к А \tab Покупка дополнительного оборудования к станку марки «А» \tab 77 \tab 83 \cr
#'  5 \tab Оборудование к Б \tab Покупка дополнительного оборудования к станку марки «Б» \tab 75 \tab 81
#'  }
#'
#'    Из приведенных вариантов мероприятий проистекает следующий набор логических связей между мероприятиями:
#'
#'  * `[1,2,3]`  - мероприятие № 1, мероприятие № 2, мероприятие № 3  взаимно друг друга исключают
#'  * `{1,2,3}` - какое-либо из мероприятий № 1, № 2, или № 3 обязательно должно быть исполнено;
#'  * `(1)->(4), (3)->(4)` - если будет исполнено мероприятие № 1 или мероприятие № 3 то может быть исполнено и мероприятие № 4;
#'  * `(2)->(5)` - если будет исполнено мероприятие № 2 то может быть исполнено и мероприятие № 5.
#'
#'  Ограниченным ресурсом являются средства на покупку станка и оборудования. Значение ограниченного
#'  ресурса задано в 700 т.руб. Целевым показателем является получение максимальной прибыли.
#'
#' @md
#'
#' @return возвращает объект класса \code{"Combinatorics"} с исходными данными задачи
#'
#' @name Model1
#' @rdname combinatorics.model1-class
#'
#' @examples
#'   model1 <- combinatorics.example.Model1()
#'   combinatorics.calculate(model1)
#'   print(combinatorics.calculate(model1), nameArrangment = TRUE)
#'
#' @export combinatorics.example.Model1
combinatorics.example.Model1 <- function(){
  data <- matrix(c(350, 117,
                   420, 125,
                   330, 105,
                   77, 83,
                   75, 81),
                 nrow=5, ncol=2, byrow = TRUE)
#  dimnames(data)<- list(c("Станок - А", "Станок - Б", "Аренда - А",
#     "Оборудование к А", "Оборудование к Б"), c("Отток", "Приток"))
   dimnames(data)<- list(c("\u0421\u0442\u0430\u043D\u043E\u043A - \u0410",
                          "\u0421\u0442\u0430\u043D\u043E\u043A - \u0411",
                          "\u0410\u0440\u0435\u043D\u0434\u0430 - \u0410",
                          "\u041E\u0431\u043E\u0440\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u0435 \u043A \u0410",
                          "\u041E\u0431\u043E\u0440\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u0435 \u043A \u0411"),
                        c("\u041E\u0442\u0442\u043E\u043A", "\u041F\u0440\u0438\u0442\u043E\u043A"))
  #Логические связи
  lc <- c("[1,2,3]", "{1,2,3}", "(1)->(4)", "(3)->(4)", "(2)->(5)")
  model <- combinatorics.model(name="Model1", data=data, arrayLogicalConnections=lc)
  model <- combinatorics.set.limit(model, 1, 500)
  model <- combinatorics.set.target(model, 2, TRUE)
  return(model)
}


#' Combinatorics model of the simple optimal integrated investment program
#'
#' @description
#'   Пример комбинаторной модели, для задачи формирования оптимальной комплексной
#'   инвестиционной программы предприятия.
#' @note
#'   «Даунбрукс» - агропромышленное предприятие, расположенное в
#'   Московской области недалеко от аэропорта. Предприятие специализируется на
#'   производстве грибов-шампиньонов и имеет незавершенное строительство в виде
#'   крупного павильона, в котором могут быть размещены различные производства.
#'   Кроме того, оно имеет некоторые свободные земельные участки и располагает
#'   ограниченными собственными средствами для финансирования проектов.
#'   Однако предприятие может привлечь соинвесторов на взаимно выгодных
#'   условиях.
#'
#'   С помощью специалистов предприятия и соинвесторов было разработано
#'   множество возможных вариантов инвестиционных проектов (ИП), которые в
#'   совокупности и образовали портфель ИП. Для каждого ИП рассчитан валовой
#'   дисконтированный на весь горизонт проекта доход («приток») и совокупные на
#'   весь горизонт проекта дисконтированные затраты («отток»)
#'  \tabular{clrr}{
#'  № \tab Название инвестиционного проекта \tab Отток, млн.руб. \tab Приток, млн.руб. \cr
#'  1 \tab Расширение шампиньонного комплекса \tab 5.40 \tab 7.60 \cr
#'  2 \tab Производство упаковки \tab 3.70 \tab 4.40 \cr
#'  3 \tab Таможенный склад \tab 2.70 \tab 3.20 \cr
#'  4 \tab Склад продукции \tab 3.30 \tab 5.60 \cr
#'  5 \tab Производство компоста \tab 1.00 \tab 1.70 \cr
#'  6 \tab Производство рыбы \tab 1.30 \tab 2.30 \cr
#'  7 \tab Теплица 1 \tab 2.50 \tab 3.60 \cr
#'  8 \tab Теплица 2 \tab 1.60 \tab 2.50
#'  }
#'
#'  Предполагается, что между заданным множеством инвестиционных
#'  проектов имеются некоторые логические связи (ЛС), которые могут быть
#'  представлены в следующей содержательной форме:
#'  * \emph{«Расширение шампиньонного комплекса»} - предполагается расширить
#'  производственные площади и модернизировать действующее производство.
#'  * \emph{«Производство упаковки»} – в настоящее время упаковка для грибов
#'  закупается. Данный инвестиционный проект предполагает производить
#'  упаковку собственными силами. При этом производство предполагается
#'  расположить в недостроенном помещении.
#'  * \emph{«Таможенный склад»} – поскольку шампиньонный комплекс
#'  расположен недалеко от аэропорта, то имеющееся недостроенное помещение
#'  может быть достроено, оборудовано соответствующим образом и сдано в
#'  аренду аэропорту.
#'  * \emph{«Склад продукции»} - реконструкция и переоборудование действующего
#'  склада.
#'  * \emph{«Производство компоста»} - компост для производства грибов
#'  производится на предприятии в недостаточном количестве, а остальное
#'  закупается. Данный проект позволит настолько увеличить производство
#'  компоста, что его будет достаточно как для собственных нужд, так и для
#'  продажи.
#'  * \emph{«Производство рыбы»} – строительство бассейна для промышленного
#'  выращивания рыбы.
#'  * \emph{«Теплица 1»} – строительство теплицы для выращивания овощей с
#'  независимым водоснабжением.
#'  * \emph{«Теплица 2»} – строительство теплицы на тех же площадях, что и в
#'  варианте 1, но с водоснабжением из бассейна для рыбы, а так же с
#'  получением части удобрения из бассейна.
#'
#'  Общая сумма средств, которое предприятие может использовать для
#'  инвестиций на все инвестиционные проекты, составляет \strong{11 млн. руб.}
#'  Задача состоит в том, чтобы, не выходя за заданный лимит финансовых
#'  средств и не нарушая ни одного из логических условий сформировать
#'  портфель инвестиционных проектов, у которого суммарный «приток» будет
#'  максимален. Этот портфель ИП и будет оптимальной комплексной
#'  инвестиционной программой предприятия.
#' @md
#'
#' @name Model2
#' @rdname combinatorics.model2-class
#'
#' @examples
#'   model2 <- combinatorics.example.Model2()
#'   combinatorics.calculate(model2)
#'
#' @export combinatorics.example.Model2
combinatorics.example.Model2 <- function(){
  data <- matrix(c(5.4, 7.6,
                   3.7, 4.4,
                   2.7, 3.2,
                   3.3, 5.6,
                   1, 1.7,
                   1.3, 2.3,
                   2.5, 3.6,
                   1.6, 2.5),
                 nrow=8, ncol=2, byrow = TRUE)
#  dimnames(data)<- list(c("Расширение шампиньонного комплекса", "Производство упаковки", "Таможенный склад",
#       "Склад продукции", "Производство компоста", "Производство рыбы", "Теплица 1", "Теплица 2"),
#       c("Отток, млн.руб.", "Приток, млн.руб."))
   dimnames(data)<- list(c("\u0420\u0430\u0441\u0448\u0438\u0440\u0435\u043D\u0438\u0435 \u0448\u0430\u043C\u043F\u0438\u043D\u044C\u043E\u043D\u043D\u043E\u0433\u043E \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u0430",
                           "\u041F\u0440\u043E\u0438\u0437\u0432\u043E\u0434\u0441\u0442\u0432\u043E \u0443\u043F\u0430\u043A\u043E\u0432\u043A\u0438",
                           "\u0422\u0430\u043C\u043E\u0436\u0435\u043D\u043D\u044B\u0439 \u0441\u043A\u043B\u0430\u0434",
                           "\u0421\u043A\u043B\u0430\u0434 \u043F\u0440\u043E\u0434\u0443\u043A\u0446\u0438\u0438",
                           "\u041F\u0440\u043E\u0438\u0437\u0432\u043E\u0434\u0441\u0442\u0432\u043E \u043A\u043E\u043C\u043F\u043E\u0441\u0442\u0430",
                           "\u041F\u0440\u043E\u0438\u0437\u0432\u043E\u0434\u0441\u0442\u0432\u043E \u0440\u044B\u0431\u044B",
                           "\u0422\u0435\u043F\u043B\u0438\u0446\u0430 1",
                           "\u0422\u0435\u043F\u043B\u0438\u0446\u0430 2"),
                          c("\u041E\u0442\u0442\u043E\u043A", "\u041F\u0440\u0438\u0442\u043E\u043A"))
  #Логические связи
  lc <- c("[2,3]", "[7,8]", "(6)->(8)", "1", "4", "5")
  model <- combinatorics.model(name="Model2", data=data, arrayLogicalConnections=lc)
  model@parameters[[1]]@dimension <- "\u043C\u043B\u043D.\u0440\u0443\u0431."
  model@parameters[[2]]@dimension <- "\u043C\u043B\u043D.\u0440\u0443\u0431."
  model <- combinatorics.set.limit(model, 1, 11)
  model <- combinatorics.set.target(model, 2, TRUE)
  return(model)
}


#' Combinatorial model of large dimension
#'
#' @description
#'   Пример комбинаторной модели, включающий 25 мероприятий (M1-M25), 6 показателей
#'   (три ограниченных ресурса - ОР1, ОР2, ОР3; один целевой показатель - ЦП; и два
#'   дополнительных показателя - R1, R2) и логическую сеть из 15 логических связей.
#'
#' @name Model3
#' @rdname combinatorics.model3-class
#'
#' @examples
#'   model3 <- combinatorics.example.Model3()
#'   combinatorics.calculate(model3)
#'
#' @export combinatorics.example.Model3
combinatorics.example.Model3 <- function(){
 model <- combinatorics.model(name="Model3")
 model <- combinatorics.add.parameter(model, "\u041E\u04201", value = 1450)
 model <- combinatorics.add.parameter(model, "\u041E\u04202", value = 1690)
 model <- combinatorics.add.parameter(model, "\u041E\u04203", value = 1550)
 model <- combinatorics.add.parameter(model, "R1")
 model <- combinatorics.add.parameter(model, "R2")
 model <- combinatorics.add.parameter(model, "\u0426\u041F", value = 600)
 model <- combinatorics.set.target(model, "\u0426\u041F", TRUE)
 model <- combinatorics.add.arrangement(model, "M0", c(150.0, 117.0, 30.0, 256.0, 350.0, 25.0))
 model <- combinatorics.add.arrangement(model, "M1", c(220.0, 129.0, 108.0, 108.0, 389.0, 75.0))
 model <- combinatorics.add.arrangement(model, "M2", c(357.0, 183.0, 289.0, 105.0, 185.0, 105.0))
 model <- combinatorics.add.arrangement(model, "M3", c(482.0, 202.0, 12.0, 403.0, 15.0, 185.0))
 model <- combinatorics.add.arrangement(model, "M4", c(492.0, 119.0, 167.0, 189.0, 205.0, 205.0))
 model <- combinatorics.add.arrangement(model, "M5", c(23.0, 292.0, 256.0, 133.0, 304.0, 56.0))
 model <- combinatorics.add.arrangement(model, "M6", c(311.0, 62.0, 313.0, 132.0, 312.0, 18.0))
 model <- combinatorics.add.arrangement(model, "M7", c(56.0, 19.0, 345.0, 125.0, 287.0, 305.0))
 model <- combinatorics.add.arrangement(model, "M8", c(428.0, 118.0, 36.0, 108.0, 405.0, 250.0))
 model <- combinatorics.add.arrangement(model, "M9", c(310.0, 143.0, 123.0, 103.0, 15.0, 115.0))
 model <- combinatorics.add.arrangement(model, "M10", c(151.0, 135.0, 178.0, 114.0, 79.0, 132.0))
 model <- combinatorics.add.arrangement(model, "M11", c(56.0, 148.0, 198.0, 522.0, 115.0, 148.0))
 model <- combinatorics.add.arrangement(model, "M12", c(480.0, 219.0, 35.0, 22.0, 411.0, 126.0))
 model <- combinatorics.add.arrangement(model, "M13", c(202.0, 239.0, 78.0, 117.0, 119.0, 198.0))
 model <- combinatorics.add.arrangement(model, "M14", c(404.0, 298.0, 145.0, 14.0, 298.0, 145.0))
 model <- combinatorics.add.arrangement(model, "M15", c(177.0, 123.0, 175.0, 225.0, 358.0, 79.0))
 model <- combinatorics.add.arrangement(model, "M16", c(138.0, 179.0, 198.0, 862.0, 313.0, 365.0))
 model <- combinatorics.add.arrangement(model, "M17", c(12.0, 163.0, 254.0, 45.0, 279.0, 269.0))
 model <- combinatorics.add.arrangement(model, "M18", c(198.0, 289.0, 287.0, 27.0, 113.0, 205.0))
 model <- combinatorics.add.arrangement(model, "M19", c(419.0, 11.0, 345.0, 113.0, 276.0, 148.0))
 model <- combinatorics.add.arrangement(model, "M20", c(376.0, 178.0, 334.0, 45.0, 119.0, 311.0))
 model <- combinatorics.add.arrangement(model, "M21", c(312.0, 202.0, 128.0, 22.0, 22.0, 24.0))
 model <- combinatorics.add.arrangement(model, "M22", c(205.0, 214.0, 64.0, 59.0, 533.0, 115.0))
 model <- combinatorics.add.arrangement(model, "M23", c(102.0, 186.0, 309.0, 17.0, 249.0, 195.0))
 model <- combinatorics.add.arrangement(model, "M24", c(36.0, 13.0, 330.0, 7.0, 110.0, 260.0))
 lc <- c("6","12","19","[1,4,5]","[2,13,23]","[7,9,23]","[11,20]",
   "(1)->(7,18,22,24)","(3,10)->(15)","(7,5)->(11)","(20)->(25)","(23,24)->(2,13)",
   "{17,21}","{1,5,7,14}","{3,8,15,20,22}")
 model <- combinatorics.add.logicconnection(model, "6")
 model <- combinatorics.add.logicconnection(model, "12")
 model <- combinatorics.add.logicconnection(model, "19")
 model <- combinatorics.add.logicconnection(model, "[1,4,5]")
 model <- combinatorics.add.logicconnection(model, "[2,13,23]")
 model <- combinatorics.add.logicconnection(model, "[7,9,23]")
 model <- combinatorics.add.logicconnection(model, "[11,20]")
 model <- combinatorics.add.logicconnection(model, "(1)->(7,18,22,24)")
 model <- combinatorics.add.logicconnection(model, "(3,10)->(15)")
 model <- combinatorics.add.logicconnection(model, "(7,5)->(11)")
 model <- combinatorics.add.logicconnection(model, "(20)->(25)")
 model <- combinatorics.add.logicconnection(model, "(23,24)->(2,13)")
 model <- combinatorics.add.logicconnection(model, "{17,21}")
 model <- combinatorics.add.logicconnection(model, "{1,5,7,14}")
 model <- combinatorics.add.logicconnection(model, "{3,8,15,20,22}")
 return (model)
}
