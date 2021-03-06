% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combinatorics.models.R
\name{Model3}
\alias{Model3}
\alias{combinatorics.example.Model3}
\title{Combinatorial model of large dimension}
\usage{
combinatorics.example.Model3()
}
\description{
Пример комбинаторной модели, включающий 25 мероприятий (M1-M25), 6 показателей
  (три ограниченных ресурса - ОР1, ОР2, ОР3; один целевой показатель - ЦП; и два
  дополнительных показателя - R1, R2) и логическую сеть из 15 логических связей.
}
\examples{
  model3 <- combinatorics.example.Model3()
  combinatorics.calculate(model3)

}
