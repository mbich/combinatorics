
<!-- README.md is generated from README.Rmd. Please edit that file -->
combinatorics
=============

The goal package of combinatorics is to algorithm implementation of discrete optimization

Installation
------------

You can install combinatorics from github with:

``` r
# install.packages("devtools")
devtools::install_github("mbich/combinatorics")
```

Example
-------

Постановка комбинаторной задачи естественным образом происходит при анализе ситуации, в которой имеются варианты различных мероприятий. Рассмотрим предельно простой пример. Пусть фирма предполагает купить станок. При этом возникают варианты мероприятий, представленные в следующей таблице:

<table style="width:93%;">
<colgroup>
<col width="2%" />
<col width="26%" />
<col width="30%" />
<col width="16%" />
<col width="16%" />
</colgroup>
<thead>
<tr class="header">
<th>№</th>
<th>Название мероприятия</th>
<th>Описание мероприятия</th>
<th align="right">Затраты, тыс.руб.</th>
<th align="right">Прибыль в год, тыс.руб.</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>1</td>
<td>Станок - А</td>
<td>Покупка станка марки «А»</td>
<td align="right">350</td>
<td align="right">117</td>
</tr>
<tr class="even">
<td>2</td>
<td>Станок - Б</td>
<td>Покупка станка марки «Б»</td>
<td align="right">420</td>
<td align="right">125</td>
</tr>
<tr class="odd">
<td>3</td>
<td>Аренда - А</td>
<td>Аренда станка марки «А»</td>
<td align="right">330</td>
<td align="right">105</td>
</tr>
<tr class="even">
<td>4</td>
<td>Оборудование к А</td>
<td>Покупка дополнительного оборудования к станку марки «А»</td>
<td align="right">77</td>
<td align="right">83</td>
</tr>
<tr class="odd">
<td>5</td>
<td>Оборудование к Б</td>
<td>Покупка дополнительного оборудования к станку марки «Б»</td>
<td align="right">75</td>
<td align="right">81</td>
</tr>
</tbody>
</table>

Из приведенных вариантов мероприятий проистекает следующий набор логических связей между мероприятиями:

-   \[1,2,3\] - мероприятие № 1, мероприятие № 2, мероприятие № 3 взаимно друг друга исключают
-   {1,2,3} - какое-либо из мероприятий № 1, № 2, или № 3 обязательно должно быть исполнено;
-   (1)-&gt;(4), (3)-&gt;(4) - если будет исполнено мероприятие № 1 или мероприятие № 3 то может быть исполнено и мероприятие № 4;
-   (2)-&gt;(5) - если будет исполнено мероприятие № 2 то может быть исполнено и мероприятие № 5.

Общая сумма средств на покупку станка и оборудования не должна быть более, чем 700 т.руб. Задача в том, чтобы получить максимальную прибыль.

Загрузим пакет combinatorics

``` r
require(combinatorics)
#> Loading required package: combinatorics
#> Loading required package: rJava
```

Создадим новую модель и заполним её данными. В качестве целевого параметра укажем - "Прибыль", в качестве ограниченного ресурса - "Затраты" и зададим лимит ограниченного ресурса в размере 700.

``` r
model <- combinatorics.model(name="Пример")
model <- combinatorics.add.parameter(model, "Затраты",  "тыс.руб.", 700)
model <- combinatorics.add.parameter(model, "Прибыль в год",  "тыс.руб.")
model <- combinatorics.set.target(model, "Прибыль в год", TRUE)
model <- combinatorics.add.arrangement(model, "Станок - А", c(350.0, 117.0))
model <- combinatorics.add.arrangement(model, "Станок - Б", c(420.0, 125.0))
model <- combinatorics.add.arrangement(model, "Аренда - А", c(330.0, 105.0))
model <- combinatorics.add.arrangement(model, "Оборудование к А", c(77,83))
model <- combinatorics.add.arrangement(model, "Оборудование к Б", c(75,81))
model <- combinatorics.add.logicconnection(model, "[1,2,3]")
model <- combinatorics.add.logicconnection(model, "{1,2,3}")
model <- combinatorics.add.logicconnection(model, "(1)->(4)")
model <- combinatorics.add.logicconnection(model, "(3)->(4)")
model <- combinatorics.add.logicconnection(model, "(2)->(5)")
model
#> 
#> Combinatorics model: Пример(Combinatorics)
#> INITIAL DATA
#>                   Затраты, тыс.руб.  Прибыль в год, тыс.руб.
#> Станок - А                      350                      117
#> Станок - Б                      420                      125
#> Аренда - А                      330                      105
#> Оборудование к А                 77                       83
#> Оборудование к Б                 75                       81
#> 
#> Target parameter: Прибыль в год
#> Limit parameter:
#>                    value
#> Затраты, тыс.руб.    700
#> 
#> Logical connections:
#>  [1,2,3]
#>  {1,2,3}
#>  (1)->(4)
#>  (3)->(4)
#>  (2)->(5)
```

Выполним расчёт на максимум

``` r
combinatorics.calculate(model)
#> 
#> Model: Пример(Combinatorics)
#> Calculate from: maximun
#> Target parameter: Прибыль в год
#> Limit parameter:
#>          value
#> Затраты    700
#> Optimal combination of arrangements!
#>    Прибыль в год  Затраты      T/L  Arrangements
#> 1            105      330  0.31818             3
#> 2            117      350  0.33429             1
#> 3            188      407  0.46192          3, 4
#> 4            200      427  0.46838          1, 4
#> 5            206      495  0.41616          2, 5
```

Увеличим значение ограниченного ресурса `"Затраты"` до 900 тыс.руб. и повторно выполним расчёт на максимум

``` r
model <- combinatorics.set.limit(model, "Затраты", 900)
combinatorics.calculate(model)
#> 
#> Model: Пример(Combinatorics)
#> Calculate from: maximun
#> Target parameter: Прибыль в год
#> Limit parameter:
#>          value
#> Затраты    900
#> Optimal combination of arrangements!
#>    Прибыль в год  Затраты      T/L  Arrangements
#> 1            105      330  0.31818             3
#> 2            117      350  0.33429             1
#> 3            188      407  0.46192          3, 4
#> 4            200      427  0.46838          1, 4
#> 5            206      495  0.41616          2, 5
```

Для просмотра названий в списке оптимальных мероприятий нужно добавить параметр `nameArrangment`

``` r
out <- combinatorics.calculate(model) 
print(out, nameArrangment=TRUE)
#> 
#> Model: Пример(Combinatorics)
#> Calculate from: maximun
#> Target parameter: Прибыль в год
#> Limit parameter:
#>          value
#> Затраты    900
#> Optimal combination of arrangements!
#>    Прибыль в год  Затраты      T/L                  Arrangements
#> 1            105      330  0.31818                    Аренда - А
#> 2            117      350  0.33429                    Станок - А
#> 3            188      407  0.46192  Аренда - А, Оборудование к А
#> 4            200      427  0.46838  Станок - А, Оборудование к А
#> 5            206      495  0.41616  Станок - Б, Оборудование к Б
```
