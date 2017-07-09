# A unit test for combinatorics.example.*. Количество элементов моделях
if(require(testthat))
{
  test_that("tests for structure combinatorics.example.Model1", {
    model <- combinatorics.example.Model1()
    #Количество мероприятий, логических связей и показателей
    expect_equal(length(model@parameters), 2)
    expect_equal(length(model@arrangements), 5)
    expect_equal(length(model@logicconnections), 5)

    #Характеристики показателей
    expect_true(model@parameters[[1]]@isLimited)
    expect_true(model@parameters[[2]]@isTarget)
  })

  test_that("tests for structure combinatorics.example.Model2", {
    model <- combinatorics.example.Model2()

    #Количество мероприятий, логических связей и показателей
    expect_equal(length(model@parameters), 2)
    expect_equal(length(model@arrangements), 8)
    expect_equal(length(model@logicconnections), 6)

    #Характеристики показателей
    expect_true(model@parameters[[1]]@isLimited)
    expect_true(model@parameters[[2]]@isTarget)
  })

  test_that("tests for structure combinatorics.example.Model3", {
    model <- combinatorics.example.Model3()

    #Количество мероприятий, логических связей и показателей
    expect_equal(length(model@parameters), 6)
    expect_equal(length(model@arrangements), 25)
    expect_equal(length(model@logicconnections), 15)

    #Характеристики показателей
    expect_true(model@parameters[[1]]@isLimited)
    expect_true(model@parameters[[2]]@isLimited)
    expect_true(model@parameters[[3]]@isLimited)
    expect_true(model@parameters[[6]]@isTarget)
  })
}
