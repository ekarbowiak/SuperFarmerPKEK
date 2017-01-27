test_that("Wartosc krolika", {
wynik <- 1
expect_equal(strategy3_wartosc_stada(c(1,0,0,0,0,0,0)), wynik)
})

test_that("Wartosc krowy", {
wynik <- 36
expect_equal(strategy3_wartosc_stada(c(0,0,0,1,0,0,0)), wynik)
})

test_that("Funkcja wartosc zwraca obiekt o klasie numeric",{expect_is(strategy3_wartosc_stada(c(1,0,3,0,0,1,0)),"numeric")})
