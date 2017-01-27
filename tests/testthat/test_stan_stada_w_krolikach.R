test_that("Rezultatem funkcji stan_stada_w_krolikach jest liczba",
          {expect_is(strategy3_wartosc_stada(c(1,1,1,0,0,0,0)),"numeric")})

test_that("Dwa konie maja wartosc 144 krolikow",{
StadoGracza <- c(0,0,0,0,2,0,0)
names(StadoGracza) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')
wynik <- strategy3_wartosc_stada(StadoGracza)
expect_equal(wynik,144)
})

test_that("Duzy pies i krowa maja wartosc konia",{
StadoGracza <- c(0,0,0,1,0,0,1)
StadoGracza1 <- c(0,0,0,0,1,0,0)
names(StadoGracza) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')
names(StadoGracza1) <- c('krolik','owca','swinia','krowa','kon','maly_pies','duzy_pies')
wynik <- strategy3_wartosc_stada(StadoGracza)
wynik1 <- strategy3_wartosc_stada(StadoGracza1)
expect_equal(wynik,wynik1)
})
