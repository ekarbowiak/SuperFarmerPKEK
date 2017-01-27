test_that("Poprawny wynik po ataku lisa", {
StadoGracza <- c(15,0,3,1,0,0,0)
names(StadoGracza) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
wynik <- c(0,0,3,1,0,0,0)
names(wynik) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
expect_equal(reproduction("krolik","lis", StadoGracza), wynik)
})

test_that("Poprawny wynik po ataku wilka",{
StadoGracza <- c(2,5,2,0,0,0,0)
names(StadoGracza) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
wynik <- c(0,0,0,0,0,0,0)
names(wynik) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
expect_equal(reproduction("wilk", "krolik",StadoGracza), wynik)
})

test_that("Wilk nie zjada koni", {
StadoGracza <- c(12,3,1,0,1,0,0)
names(StadoGracza) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
wynik <- c(0,0,0,0,1,0,0)
names(wynik) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
expect_equal(reproduction("wilk","krolik",StadoGracza), wynik)
})

test_that("Maly pies chroni kroliki", {
StadoGracza <- c(11,0,0,0,1,1,0)
names(StadoGracza) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
wynik <- c(11,0,0,0,1,0,0)
names(wynik) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
expect_equal(reproduction("owca","lis",StadoGracza), wynik)
})

test_that("Duzy pies chroni przed wilkiem", {
StadoGracza <- c(11,1,0,1,0,1,1)
names(StadoGracza) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
wynik <- c(11,1,0,1,0,1,0)
names(wynik) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
expect_equal(reproduction("wilk","swinia",StadoGracza), wynik)
})
