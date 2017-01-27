test_that("Stado gracza nie zostanie rozmnozone o wiecej zwierzat niz w stadzie glownym",{
StadoGracza <- c(59,5,2,0,0,1,0)
names(StadoGracza) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
wynik <- reproduction("krolik",2,StadoGracza)
expect_equal(wynik[['krolik']],60)
})

test_that("Dla braku danych zwierzat przy identycznym wyniku na obu kostkach gracz otrzymuje 1 zwierze",{
StadoGracza <- c(0,0,0,0,0,0,0)
names(StadoGracza) <- c("krolik","owca","swinia", "krowa", "kon", "maly_pies","duzy_pies")
wynik <- reproduction("krolik",2,StadoGracza)
expect_equal(wynik[['krolik']],0)
})