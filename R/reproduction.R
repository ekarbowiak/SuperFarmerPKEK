#' Mechanizm gry, dodawanie zwierzat po rzucie kostkami
#'
#' Funkcja reproduction() oblicza stan StadaGracza po rzucie kostkami. Uwzglednia sytuacje, ktorej na kostkach pojawia sie lis lub wilk.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param kostka1.wynik nazwa zwierzecia wylosowanego na pierwszej kostce
#' @param kostka2.wynik nazwa zwierzecia wylosowanego na drugiej kostce
#' @param max_stado maksymalna liczba zwierzat dostepna w grze, zapisana jako nazwany wektor
#' @export
#' @return funkcja zwraca StadoGracza po rzucie kostkami
#' 
#' 


reproduction = function(kostka1.wynik, kostka2.wynik, StadoGracza,max_stado= SuperFarmerPKEK::max_stado) {
    
    if (kostka1.wynik == kostka2.wynik) {
        StadoGracza[[kostka1.wynik]] = min(floor(StadoGracza[[kostka1.wynik]] + (StadoGracza[[kostka1.wynik]] + 2)/2), max_stado[[kostka1.wynik]])
    } else if (kostka1.wynik == "wilk" && kostka2.wynik == "lis") {
        if (StadoGracza[["duzy_pies"]] == 0) {
            StadoGracza[["krolik"]] = 0
            StadoGracza[["owca"]] = 0
            StadoGracza[["swinia"]] = 0
            StadoGracza[["krowa"]] = 0
        } else {
            StadoGracza[["duzy_pies"]] = StadoGracza[["duzy_pies"]] - 1
        }
        if (StadoGracza[["maly_pies"]] == 0) {
            StadoGracza[["krolik"]] = 0
        } else {
            StadoGracza[["maly_pies"]] = StadoGracza[["maly_pies"]] - 1
        }
    } else if (kostka1.wynik == "wilk") {
        if (StadoGracza[["duzy_pies"]] == 0) {
            StadoGracza[["krolik"]] = 0
            StadoGracza[["owca"]] = 0
            StadoGracza[["swinia"]] = 0
            StadoGracza[["krowa"]] = 0
        } else {
            StadoGracza[["duzy_pies"]] = StadoGracza[["duzy_pies"]] - 1
        }
        StadoGracza[[kostka2.wynik]] = min(floor(StadoGracza[[kostka2.wynik]] + (StadoGracza[[kostka2.wynik]] + 1)/2), max_stado[[kostka2.wynik]])
    } else if (kostka2.wynik == "lis") {
        if (StadoGracza[["maly_pies"]] == 0) {
            StadoGracza[["krolik"]] = 0
        } else {
            StadoGracza[["maly_pies"]] = StadoGracza[["maly_pies"]] - 1
        }
        StadoGracza[[kostka1.wynik]] = min(floor(StadoGracza[[kostka1.wynik]] + (StadoGracza[[kostka1.wynik]] + 1)/2), max_stado[[kostka1.wynik]])
    } else {
        StadoGracza[[kostka1.wynik]] = min(floor(StadoGracza[[kostka1.wynik]] + (StadoGracza[[kostka1.wynik]] + 1)/2), max_stado[[kostka1.wynik]])
        StadoGracza[[kostka2.wynik]] = min(floor(StadoGracza[[kostka2.wynik]] + (StadoGracza[[kostka2.wynik]] + 1)/2), max_stado[[kostka2.wynik]])
    }
    return(c(StadoGracza))
}
