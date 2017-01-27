#' Warunek wygrania w gre SuperFarmer
#'
#' Funkcja czy_wygrana() sprawdza, czy zostal spelniony warunek wygranej w grze SuperFarmer
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @return funkcja zwraca wartość logiczna TRUE, gdy gracz wygral i FALSE w przeciwnym wypadku
#'
#' @export
#'
czy_wygrana = function(StadoGracza) {
    min(StadoGracza[c(1, 2, 3, 4, 5)]) > 0
}
