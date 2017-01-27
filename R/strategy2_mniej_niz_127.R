#' Strategia 2, strategia wymiany w stadzie o wartości mniejszej niz 127 krolkow
#'
#' Funkcja strategy2_mniej_niz_127() opisuje strategie wynian w sytuacji, gdy gracz posiada zwierzeta odpowiadajace mniej niz 127 krolikom
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartość zwierzat wyrazona w krolikach
#' @param ruch wskaznik, czy zostala zrobiona wymiana w tym kroku gry (przyjmuje wartości 0 i 1)
#' @export
#' @return funkcja zwraca StadoGracza po odpowiedniej wymianie
#' 
#' 
strategy2_mniej_niz_127 = function(StadoGracza, ceny_w_krolikach, ruch) {
    if (StadoGracza[["maly_pies"]] == 0 && StadoGracza[["krolik"]] > 9 && ruch == 0) {
        StadoGracza = strategy2.wymiana.krolik(StadoGracza, ceny_w_krolikach, "maly_pies")
        ruch = 1
    } else if (StadoGracza[["krolik"]] == 0 && StadoGracza[["maly_pies"]] > 0 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "maly_pies", "krolik")
        ruch = 1
    } else if (StadoGracza[["owca"]] == 0 && StadoGracza[["krolik"]] > 15 && ruch == 0) {
        StadoGracza = strategy2.wymiana.krolik(StadoGracza, ceny_w_krolikach, "owca")
        ruch = 1
    } else if (StadoGracza[["owca"]] > 0 && StadoGracza[["krolik"]] > 20 && StadoGracza[["swinia"]] == 0 && ruch == 0) {
        StadoGracza = strategy2.wymiana.krolik(StadoGracza, ceny_w_krolikach, "swinia")
        ruch = 1
    }
    return(c(StadoGracza))
}
