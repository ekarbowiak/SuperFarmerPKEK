#' Strategia 2, strategia wymiany w stadzie o wartosci wiekszej niż 127 krolkow
#'
#' Funkcja strategy2_wiecej_niz_127 opisuje strategie wynian w sytuacji, gdy gracz posiada zwierzeta odpowiadajace wiecej niz 127 krolikom
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartosc zwierzat wyrażona w krolikach
#' @param ruch wskaźnik, czy zostala zrobiona wymiana w tym kroku gry (przyjmuje wartosci 0 i 1)
#' @export
#' @return funkcja zwraca StadoGracza po odpowiedniej wymianie
#' 
strategy2_wiecej_niz_127 = function(StadoGracza, ceny_w_krolikach, ruch) {
    if (StadoGracza[["krolik"]] == 0 && StadoGracza[["krowa"]] > 1 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "krowa", "krolik")
        ruch = 1
    } else if (StadoGracza[["owca"]] == 0 && StadoGracza[["krowa"]] > 1 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "krowa", "owca")
        ruch = 1
    } else if (StadoGracza[["swinia"]] == 0 && StadoGracza[["krowa"]] > 1 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "krowa", "swinia")
        ruch = 1
    } else if (StadoGracza[["krolik"]] == 0 && StadoGracza[["swinia"]] > 1 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "swinia", "krolik")
        ruch = 1
    } else if (StadoGracza[["owca"]] == 0 && StadoGracza[["swinia"]] > 1 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "swinia", "owca")
        ruch = 1
    } else if (StadoGracza[["krolik"]] == 0 && StadoGracza[["owca"]] > 1 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "owca", "krolik")
        ruch = 1
    } else if (StadoGracza[["owca"]] == 0 && StadoGracza[["krolik"]] > 6 && ruch == 0) {
        StadoGracza = strategy2.wymiana.krolik(StadoGracza, ceny_w_krolikach, "owca")
        ruch = 1
    } else if (StadoGracza[["swinia"]] == 0 && ruch == 0) {
        StadoGracza = strategy2_na_wieksze_zwierze(StadoGracza, ceny_w_krolikach, "swinia")
        ruch = 1
    } else if (StadoGracza["krowa"] == 0 && ruch == 0) {
        StadoGracza = strategy2_na_wieksze_zwierze(StadoGracza, ceny_w_krolikach, "krowa")
        ruch = 1
    }
    return(c(StadoGracza))
}
