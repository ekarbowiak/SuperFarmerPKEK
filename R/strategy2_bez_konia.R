#' Strategia 2, strategia wymiany w stadzie bez konia
#'
#' Funkcja strategy2_bez_konia() opisuje strategie wynian w sytuacji, gdy gracz nie posiada żadnego konia
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartosc zwierzat wyrażona w krolikach
#' @param wartosc_zagrody liczba wyrażajaca wartosc stada w krolikach
#' @param ruch wskaźnik, czy zostala zrobiona wymiana w tym kroku gry (przyjmuje wartosci 0 i 1)
#' @export
#' @return funkcja zwraca StadoGracza po odpowiedniej wymianie
#' 
#' 
strategy2_bez_konia = function(StadoGracza, ceny_w_krolikach, wartosc_zagrody, ruch) {
    if (wartosc_zagrody >= 72 && ruch == 0) {
        StadoGracza = strategy2_na_wieksze_zwierze(StadoGracza, ceny_w_krolikach, "kon")
        ruch = 1
    } else if (StadoGracza[["maly_pies"]] == 0 && StadoGracza[["krolik"]] > 9 && ruch == 0) {
        StadoGracza = strategy2.wymiana.krolik(StadoGracza, ceny_w_krolikach, "maly_pies")
        ruch = 1
    } else if (StadoGracza[["maly_pies"]] == 1 && StadoGracza[["krolik"]] > 25 && ruch == 0) {
        StadoGracza = strategy2.wymiana.krolik(StadoGracza, ceny_w_krolikach, "maly_pies")
        ruch = 1
    } else if (StadoGracza[["krolik"]] < 27 && StadoGracza[["swinia"]] > 0 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "swinia", "krolik")
        ruch = 1
    } else if (StadoGracza[["krolik"]] < 33 && StadoGracza[["owca"]] > 0 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "owca", "krolik")
        ruch = 1
    } else if (StadoGracza[["krolik"]] == 0 && StadoGracza[["maly_pies"]] > 0 && ruch == 0) {
        StadoGracza = strategy2.zwierze1_na_mniejsze_zwierze2(StadoGracza, ceny_w_krolikach, "maly_pies", "krolik")
        ruch = 1
    } else if (StadoGracza[["krolik"]] <= 45 && StadoGracza[["krolik"]] > 39 && StadoGracza[["owca"]] < 23 && ruch == 0) {
        StadoGracza = strategy2.wymiana.krolik(StadoGracza, ceny_w_krolikach, "owca")
        ruch = 1
    } else if (StadoGracza[["krolik"]] >= 46 && StadoGracza[["swinia"]] < 20 && ruch == 0) {
        StadoGracza = strategy2.wymiana.krolik(StadoGracza, ceny_w_krolikach, "swinia")
        ruch = 1
    }
    return(c(StadoGracza))
}
