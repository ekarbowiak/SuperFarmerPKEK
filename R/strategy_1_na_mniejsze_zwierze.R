#' Strategia 1, wymiana na mniejsze zwierze
#'
#' Funkcja strategy_1_na_mniejsze_zwierze() wymiania i-te zwierze 
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param max_stado wektor mowiacy ile zwierzat jest w puli
#' @param ceny_w_krolikach wektor opisujacy wartosc zwierzat wyrażona w krolikach
#' @param i wspołrzedna, ktora kresla położenie zwierzecia, ktore chcemy wymienic
#' @param nowa_kolejnosc kolejnosc uproszczajaca fukcje
#' @export
#' @return funkcja zwraca StadoGracza po zamianie na mniejsze zwierze 
#'
#'
strategy_1_na_mniejsze_zwierze = function(StadoGracza, max_stado, ceny_w_krolikach, i, nowa_kolejnosc) {
    StadoGracza[nowa_kolejnosc[i]] = StadoGracza[nowa_kolejnosc[i]] - 1
    max_stado[nowa_kolejnosc[i]] = max_stado[nowa_kolejnosc[i]] + 1
    for (j in (i - 1):1) {
        StadoGracza[nowa_kolejnosc[j]] = StadoGracza[nowa_kolejnosc[j]] + 1
        max_stado[nowa_kolejnosc[j]] = max_stado[nowa_kolejnosc[j]] - 1
    }
    return(c(StadoGracza))
}
