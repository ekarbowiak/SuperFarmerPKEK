#' Strategia 1, wymiana na wieksze zwierze
#'
#' Funkcja strategy_1_na_wieksze_zwierze wymiania i-te zwierze 
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param max_stado wektor mowiacy ile zwierzat jest w puli
#' @param ceny_w_krolikach wektor opisujacy wartosć zwierzat wyrażona w krolikach
#' @param i wspolrzedna, ktora kresla polożenie zwierzecia, ktore chcemy otrzymać
#' @param nowa_kolejnosc kolejnosć uproszczajaca fukcje
#' @export
#' @return funkcja zwraca StadoGracza po zamianie na wieksze zwierze 
#'
#'

strategy_1_na_wieksze_zwierze = function(StadoGracza, max_stado, ceny_w_krolikach, i, nowa_kolejnosc) {
    StadoGracza[nowa_kolejnosc[i]] = 1
    max_stado[nowa_kolejnosc[i]] = max_stado[nowa_kolejnosc[i]] - 1
    dosplaty = ceny_w_krolikach[nowa_kolejnosc[i]]
    j = (i - 1)
    while (dosplaty > 0) {
        if (StadoGracza[nowa_kolejnosc[j]] >= 1) {
            dosplaty = (dosplaty - ceny_w_krolikach[nowa_kolejnosc[j]])
            StadoGracza[nowa_kolejnosc[j]] = StadoGracza[nowa_kolejnosc[j]] - 1
            max_stado[nowa_kolejnosc[j]] = max_stado[nowa_kolejnosc[j]] + 1
        }
        if (StadoGracza[nowa_kolejnosc[j]] == 0) {
            j = (j - 1)
        }
    }
    return(c(StadoGracza))
}
