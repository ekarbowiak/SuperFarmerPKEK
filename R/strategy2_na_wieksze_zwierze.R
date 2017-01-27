#' Strategia 2, wymiana na wieksze zwierze
#'
#' Funkcja strategy_2_na_wieksze_zwierze() wymiania zwierze 'zwierze'
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartość zwierzat wyrazona w krolikach
#' @param zwierze nazwa zwierzecia, ktore chcemy otrzymać
#' @export
#' @return funkcja zwraca StadoGracza po zamianie na wieksze zwierze 
#'
#'
strategy2_na_wieksze_zwierze = function(StadoGracza, ceny_w_krolikach, zwierze) {
    nowa_kolejnosc2 = c(1, 6, 2, 3, 4, 7, 5)
    i = which(names(StadoGracza[nowa_kolejnosc2]) == zwierze)
    if ((sum(StadoGracza[nowa_kolejnosc2[1:(i - 1)]] * ceny_w_krolikach[nowa_kolejnosc2[1:(i - 1)]])) >= ceny_w_krolikach[nowa_kolejnosc2[i]]) {
        StadoGracza[nowa_kolejnosc2[i]] = StadoGracza[nowa_kolejnosc2[i]] + 1
        dosplaty = ceny_w_krolikach[nowa_kolejnosc2[i]]
        j = (i - 1)
        while (dosplaty > 0) {
            if (StadoGracza[nowa_kolejnosc2[j]] >= 1) {
                dosplaty = (dosplaty - ceny_w_krolikach[nowa_kolejnosc2[j]])
                StadoGracza[nowa_kolejnosc2[j]] = StadoGracza[nowa_kolejnosc2[j]] - 1
            }
            if (StadoGracza[nowa_kolejnosc2[j]] == 0) {
                j = (j - 1)
            }
        }
    }
    return(c(StadoGracza))
}
