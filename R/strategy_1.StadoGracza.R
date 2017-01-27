#' Strategia 1, wymiana
#'
#' Funkcja strategy_1.StadoGracza() dokonuje wymany zwierzat zgodnie ze strategia 1 
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param max_stado wektor mowiacy ile zwierzat jest w puli
#' @param ceny_w_krolikach wektor opisujacy wartosc zwierzat wyrażona w krolikach
#' @export
#' @return funkcja zwraca StadoGracza po wymianie zwierzat zgodnie ze strategia 1
#'
#'
strategy_1.StadoGracza = function(StadoGracza, max_stado, ceny_w_krolikach) {
    nowa_kolejnosc = c(1, 2, 3, 4, 7, 5, 6)
    for (i in 6:2) {
        if (StadoGracza[nowa_kolejnosc[i]] == 0) {
            if ((sum(StadoGracza[nowa_kolejnosc[1:(i - 1)]] * ceny_w_krolikach[nowa_kolejnosc[1:(i - 1)]])) >= ceny_w_krolikach[nowa_kolejnosc[i]]) {
                StadoGracza = strategy_1_na_wieksze_zwierze(StadoGracza, max_stado, ceny_w_krolikach, i, nowa_kolejnosc)
                
            }
        }
      break
    }
    for (i in 6:2) {
        if (StadoGracza[nowa_kolejnosc[i]] > 1) {
            if (prod(StadoGracza[nowa_kolejnosc[i:6]]) > 0) {
                StadoGracza = strategy_1_na_mniejsze_zwierze(StadoGracza, max_stado, ceny_w_krolikach, i, nowa_kolejnosc)
            }
        }
    }
    return(StadoGracza)
}
