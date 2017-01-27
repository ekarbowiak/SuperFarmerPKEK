#' Strategia 3, wymiana wg strategii 3 dla siodmego przedzialu
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartosć zwierzat wyrażona w krolikach
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#'
#'

strategy3.7 <- function(StadoGracza, ceny_w_krolikach) {
    # 7. Dla wartości powyżej 127 robimy wymiany i kończymy grę. Ponadto, jeżeli wartość zagrody przekracza 144, to
    # kupujemy drugiego konia (pod warunkiem, że brakuje nam co najmniej 2 zwierząt), a w następnej turze zakupimy
    # brakujące zwierzęta.
    
    # kupujemy drugiego konia
    if (StadoGracza[5] < 2 #&& sum(StadoGracza[1:5] == 0) > 1#
        ) {
      StadoGracza = sprzedaj_kilka_tanszych(StadoGracza, "kon", ceny_w_krolikach)
    } else if (StadoGracza[5] > 1) {
        # sprzedajemy drugiego konia
      StadoGracza = strategy3_sprzedaj_drugiego_konia(StadoGracza, ceny_w_krolikach)
    }
    
    return(StadoGracza)
}
