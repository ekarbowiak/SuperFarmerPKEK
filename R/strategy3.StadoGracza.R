#' Strategia 3
#'
#' Funkcja strategy_3.StadoGracza() dokonuje wymiany zwierzat zgodnie ze strategia 3 
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartosc zwierzat wyrazona w krolikach
#' @export
#' @return funkcja zwraca StadoGracza po wymianie zwierzat zgodnie ze strategia 3

strategy3.StadoGracza = function(StadoGracza, ceny_w_krolikach) {
    wartosc_zagrody = sum(StadoGracza * ceny_w_krolikach)
    ruch = 0
    if (wartosc_zagrody <= 33 && ruch == 0) {
        StadoGracza = strategy3.1(StadoGracza)
        ruch = 1
    } else if (wartosc_zagrody < 46 && ruch == 0 && wartosc_zagrody>33) {
        StadoGracza = strategy3.2(StadoGracza)
        ruch = 1
    } else if (wartosc_zagrody < 72 && ruch == 0 && wartosc_zagrody>45) {
        StadoGracza = strategy3.3(StadoGracza)
        ruch = 1
    } else if (wartosc_zagrody < 105 && ruch == 0 && wartosc_zagrody>71) {
        StadoGracza = strategy3.4(StadoGracza, ceny_w_krolikach)
        ruch = 1
    } else if (wartosc_zagrody < 119 && ruch == 0 && wartosc_zagrody>118) {
        StadoGracza = strategy3.5(StadoGracza)
        ruch = 1
    } else if (wartosc_zagrody < 127 && ruch == 0 && wartosc_zagrody>118) {
        StadoGracza = strategy3.6(StadoGracza)
        ruch = 1
    } else if (wartosc_zagrody < 143 && ruch == 0 && wartosc_zagrody > 127) {
        StadoGracza = strategy3.8(StadoGracza, ceny_w_krolikach)
        ruch = 1
    } else if (wartosc_zagrody > 144 && ruch == 0) {
        StadoGracza = strategy3.7(StadoGracza, ceny_w_krolikach)
        ruch = 1
    }
    
    return(c(StadoGracza))
}
