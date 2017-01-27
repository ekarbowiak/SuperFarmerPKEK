#' Strategia 2, wymiana
#'
#' Funkcja strategy_2.StadoGracza() dokonuje wymany zwierzat zgodnie ze strategia 2 
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartosc zwierzat wyrazona w krolikach
#' @export
#' @return funkcja zwraca StadoGracza po wymianie zwierzat zgodnie ze strategia 2

strategy2.StadoGracza = function(StadoGracza, ceny_w_krolikach) {
    wartosc_zagrody = sum(StadoGracza * ceny_w_krolikach)
    nowa_kolejnosc2 = c(1, 2, 4, 5, 7, 3, 6)
    ruch = 0
    
    if (StadoGracza[["kon"]] == 0) {
        StadoGracza = strategy2_bez_konia(StadoGracza, ceny_w_krolikach, wartosc_zagrody, ruch)
        ruch = 1
    } else if (StadoGracza[["kon"]] == 1) {
        if (wartosc_zagrody >= 144 && ruch == 0) {
            StadoGracza = strategy2_na_wieksze_zwierze(StadoGracza, ceny_w_krolikach, "kon")
            ruch = 1
        } else if (wartosc_zagrody >= 127 && ruch == 0) {
            StadoGracza = strategy2_wiecej_niz_127(StadoGracza, ceny_w_krolikach, ruch)
            ruch = 1
        } else if (wartosc_zagrody < 127 && ruch == 0) {
            StadoGracza = strategy2_mniej_niz_127(StadoGracza, ceny_w_krolikach, ruch)
            ruch = 1
        }
    } else if (StadoGracza[["kon"]] > 1 && ruch == 0) {
        StadoGracza = strategy2_wiecej_niz_jeden_kon(StadoGracza, ceny_w_krolikach, ruch)
        ruch = 1
    }
    return(c(StadoGracza))
}
