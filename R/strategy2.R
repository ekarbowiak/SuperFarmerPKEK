#' Strategia 2, wymiana wg strategii 2
#'
#' Funkcja strategy2() zwraca StadoGracza po wymianie zgodniej ze strategia 2. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach domyslne wartosci zwierzat wyrazone w krolikach, zapisane jako nazwany wektor
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#' 
#' 

strategy2 = function(StadoGracza,ceny_w_krolikach=SuperFarmerPKEK::ceny_w_krolikach) {
    StadoGracza = strategy2.StadoGracza(StadoGracza, ceny_w_krolikach)
    return(c(StadoGracza))
}
