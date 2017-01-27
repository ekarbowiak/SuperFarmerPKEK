#' Strategia 3, wymiana wg strategii 3
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach domyslne wartosci zwierzat wyrazone w krolikach, zapisane jako nazwany wektor
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#' 
#' 

strategy3 = function(StadoGracza,ceny_w_krolikach=SuperFarmerPKEK::ceny_w_krolikach) {
    StadoGracza = strategy3.StadoGracza(StadoGracza, ceny_w_krolikach)
    return(c(StadoGracza))
}
