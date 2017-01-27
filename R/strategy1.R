#' Strategia 1, wymiana wg strategii 1
#'
#' Funkcja strategy1() zwraca StadoGracza po wymianie zgodniej ze strategia 1. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param max_stado maksymalna liczba zwierzat dostepna w grze, zapisana jako nazwany wektor
#' @param ceny_w_krolikach domyslne wartosci zwierzat wyrazone w krolikach, zapisane jako nazwany wektor
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#' 
#' 

strategy1 = function(StadoGracza, max_stado= SuperFarmerPKEK::max_stado, ceny_w_krolikach=SuperFarmerPKEK::ceny_w_krolikach) {
    max_stado1 = c(60, 24, 20, 12, 6, 4, 2)
    names(max_stado1) = c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies", "duzy_pies")
    max_stado1 = max_stado - StadoGracza
    StadoGracza = strategy_1.StadoGracza(StadoGracza, max_stado1, ceny_w_krolikach)
    return(c(StadoGracza))
}
