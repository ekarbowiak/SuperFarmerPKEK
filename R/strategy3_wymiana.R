#' Wymiana
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#' 
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param co_sprzedaj ktore zwierze chcemy sprzedac
#' @param co_kup ktore zwierze chcemy kupic
#' @param ile_sprzedaj liczba zwierzat, ktore chcemy sprzedac
#' @param ile_kup liczba zwierzat, ktore chcemy kupic
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#'
#'


wymiana <- function(StadoGracza, co_sprzedaj, co_kup, ile_sprzedaj, ile_kup) {
    StadoGracza[co_kup] <- StadoGracza[co_kup] + ile_kup
    StadoGracza[co_sprzedaj] <- StadoGracza[co_sprzedaj] - ile_sprzedaj
    return(StadoGracza)
}
