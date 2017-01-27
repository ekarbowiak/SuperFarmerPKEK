#' Strategia 3, wartosc stada gracza
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @export
#' @return funkcja zwraca wartosc stada gracza
#' 
#' 

strategy3_wartosc_stada <- function(StadoGracza) {
    wartosc_w_krolikach <- c(1, 6, 12, 36, 72, 6, 36)
    wartosc = rep(0, times = 7)
    for (i in 1:7) {
        wartosc[i] <- StadoGracza[i] * wartosc_w_krolikach[i]
    }
    return(sum(wartosc))
}
