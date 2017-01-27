#' Strategia 3, wymiana wg strategii 3 dla szostego przedzialu
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_1ach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#'
#'

strategy3.6 <- function(StadoGracza) {
    # 6. Dla 119 do 127: 1 koń, mały pies, reszta świnie i owce.
    
    if (StadoGracza[6] == 0 && StadoGracza[1] > 9) {
      StadoGracza = wymiana(StadoGracza, 1, 6, 6, 1)
    } else if (StadoGracza[1] == 0 && StadoGracza[6] > 0) {
      StadoGracza = wymiana(StadoGracza, 6, 1, 1, 6)
    } else if (StadoGracza[2] == 0 && StadoGracza[1] > 6) {
      StadoGracza = wymiana(StadoGracza, 1, 2, 6, 1)
    } else if (StadoGracza[3] == 0 && StadoGracza[1] > 12) {
      StadoGracza = wymiana(StadoGracza, 1, 3, 12, 1)
    }
    return(StadoGracza)
}
