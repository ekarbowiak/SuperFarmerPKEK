#' Strategia 3, wymiana wg strategii 3 dla pierwszego przedzialu
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#'
#'
strategy3.1 <- function(StadoGracza) {
    
    # 1. Do wartości zagrody gracza równej 33, trzymamy same króliki.
    
  if (StadoGracza[["swinia"]] > 0) {
    StadoGracza = wymiana(StadoGracza, 3, 1, 1, 12)
  } else if (StadoGracza["owca"] > 0) {
    StadoGracza = wymiana(StadoGracza, 2, 1, 1, 6)
  }
  return(StadoGracza)
}


