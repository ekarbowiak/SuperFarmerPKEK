#' Strategia 3, wymiana wg strategii 3 dla drugiego przedzialu
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_['krolik']ach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#'
#'

strategy3.2 <- function(StadoGracza) {
    # 2. Dla wartości od 33 do 46: mały pies i same króliki.
    
    if (StadoGracza["maly_pies"] == 0 && StadoGracza[["krolik"]] > 9) {
      StadoGracza = wymiana(StadoGracza, 1, 6, 6, 1)
    } else if (StadoGracza[["swinia"]] > 0) {
      StadoGracza = wymiana(StadoGracza, 3, "krolik", 1, 12)
    } else if (StadoGracza[["owca"]] > 0) {
      StadoGracza = wymiana(StadoGracza, "owca", "krolik", 1, 6)
    }
    return(c(StadoGracza))
}
