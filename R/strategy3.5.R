#' Strategia 3, wymiana wg strategii 3 dla piątego przedzialu
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#'
#'

strategy3.5 <- function(StadoGracza) {
    # 5. Dla wartości od 105 do 118: 1 koń, mały pies i same króliki.
    
    
    if (StadoGracza["maly_pies"] == 0 && StadoGracza["krolik"] > 9) {
      StadoGracza = wymiana(StadoGracza, "krolik", "maly_pies", 6, 1)
    } else if (StadoGracza["swinia"] > 0) {
      StadoGracza = wymiana(StadoGracza, "swinia", "krolik", 1, 12)
    } else if (StadoGracza["owca"] > 0) {
      StadoGracza = wymiana(StadoGracza, "owca", "krolik", 1, 6)
    }
    return(StadoGracza)
}
