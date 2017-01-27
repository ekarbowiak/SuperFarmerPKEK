#' Strategia 3, wymiana wg strategii 3 dla trzeciego przedzialu
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#'
#'

strategy3.3 <- function(StadoGracza) {
    # 3. Od 47 do 71: do 40 królików, mały pies, reszta świnie i owce.
    
    
    if (StadoGracza["maly_pies"] == 0 && StadoGracza["krolik"] > 6) {
      StadoGracza = wymiana(StadoGracza, "krolik", "maly_pies", 6, 1)
    } else if (StadoGracza["swinia"] < floor((StadoGracza["krolik"] - 34)/12)) {
      StadoGracza = wymiana(StadoGracza, "krolik", "swinia", 12, 1)
    } else if (StadoGracza["owca"] < floor((StadoGracza["krolik"] - 35)/6)) {
      StadoGracza = wymiana(StadoGracza, "krolik", "owca", 6, 1)
    }
    
    
    # Kupujemy pierwszego konia
    if (strategy3_wartosc_stada(StadoGracza) >= 72 && StadoGracza["kon"] == 0) {
      StadoGracza = sprzedaj_kilka_tanszych(StadoGracza, "kon")
    }
    return(StadoGracza)
}
