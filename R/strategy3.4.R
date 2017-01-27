#' Strategia 3, wymiana wg strategii 3 dla trzeciego przedzialu
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_krolikach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartosc zwierzat wyrazona w krolikach
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#'
#'

strategy3.4 <- function(StadoGracza, ceny_w_krolikach) {
    # Od 72 postępujemy tak samo, z tym, że w zagrodzie mamy już konia. 4. Do wartości zagrody 105 trzymamy same króliki i
    # 1 konia
  if (StadoGracza["kon"] == 0)
  {
    StadoGracza = sprzedaj_kilka_tanszych(StadoGracza, "kon", ceny_w_krolikach)
  }
    
   else if (StadoGracza["swinia"] > 0) {
      StadoGracza = wymiana(StadoGracza, "swinia", "krolik", 1, 12)
    } 
  else if (StadoGracza["owca"] > 0) {
      StadoGracza = wymiana(StadoGracza, "owca", "krolik", 1, 6)
    }
    return(StadoGracza)
}
