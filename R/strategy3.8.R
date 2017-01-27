#' Strategia 3, wymiana wg strategii 3 dla osmego przedzialu
#'
#' Funkcja strategy3() zwraca StadoGracza po wymianie zgodniej ze strategia 3. Zawiera zdefiniowane wektory ceny_w_'krolik'ach i max_stado.
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartosc zwierzat wyrazona w krolikach
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#'
#'

strategy3.8 <- function(StadoGracza, ceny_w_krolikach) {
    # Gracz może kilka zwierząt zamienić na jedno zwierzę. Może też jedno zwierzę wymienić na kilka zwierząt.
    if ((StadoGracza[["krolik"]] == 0 || StadoGracza[["owca"]] == 0 || StadoGracza[["swinia"]] == 0) && StadoGracza[["krowa"]] > 1) {
      StadoGracza = kup_kilka_tanszych(StadoGracza, "krowa", ceny_w_krolikach)
    } else if ((StadoGracza[["krolik"]] == 0 || StadoGracza[["owca"]] == 0) && StadoGracza[["swinia"]] > 1) {
      StadoGracza = kup_kilka_tanszych(StadoGracza, "swinia", ceny_w_krolikach)
    } else if (StadoGracza[["krolik"]] == 0 && StadoGracza[["owca"]] > 1) {
      StadoGracza = kup_kilka_tanszych(StadoGracza, "owca", ceny_w_krolikach)
    } else if (StadoGracza[["owca"]] == 0 && StadoGracza[["krolik"]] > 6) {
      StadoGracza = sprzedaj_kilka_tanszych(StadoGracza, "owca", ceny_w_krolikach)
    } else if (StadoGracza[["swinia"]] == 0) {
      StadoGracza = sprzedaj_kilka_tanszych(StadoGracza, "swinia", ceny_w_krolikach)
    } else if (StadoGracza[["krowa"]] == 0) {
      StadoGracza = sprzedaj_kilka_tanszych(StadoGracza, "krowa", ceny_w_krolikach)
    }
    return(StadoGracza)
}
