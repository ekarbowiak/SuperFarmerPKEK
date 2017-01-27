#' Strategia 2, wymiana jednego zwierzecia na drugie
#'
#' Funkcja strategy2.zwierze1_na_mniejsze_zwierze2() wymiania zwierze1 na zwierze2 
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartośc zwierzat wyrażona w krolikach
#' @param zwierze1 nazwa zwierzecia, ktore chcemy wymienic
#' @param zwierze2  nazwa zwierzecia, ktore chcemy otrzymac
#' @export
#' @return funkcja zwraca StadoGracza po wymianie
#' 
#' 
strategy2.zwierze1_na_mniejsze_zwierze2 = function(StadoGracza, ceny_w_krolikach, zwierze1, zwierze2) {
    StadoGracza[zwierze1] = StadoGracza[zwierze1] - 1
    StadoGracza[zwierze2] = StadoGracza[zwierze2] + (ceny_w_krolikach[[zwierze1]]/ceny_w_krolikach[[zwierze2]])
    return(c(StadoGracza))
}
