#' Strategia 2, wymiana kroliki na wieksze zwierze
#'
#' Funkcja strategy2.wymiana.krolik() wymiania kroliki na dane wieksze zwierze 
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartość zwierzat wyrazona w krolikach
#' @param zwierze nazwa zwierzecia, ktore chcemy otrzymać
#' @export
#' @return funkcja zwraca StadoGracza po wymianie krolikow
#' 
#' 
strategy2.wymiana.krolik = function(StadoGracza, ceny_w_krolikach, zwierze) {
    StadoGracza[zwierze] = StadoGracza[zwierze] + 1
    StadoGracza["krolik"] = StadoGracza["krolik"] - ceny_w_krolikach[[zwierze]]
    return(c(StadoGracza))
}
