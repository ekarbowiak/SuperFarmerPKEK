#' Strategia 2, strategia wymiany w stadzie, z conajmniej 2 konmi
#'
#' Funkcja strategy2_wiecej_niz_jeden_kon() opisuje strategie wynian w sytuacji, gdy gracz posiada wiecej niż jednego konia. 
#' Jest to funkcja konczaca gre
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartosc zwierzat wyrażona w krolikach
#' @param ruch wskaznik, czy została zrobiona wymiana w tym kroku gry (przyjmuje wartości 0 i 1)
#' @export
#' @return funkcja zwraca StadoGracza po odpowiedniej wymianie
#' 
#' 
strategy2_wiecej_niz_jeden_kon = function(StadoGracza, ceny_w_krolikach, ruch) {
    StadoGracza["kon"] = StadoGracza["kon"] - 1
    StadoGracza["krowa"] = StadoGracza["krowa"] + 1
    StadoGracza["swinia"] = StadoGracza["swinia"] + 2
    StadoGracza["owca"] = StadoGracza["owca"] + 1
    if (StadoGracza[["krolik"]] < 54) {
        StadoGracza["krolik"] = StadoGracza["krolik"] + 6
    } else {
        StadoGracza["owca"] = StadoGracza["owca"] + 1
    }
    return(c(StadoGracza))
}
