#' Rozklad czasu gry w SuperFarmera zadana strategia
#'
#' Funkcja badaj_gre() oblicza podstawowe statystyki dotyczace czasu gry w SupreFarmera zadana strategia,
#'  po wykonianiu funkcji gra() 10000.
#'
#' @param strategia strategia, ktora chcemy zagrać. W pakiecie do wyboru: 'strategy1', 'strategy2', 'strategy3'
#' @return funkcja zwraca podstawowe statyki czasu gry dana strategia po 10000 krotnych powtorzeniu fukcji gra()
#'
#' @export
#'
#'



badaj_gre = function(strategia) {
    a = numeric(10000)
    a = replicate(10000, gra(strategia))
    return(summary(a))
}
