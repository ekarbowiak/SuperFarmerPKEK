#' Rozklad czasu gry w SuperFarmera zadana strategia
#'
#' Funkcja badaj_gre2() powtarza gre dana strategia 10000 razy.
#' 
#'
#' @param strategia strategia, ktora chcemy zagrac. W pakiecie do wyboru: strategy1, strategy2, strategy3
#' @return funkcja zwraca wektor, w którym każda współrzędna jest czasem gry zadana strategia
#'
#' @export
#'
#'



badaj_gre2 = function(strategia) {
  a = numeric(10000)
  a = replicate(10000, gra(strategia))
  return((a))
}
