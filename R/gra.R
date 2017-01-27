#' Gra w SuperFarmera zadana strategia
#'
#' Funkcja gra() oblicza czas gry w grę SupreFarmer zadana strategia
#'
#' @param strategia strategia, ktora chcemy zagrac. W pakiecie do wyboru: strategy1, strategy2, strategy3
#' @param kostka1 zwierzeta znajdujace sie na kostce nr 1
#' @param kostka2 zwierzeta znajdujace sie na kostce nr 2
#' @param StadoGracza stan stada gracza, zapisany jako nazwany wektor
#' @return funkcja zwraca czas gry dana strategia
#'
#' @export
#'
#'

gra = function(strategia=SuperFarmerPKEK::strategy2, kostka1 = SuperFarmerPKEK::kostka1,
               kostka2 = SuperFarmerPKEK::kostka2,StadoGracza = SuperFarmerPKEK::StadoGracza) {
  
    czas = 0
    while (!czy_wygrana(StadoGracza)) {
        czas = czas + 1
        StadoGracza = strategia(StadoGracza)
        if (!czy_wygrana(StadoGracza)) {
          kostka1.wynik = sample(kostka1, 1)
          kostka2.wynik = sample(kostka2, 1)
          StadoGracza = reproduction(kostka1.wynik, kostka2.wynik, StadoGracza)}

}
    return(czas)
}
