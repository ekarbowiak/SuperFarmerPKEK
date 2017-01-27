#' Rozklad czasu gry w SuperFarmera dla trzech zadanych strategii
#'
#' Funkcja porownaj_strategie przedstawia na wykresie skrzypcowym rozklad czasu gry dla trzech dowolnych strategii 
#' (także z innych pakietów), przy powtorzeniu gry 10000 razy. 
#' Domyslnie rysowane są wykresy dla trzech strategii dostepnych w pakiecie
#' @param strategy1 strategia, ktorej rozklad chcemy zobaczyc na wykresie skrzypcowym 
#' @param strategy2 strategia, ktorej rozklad chcemy zobaczyc na wykresie skrzypcowym
#' @param strategy3 strategia, ktorej rozklad chcemy zobaczyc na wykresie skrzypcowym
#' @return funkcja zwraca wykres skrzypcowy z rozkladem czasu gry
#' @export
#'
#'


porownaj_strategie=function(strategy1=SuperFarmerPKEK::strategy1,strategy2=SuperFarmerPKEK::strategy2,strategy3=SuperFarmerPKEK::strategy3){
  
  a1<-badaj_gre2(strategy1)
  a2<-badaj_gre2(strategy2)
  a3<-badaj_gre2(strategy3)
  
  astrategy1 <- data.frame(rep("strategy1", length(a1)), a1)
  colnames(astrategy1) <- c("strategia", "Czas_gry")
  astrategy2 <- data.frame(rep("strategy2", length(a2)), a2)
  colnames(astrategy2) <- c("strategia", "Czas_gry")
  astrategy3 <- data.frame(rep("strategy3", length(a3)), a3)
  colnames(astrategy3) <- c("strategia", "Czas_gry")
  strat <- rbind(astrategy1,astrategy2,astrategy3)
  
  ggplot2:: ggplot(strat, ggplot2::aes(x = strat$strategia, y = strat$Czas_gry, fill = strat$strategia)) + ggplot2:: geom_violin() + ggplot2:: guides(fill = FALSE) +
    ggplot2:: ggtitle("Rozklad czasu gry dla danych strategii") + ggplot2:: stat_summary(fun.y = "mean", geom = "point", size = 4) 
}