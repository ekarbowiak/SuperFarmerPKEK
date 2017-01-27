#' Sprzedawanie kilku tanszych zwierzat i kupowanie drozszego.
#'
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param zwierze zwierze, ktore chcemy kupic
#' @param ceny_w_krolikach wektor opisujacy wartosć zwierzat wyrażona w krolikach
#' @export
#' @return funkcja zwraca StadoGracza po sprzedaniu tanszych zwierzat
#' 
#' 




sprzedaj_kilka_tanszych <- function(StadoGracza, zwierze, ceny_w_krolikach) {
    if (zwierze == "kon") {
      StadoGracza = StadoGracza + c(0, 0, 0, 0, 1, 0, 0)
        wektor_ograniczen <- rep(0, times = 6)
    } else {
        wektor_ograniczen <- c(1, 1, 1, 1, 1, 0)
        # wektor_ograniczen daje ograniczenia na sprzedaz zwierzat, tak aby zachowac odpowiednia ich liczbe, postac tego wektora
        # została dobrana pod nasza strategie
    }
  if (zwierze == "krowa") {
    StadoGracza = StadoGracza + c(0, 0, 0, 1, 0, 0, 0)
  }
  if (zwierze == "swinia") {
    StadoGracza = StadoGracza + c(0, 0, 1, 0, 0, 0, 0)
  }
  if (zwierze == "owca") {
    StadoGracza = StadoGracza + c(0, 1, 0, 0, 0, 0, 0)
  }
    
    do_zaplaty <- -ceny_w_krolikach[[zwierze]]
    wektor_kosztu <- c(1, 6, 12, 36, 0, 6)
    names(wektor_kosztu)=c("krolik", "owca", "swinia", "krowa", "kon", "maly_pies")
    # wektor_kosztu jest wyrazony w liczbie krolikow, np. krolik ma wartosc 1, owca 6 itd.
    for (i in c(6,4,3,2,1)) {
        while (do_zaplaty < 0 && StadoGracza[[i]] > wektor_ograniczen[[i]] && ceny_w_krolikach[[zwierze]] > wektor_kosztu[[i]]) {
            StadoGracza = wymiana(StadoGracza, i, zwierze, 1, 0)
            do_zaplaty <- do_zaplaty + wektor_kosztu[[i]]
        }
    }
    return(StadoGracza)
}
