#' Kupowanie kilku tanszych zwierzat za drozsze.
#'
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param zwierze zwierze, ktore chcemy sprzedac
#' @param ceny_w_krolikach wektor opisujacy wartosć zwierzat wyrażona w krolikach
#' @export
#' @return funkcja zwraca StadoGracza kupieniu tanszych zwierzat
#' 
#' 






kup_kilka_tanszych <- function(StadoGracza, zwierze, ceny_w_krolikach) {
    wektor_kosztu <- c(1, 6, 12, 36, 0, 6)
    do_zaplaty <- -ceny_w_krolikach[[zwierze]]
    # droższe zwierzęta to krowa, świnia lub owca
    if (zwierze == "krowa") {
        wektor_kosztu_zwierze <- c(36, 6, 3)
    }
    if (zwierze == "swinia") {
        wektor_kosztu_zwierze <- c(12, 2, 0)
    }
    if (zwierze == "owca") {
        wektor_kosztu_zwierze <- c(6, 0, 0)
    }
    StadoGracza[[zwierze]] <- StadoGracza[[zwierze]] - 1
    for (i in c(3,2,1)) {
        while (do_zaplaty < 0 && StadoGracza[[i]] == 0 && StadoGracza[[zwierze]] > 1 && ceny_w_krolikach[[zwierze]] > wektor_kosztu[[i]]) {
           StadoGracza = wymiana(StadoGracza, zwierze, i, 0, wektor_kosztu_zwierze[i])
            do_zaplaty <- do_zaplaty + wektor_kosztu_zwierze[i] * wektor_kosztu[[i]]
        }
    }
    return(StadoGracza)
}
