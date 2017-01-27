#' Strategia 3, wymiana drugiego konia
#'
#' Funkcja strategy3_sprzedaj_drugiego_konia() wymiania drugiego konia na tansze zwierzeta
#'
#' @param StadoGracza wektor zawieracjacy liczby poszczegolnych zwierzat w stadzie gracza
#' @param ceny_w_krolikach wektor opisujacy wartoĹ›Ä‡ zwierzat wyrazona w krolikach
#' @export
#' @return funkcja zwraca StadoGracza po zamianie na wieksze zwierze 
#'
#'



strategy3_sprzedaj_drugiego_konia <- function(StadoGracza, ceny_w_krolikach) {
    do_zaplaty <- -ceny_w_krolikach[5]
    StadoGracza = StadoGracza + c(6,1,2,1,-1,0,0)
    # ilosc <- c(6, 1, 1, 1)
    # wektor_kosztu <- c(6, 6, 12, 36)
    # while (do_zaplaty < 0) {
    #     for (i in 4:1) {
    #         if (do_zaplaty < 0 && StadoGracza[i] >= ilosc[i] && do_zaplaty + wektor_kosztu[i] <= 0) {
    #             StadoGracza = wymiana(StadoGracza, 5, i, 0, ilosc[i])
    #             do_zaplaty <- do_zaplaty + wektor_kosztu[i]
    #         }
    #     }
    # }
    return(StadoGracza)
}
