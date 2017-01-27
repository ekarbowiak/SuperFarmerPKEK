#' @title Wizytowka MEWA
#' 
#' @description Wizytowka danej strategii. Funkcja automatycznie generuje wykresy gestosci i dystrybuant 
#' oraz tabelki zawierajace podstawowe statystyki. Kazdy z powyzszych elementow wykorzystuje zarowno nasza 
#' strategie jak i strategie najlepsza (/code{SuperFarmerDA::strategia_maxrabbit}) oraz najgorsza 
#' (/code{SuperFarmerRCNK::strategia_yolo}).
#' 
#' @param a1 wektor czasow dla strategii SuperFarmerDA::strategia_maxrabbit
#' @param a33 wektor czasow dla strategii SuperFarmerRCNK::strategia_yolo
#' @param strategia Strategia, dla ktorej ma zostac wygenerowana wizytowka.
#' @param name Nazwa strategii (domyslnie jest to nazwa zmiennej, w ktorej przekazana jest zmienna).
#' 
#' @rdname wizytowkaMEWA
#' 
#' @author Ewelina Karbowiak, Marek Wawreniuk
#'
#' @export


wizytowkaMEWA <- function(strategia, name = as.vector(deparse(substitute(strategia)))[1],a1=SuperFarmerPKEK::a1,a33=SuperFarmerPKEK::a33) {    
  pdf(paste0(getwd(),"/wizytowkaMEWA_", name, ".pdf"), width=8.5, height=5,onefile=T)
  
  #a1<-SuperFarmerPKEK::badaj_gre2(SuperFarmerDA::strategia_maxrabbit) # wektor a1 znajduje siê w danych, dostêpnuch w pakiecie
  a2<-SuperFarmerPKEK::badaj_gre2(strategia)
  #a33<-SuperFarmerPKEK::badaj_gre2(SuperFarmerRCNK::strategia_yolo) #wektor a33 znajduje siê w danych, dostêpnuch w pakiecie
 
  kolory<-c("#006400", "#BB0000", "#00008B")
  kolory2<-	c("#8A8314" ,	"#A85A00",	"#885A58")
  
  tt3<-ttheme_default(base_size = 5, core=list(
    fg_params=list(fontface=c(rep("plain", 4), "bold.italic")),
    bg_params = list(fill=kolory2)
  ))
  
  
  astrategy1 <- data.frame(rep("SuperFarmerDA::strategia_maxrabbit", length(a1)), a1)
  colnames(astrategy1) <- c("strategia", "Czas_gry")
  astrategy2 <- data.frame(rep(name, length(a2)), a2)
  colnames(astrategy2) <- c("strategia", "Czas_gry")
  astrategy3 <- data.frame(rep("SuperFarmerRCNK::strategia_yolo", length(a33)), a33)
  colnames(astrategy3) <- c("strategia", "Czas_gry")
  strat <- rbind(astrategy1,astrategy2,astrategy3)
  
  
  plot1<-ggplot(data=strat,aes(x=Czas_gry, group=strategia, fill=strategia))+
    scale_colour_continuous(guide = FALSE) +
    geom_density(adjust=1.5 , alpha=0.5)+labs(title="Rozklad czasu gry")+ theme_set(theme_bw()+
                                                                                      theme(legend.position = c(0.4, 0.9),
                                                                                            legend.justification = c("left", "top"),
                                                                                            plot.title=element_text(colour="#3F1810", face="bold"),
                                                                                            axis.text = element_text(colour = "#3F1810"),
                                                                                            axis.title.x = element_text(colour = "#3F1810", face="bold"),
                                                                                            axis.title.y = element_text(colour = "#3F1810", face="bold"),
                                                                                            legend.text=element_text(size=rel(0.6)),
                                                                                            legend.key.size=unit(0.6, "lines"),
                                                                                            legend.title=element_text(size=rel(0.7), face="bold", hjust=0),
                                                                                            panel.background = element_rect(fill=NA),
                                                                                            panel.border = element_rect(colour = "#3F1810"),
                                                                                            panel.grid.minor.y = element_line(colour = "white"),
                                                                                            panel.grid.major = element_line(colour = "white"),
                                                                                            plot.background = element_rect(fill="NA"),
                                                                                            legend.key = element_blank(),
                                                                                            legend.background=element_rect(fill="NA", colour=NA)))+
    scale_fill_manual(values=kolory, name="Strategie")+
    guides(fill=guide_legend(nrow=3,byrow=TRUE))
  
  
  plot2<- ggplot(as.data.frame(strat),aes(Czas_gry, colour=strategia))+ stat_ecdf(geom = "step")+
    scale_color_manual(values=kolory2)+
    theme(legend.position = "none",
          plot.title=element_text(colour="#3F1810", face="bold"),
          axis.text = element_text(colour = "#3F1810"),
          axis.title.x = element_text(colour = "#3F1810", face="bold"),
          axis.title.y = element_text(colour = "#3F1810", face="bold"),
          panel.background = element_rect(fill=NA),
          panel.border = element_rect(colour = "#3F1810"),
          panel.grid.minor.y = element_line(colour = "white"),
          panel.grid.major = element_line(colour = "white"),
          plot.background = element_rect(fill="NA"))+
    labs(title="Dystybuanta")
  
  podsumowanie <- data.frame(rbind(summary(a1),summary(a2),summary(a33)))
  rownames(podsumowanie)<-paste(c("strategia_maxrabbit", name ,"strategia_yolo"))
  table2<- tableGrob( podsumowanie,theme=tt3)
  
  
  decyle<-as.data.frame(rbind(quantile(a1, prob = seq(0, 1, length = 11), type = 5),
                              quantile(a2, prob = seq(0, 1, length = 11), type = 5),
                              quantile(a33, prob = seq(0, 1, length = 11), type = 5)))
  rownames(decyle)<-paste(c("strategia_maxrabbit", name, "strategia_yolo"))
  table1<- tableGrob(decyle,theme=tt3)
  
  
  title <- textGrob(paste("\n\n WIZYTOWKA \n", name, sep = " "),just = "center", gp=gpar(fontsize=16, col="#3F1810",fontface="bold"))
  text1<- textGrob(paste("Autorzy wizytowki: Ewelina Karbowiak, Marek Wawreniuk
                         \n \n  Po prawej stonie mamy wkres gestosci czasu gry i dystrybuant2 . \n W ponizszych tabelkach umieszczone sa podstawowe statystyki i decyle \n dla badanej strategii zestawione z najlepsza i najgorsza strategia,\n ktorymi sa odpowiednio SuperFarmerDA::strategia_maxrabbit \n i SuperFarmerRCNK::strategia_yolo", sep = " "),
                   gp=gpar(fontsize=7, col="#3F1810"))
  
  lay <- rbind(c(1,3),
               c(2,3),
               c(4,6),
               c(5,6))
  
  grid.draw(grobTree(rectGrob(gp=gpar(fill="#FFB90F", lwd=0)), grid.arrange(title, text1, plot1,table2, table1,plot2, layout_matrix = lay)
  ))
  
  dev.off()
}