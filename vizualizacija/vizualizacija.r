



narisi.place.po.regijah <- function(place_po_regijah) {
  
  imena <-unique(place_po_regijah$STATISTICNA_REGIJA)
  for (ime in imena){
    graph1 <- ggplot(data=place_po_regijah%>%filter(STATISTICNA_REGIJA==ime,STAROST != "65 let >"),
                     aes(x=LETO,y=Moški)) +
      geom_point() +
      geom_smooth(fill="blue",
                  colour="darkblue", size=1) +
      facet_grid(.~STAROST)
    
    
    graph1 <- graph1 + geom_point(aes(x=LETO,y=Ženske)) +
      geom_smooth(aes(x=LETO,y=Ženske), fill="red",
                  colour="red", size=1,alpha=0.1) +
      facet_grid(~STAROST)
    
    graph1 <- graph1 +
      ggtitle(paste("Regija -", ime)) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = -40, vjust = 1, hjust = 0)) +
      ylab("Plača") +
      xlab("Leto")
    
    print(graph1)
  }

}



box.plot.po.regijah <- function(place_po_regijah,spol="Moški") {
      color = "blue"
      aes1 = aes(x=STATISTICNA_REGIJA, y=Moški,color=STAROST)
      if (spol == "Moški"){
        aes1 = aes(x=STATISTICNA_REGIJA, y=Moški,color=STAROST)
      }else {
        aes1 = aes(x=STATISTICNA_REGIJA, y=Ženske,color=STAROST)
        color = "red"
      }
      
      place_po_regijah%>%filter(STAROST != "65 let >",STAROST != "15-64 let") %>%
        ggplot(aes1) + 
        geom_jitter() + 
        geom_boxplot(color=color,fill=color, alpha=I(0.01)) +
        theme_bw()+
        ggtitle("Razpršenost plač po regijah")+
        theme(plot.title = element_text(hjust = 0.5, colour = "darkblue"),
              axis.text.x = element_text(angle = -20, vjust = 1, hjust = 0)) +
        ylab("Plača") +
        xlab("Statistična regija")
}
box.plot.po.regijah(place_po_regijah)



placa_po_izobrazbi <- function(place_po_sektorju) {
  place_po_sektorju %>% 
    filter(SEKTOR != "1 Javni in zasebni sektor - SKUPAJ") %>%
    ggplot(aes(x=IZOBRAZBA,y=POVPRECNA_PLACA)) +
    geom_jitter() +
    geom_boxplot(color="blue",fill="blue",size=1,alpha=I(0.3)) +
    theme_bw()+ 
    ggtitle("Razpršenost plač po izobrazbi")+
    theme( plot.title = element_text(hjust = 0.5)) +
    ylab("Plača") +
    xlab("Izobrazba")
    
}



delez.zaposlenih.v.sektorjih.po.placah <- function(place_po_sektorju,placa=2000) {

  place_po_sektorju %>% 
    filter(SEKTOR != "1 Javni in zasebni sektor - SKUPAJ",
           POVPRECNA_PLACA > placa,
           SEKTOR != "11 Javni sektor - SKUPAJ") %>% # samo visokošolska izobrazba
    ggplot(aes(x=SPOL, fill=factor(SEKTOR))) + 
    geom_bar(position="fill",color="black",alpha=I(0.8))+
    theme_bw() +
    ggtitle("Delež zaposlenih v posameznih sektorjih")+
    theme( plot.title = element_text(hjust = 0.5),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black")) +
    ylab("Delež") +
    xlab("Spol") +
    guides(fill=guide_legend("SEKTOR"))
    
}



placa_po_sektorjih <- function(place_po_sektorju) {
  
  place_po_sektorju %>% 
    filter(SEKTOR != "1 Javni in zasebni sektor - SKUPAJ",
           IZOBRAZBA != "Izobrazba - Skupaj",
           SPOL != "Spol - SKUPAJ") %>%
    ggplot(aes(x=SEKTOR,y=POVPRECNA_PLACA,color=IZOBRAZBA)) +
    geom_jitter() +
    geom_boxplot(color="orange",fill="orange",size=1,alpha=I(0.1)) +
    theme_bw()+ 
    ggtitle("Razpršenost plač po sektorjih",)+
    theme( plot.title = element_text(hjust = 0.5),
           axis.text.x = element_text(angle = -10, vjust = 1, hjust = 0.1),
           legend.background = element_blank(),
           legend.box.background = element_rect(colour = "black")) +
    ylab("Plača") +
    xlab("Sektor")
}

