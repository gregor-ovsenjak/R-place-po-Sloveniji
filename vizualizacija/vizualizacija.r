require("ggplot2")
require("dplyr")
require("readr")

place_po_regijah <- read.csv2("./podatki/Pocisceni_podatki/place_po_regijah.csv")
place_po_regijah %>% select(-1) %>% View

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
