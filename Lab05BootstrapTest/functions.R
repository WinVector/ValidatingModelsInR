


scattercrossbar = function(aucf, boundsf, title) {
  ggplot() + 
    geom_crossbar(data=boundsf, aes(x=model, y=AUC, ymin=lower, ymax=upper, 
                                    color=model, fill=model), alpha=0.5, width=0.2) + 
    geom_point(data=aucf, aes(x=model, y=AUC), 
               position=position_jitter(width=0.1, height=0),
               alpha=0.3) + 
    scale_color_brewer(palette="Dark2") + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    coord_flip()
}
