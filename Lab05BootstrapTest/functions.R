
plot_densities = function(frame, column, threshold, title, tail="left"){
  
  # calculate the distribution by hand
  dens = density(frame[[column]], adjust=0.5)
  densityframe = data.frame(AUC=dens$x, density=dens$y)
  
  # ecdf returns a FUNCTION that computes the empirical cdf 
  densityfun = ecdf(frame[[column]])
  
  densityframe$tail = numeric(nrow(densityframe))
  sign = ifelse(tail=="left", 1, -1)
  densityframe$tail = ifelse(sign*densityframe$AUC < sign*threshold, densityframe$density, 0)
  
  area = switch(tail,
                left = densityfun(threshold),
                right= 1- densityfun(threshold)
  )
  text = paste("tail area = ", format(area, digits=2))
  texty = 4*max(dens$y)/5
  
  ggplot() + 
    geom_line(data=densityframe, aes(x=AUC, y=density), color="darkgray") +
    geom_ribbon(data=densityframe, aes(x=AUC, ymin=0, ymax=tail), fill="darkblue", alpha=0.5) +
    geom_vline(xintercept=threshold, color="darkblue",  linetype=2) + 
    annotate("text", x=threshold+0.001, y=texty, label=text, size=5, hjust="left", vjust="bottom") + 
    ggtitle(title)
}  



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
