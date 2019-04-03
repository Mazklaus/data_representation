

geom_levelPlot <-  function(data,x,y,z,lowCol = "pink" , highCol = "d"){
  
  require(ggplot2)
  require(scales)
  
  mid <- median(Exemple1$power)
  
  ggplot(Exemple1,aes(delta,Ntot,z=power)) +
    geom_raster(aes(fill = power)) +
    scale_fill_manual(low = "pink",mid = "orange",high = "palegreen",midpoint = 0.5) +
    # geom_contour(aes(colour = stat(level)))
    geom_contour(colour = "white")
  
  
}
