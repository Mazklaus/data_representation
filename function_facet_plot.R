##### function declaration ####

## function color header of facet_wrap ggplot.
facet_color_header <- function(data, facet_variable, seperate_variable, aes_x, aes_y, aes_col = NULL, legend = TRUE, scale_freedom = c("fixed","free","free_y","free_x")){
  
  ## label recuperation
  
  if(is.character(aes_x)){name_x <- aes_x} else {name_x <- colnames(data)[aes_x]}
  if(is.character(aes_y)){name_y <- aes_y} else {name_y <- colnames(data)[aes_y]}
  if(is.character(aes_col)){name_col <- aes_col} else {name_col <- colnames(data)[aes_col]}
  
  ## graph maker
  
  graph_skeleton = ggplot(data = data, aes(data[,aes_x],data[,aes_y], col=data[,aes_col])) + 
    geom_point(col = "black") +
    geom_smooth(method = "lm") +
    facet_wrap(~data[,facet_variable],scales = scale_freedom) +
    xlab(name_x) +
    ylab(name_y) +
    labs(color = name_col) +
    theme_minimal()
  
  dummy <- ggplot(data = data, aes(data[,aes_x],data[,aes_y],col=data[,aes_col])) +
    facet_wrap(~data[,facet_variable],scales = scale_freedom) + 
    geom_rect(aes(fill=data[,seperate_variable]), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    xlab(name_x) +
    ylab(name_y) +
    labs(color = name_col) +
    theme_minimal()
  
  # obscur portion of creation
  
  g1 <- ggplotGrob(graph_skeleton)
  g2 <- ggplotGrob(dummy)
  
  gtable_select <- function (x, ...) 
  {
    matches <- c(...)
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    x
  }
  
  panels <- grepl(pattern="panel", g2$layout$name)
  strips <- grepl(pattern="strip_t", g2$layout$name)
  g2$layout$t[panels] <- g2$layout$t[panels] - 1
  g2$layout$b[panels] <- g2$layout$b[panels] - 1
  
  new_strips <- gtable_select(g2, panels | strips)
  
  gtable_stack <- function(g1, g2){
    g1$grobs <- c(g1$grobs, g2$grobs)
    g1$layout <- transform(g1$layout, z= z-max(z), name="g2")
    g1$layout <- rbind(g1$layout, g2$layout)
    g1
  }
  
  ## ideally you'd remove the old strips, for now they're just covered
  new_plot <- gtable_stack(new_strips, g1)
  grid.newpage()
  grid.draw(new_plot)
  
}