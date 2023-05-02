#' This function creates mediation plots. 
#' It's a work in progress.
#' @returns the mediation you asked for... in a plot. It's a ggplot2 object.
#' 
#' @param box.names is the labels you want to give to the three boxes in your mediation.
#' @param effects is the effects. This has to be passed as a four-length vector.
#' @param mediation.type is the type of mediation you want. For now, I only have regular.
#' 
#' # Indicate the packages required
pkgs <- c('ggplot2')

# Check for packages that are not installed
new.pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
# Install the ones we need
if(length(new.pkgs)) install.packages(new.pkgs)

#Load them all in
lapply(pkgs, library, character.only = TRUE)

rm(pkgs, new.pkgs)

mediation.plot <- function(
    box.names,
    effects = c('X->M', 'M->Y', 'C', "C'"),
    mediation.type = 'regular'){
  if(mediation.type != 'regular'){
    stop("Sorry, I can only handle regular ole mediation at the moment. 
         Please specify 'mediation.type' as \"regular\"")
  } else if(length(box.names) != 3){
    stop("You need to name exactly three boxes in the order 'c('X', 'M', 'Y')")
  } else if(length(effects) != 4){
    stop("You need to input exactly four effects, in the order 'c('X->M', 'M->Y', 'C', \"C'\"). 
         If you only have three effects, leave one as ' '.")
  } else{
    df <- data.frame(
      x = c(0, 7, 3.5),
      y = c(0, 0, 1)
    )
    
    # Define the plot
    plot <- ggplot2::ggplot(df) +
      ggplot2::geom_rect(aes(xmin = x - 1.25, xmax = x + 1.15, ymin = y - 0.28, ymax = y + 0.20),
                fill = "grey90", alpha = 1, color = NA) +
      ggplot2::geom_rect(aes(xmin = x - 1.2, xmax = x + 1.2, ymin = y - 0.25, ymax = y + 0.25),
                fill = 'white', color = "black") +
      ggplot2::geom_segment(aes(x = 1, y = .25, xend = 2.5, yend = .75),
                   arrow = arrow(length = unit(0.5, "cm"), type = 'closed')) +
      ggplot2::geom_segment(aes(x = 4.5, y = .75, xend = 6, yend = .25),
                   arrow = arrow(length = unit(0.5, "cm"), type = 'closed')) +
      ggplot2::geom_segment(aes(x = 1.2, y = 0, xend = 5.75, yend = 0),
                   arrow = arrow(length = unit(0.5, "cm"), type = 'closed')) +
      ggplot2::annotate("text", x = 3.5, y = .1, label = effects[3], size = 5) +
      ggplot2::annotate("text", x = 3.5, y = -.1, label = effects[4], size = 5) +
      ggplot2::annotate("text", x = c(1, 6), y = 0.5, label = effects[c(1,2)], size = 5) +
      ggplot2::annotate("text", x = df$x, y = df$y, label = box.names, size = 5) +
      ggplot2::theme_void()
    
    return(plot)
  }
}
