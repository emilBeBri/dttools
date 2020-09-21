
#' @import ggplot2 
#' @export
# ggplot theme 1:
theme_bb <- function() {
  ggplot2::theme(
  panel.background = element_rect(fill = 'white'),
  panel.grid.major = element_line(colour = "grey75"),
  panel.grid.minor = element_line(colour = "white"),
  plot.title = element_text(size = rel(1.5), face = "bold"),
  axis.ticks.length.y = unit(.25, "cm"),
  axis.ticks.length.x = unit(.25, "cm"),
  panel.ontop=FALSE
)
}


#' @import ggplot2 
#' @import grid 
#' @export
#  Map theme - from K Healeys book (NB! hed før bare theme_map, men vil helst ikke i konflikt med andre pakker der hedder det)
theme_map_healy <- function(base_size=9, base_family="") {
    ggplot2::theme(base_size=base_size, base_family=base_family) %+replace%
        ggplot2::theme(axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank(),
              panel.spacing=unit(0, "lines"),
              plot.background=element_blank(),
              legend.justification = c(0,0),
              legend.position = c(0,0)
              )
}