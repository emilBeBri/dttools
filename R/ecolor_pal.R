#' output colors so that design color choices can be made 
#'
#' @description This function displays colors so that choices can be made 
#'
#' @param colorx colors to display 
#' @param background the color of the background in the plot 
#' @param size size of label of the color name (if the color has a name in the base-R color() function) 
#'
#' @import data.table
#' @import ggplot2
#' @importFrom grDevices colors
#' 
#'
#' @return This function returns \code{the url} blah blah blah
#' @examples
#'\dontrun{
#' ecolor_pal(ecolor_data[grp %in% 'paul tol 14', hex])
#' ${1:}
#'}
#' @export

# saet ind her som svar naar du er faerdig.
# https://stackoverflow.com/questions/25726276/visualize-a-list-of-colors-palette-in-r

# ikke parat endnu
ecolor_pal <- function(colorx, background = "light gray", size=16) {
    # colorx <- c('red', 'green',' blue')
    # colorx <- ecolor_data[color %ilike% 'red', hex]
    # size=16
    # background <- "light gray"

  #   no <- var1 <- NULL # programming with data.table

  # # errorcheck
  # if( !is.vector(colorx)) stop('this is not a vector')

  # # to data.table
  # DT1 <- data.table(colorx)[1:7]
  # DT1[, no := 1:.N][, var1 := 1]
  # DT1[, colorx := factor(colorx)]

  # # er navnet paa hex-vaerdien i base-Rs farveregister? i saa fald giv navn efter farven istede for hex-value
  # # #OBS# Nej, ikke alligevel - fordi flere farver kan godt have samme hex vaErdi. Saa skulle man displaye alle de forskellie navne, en vaerdi havde... det kunne man ogsaa godt. Men nu er du for traet, saa lad vaer. Tue Jan 19 18:56:40 2021 #todo#
  # # test1 <- DT1[colorx %in% ecolor_data$hex]
  # # if(nrow(test1)==nrow(DT1)){

  # #   DT1[ecolor_data, ('e_labels') := color, on=c(colorx='hex')]
  # #   e_labels <- DT1$e_labels
  # # ecolor_data[color %ilike% 'red']
  # # dups(ecolor_data, by='hex')

  # # } else 

  # e_labels <- 1:nrow(DT1)
  # e_labels <- DT1$colorx

  # DT1

  # ecolor_data[color %ilike% 'red', ]


  # p1 <-  ggplot(DT1, aes(no, var1,fill=colorx)) + geom_col(show.legend=F) + scale_fill_manual(values=colorx)  + scale_y_continuous(expand = expansion(mult = c(0, .1)))


  # # theming
  # p2 <- p1 + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),axis.text.x = element_text(face = "bold", color = "black", size = size), 
  #   panel.border = element_blank(),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   plot.background = element_rect(fill = background),
  #   panel.background = element_rect(fill = background,
  #     colour = background))



  # p3 <- p2 + scale_x_continuous(breaks=1:nrow(DT1),labels=e_labels)

  # # adjust angle for readability if there are many colors
  # if(nrow(DT1) > 14 ) p3 <- p3 + theme(axis.text.x = element_text(angle = 40, hjust = 1))

  # print(p3) # output}

  # ecolor_pal('#8B2252')

}

