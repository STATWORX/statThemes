#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @param dark choose dark mode
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_classic()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
statworx_classic <- function(font = "Arial", dark = FALSE){

  if (dark == TRUE){

    plot.background = element_rect(fill = "#000000")
    panel.background = element_rect(fill = "#000000")
    colour_sub = "#FFFFFF"
    legend.key = element_rect(fill = "#000000")
    legend.background = element_rect(fill= "#000000")
    legend.text = element_text(size = 12, family = font, colour = "#B6BDCC")
    legend.title = element_text(size = 12, family = font, colour = "#B6BDCC")
    axis.line.x.bottom=element_line(color="#B6BDCC")
    axis.line.y = element_line(color="#B6BDCC")
    color_gen = "#B6BDCC"
    color_axis = "#B6BDCC"
    axis_title = "#B6BDCC"


  } else {

    plot.background = element_rect(fill = "#FFFFFF")
    panel.background = element_rect(fill = "#FFFFFF")
    colour_sub = "#000000"
    legend.key = element_blank()
    legend.background = element_rect(fill= "#FFFFFF")
    axis.line.x.bottom = element_line(color="#283440")
    axis.line.y = element_line(color="#283440")
    legend.title = element_text(size = 12, family = font)
    legend.text = element_text(size = 12, family = font)
    color_gen = "#283440"
    color_axis = "#283440"
    axis_title = "#000000"

  }

  theme_classic() + theme(

    plot.background = plot.background,
    panel.background = panel.background,
    axis.line.x.bottom = axis.line.x.bottom,
    axis.line.y = axis.line.y,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_line(colour = color_gen),
    axis.ticks.y = element_line(colour = color_gen),

    #text elements
    plot.title = element_text(
      family = font,
      colour = "#0000FF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = colour_sub,
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 1.5),

    plot.caption = element_text(
      family = font,
      colour = color_gen,
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = color_axis,
      size = 12),

    axis.text = element_text(
      colour = color_gen,
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, b = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = legend.text,
    legend.title = legend.title,
    legend.key = legend.key,
    legend.background = legend.background
  )

}

#' statworx Theme based on theme_minimal
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_minimal}}
#' @param font set default font
#' @param dark choose dark mode
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_minimal()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
statworx_minimal <- function(font = "Arial", dark = FALSE){

  if (dark == TRUE){

    plot.background = element_rect(fill = "#000000")
    panel.background = element_rect(fill = "#000000")
    colour_sub = "#FFFFFF"
    legend.key = element_rect(fill = "#000000")
    legend.background = element_rect(fill= "#000000")
    legend.text = element_text(size = 12, family = font, colour = "#B6BDCC")
    legend.title = element_text(size = 12, family = font, colour = "#B6BDCC")
    panel.grid.major.x=element_line(color="#B6BDCC", size = 0.15)
    panel.grid.major.y = element_line(color="#B6BDCC", size = 0.15)
    color_gen = "#B6BDCC"
    color_axis = "#B6BDCC"
    axis_title = "#B6BDCC"


  } else {

    plot.background = element_blank()
    panel.background = element_blank()
    colour_sub = "#000000"
    legend.key = element_blank()
    legend.background = element_blank()
    panel.grid.major.x = element_line(color="#B6BDCC", size = 0.15)
    panel.grid.major.y = element_line(color="#B6BDCC", size = 0.15)
    legend.title = element_text(size = 12, family = font)
    legend.text = element_text(size = 12, family = font)
    color_gen = "#B6BDCC"
    color_axis = "#283440"
    axis_title = "#000000"

  }

  theme_minimal() + theme(

    #grid elements
    plot.background = plot.background,
    panel.background = panel.background,
    panel.grid.major.x = panel.grid.major.x,
    panel.grid.major.y = panel.grid.major.y,

    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(colour = color_gen, size = 0.15),
    axis.ticks.y = element_line(colour = color_gen, size = 0.15),

    #text elements
    plot.title = element_text(
      family = font,
      colour = "#0000FF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = colour_sub,
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = color_gen,
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = color_axis,
      size = 12),

    axis.text = element_text(
      colour = color_axis,
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, b = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = legend.text,
    legend.title = legend.title,
    legend.key = legend.key,
    legend.background = legend.background
  )

}

#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @param dark choose dark mode
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_scientific()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
statworx_scientific <- function(font = "Arial", dark = FALSE){

  if (dark == TRUE){

    plot.background = element_rect(fill = "#000000")
    panel.background = element_rect(fill = "#000000")
    colour_sub = "#FFFFFF"
    legend.key = element_rect(fill = "#000000")
    legend.background = element_rect(fill= "#000000")
    legend.text = element_text(size = 12, family = font, colour = "#B6BDCC")
    legend.title = element_text(size = 12, family = font, colour = "#B6BDCC")
    color_gen = "#B6BDCC"
    color_axis = "#B6BDCC"
    axis.line.x.bottom=element_line(color="#B6BDCC")
    axis.line.y = element_line(color="#B6BDCC")
    axis_title = "#B6BDCC"


  } else {

    plot.background = element_blank()
    panel.background = element_blank()
    colour_sub = "#000000"
    legend.key = element_blank()
    legend.background = element_blank()
    legend.title = element_text(size = 12, family = font)
    legend.text = element_text(size = 12, family = font)
    color_axis = "#283440"
    axis.line.x.bottom = element_line(color="#283440")
    axis.line.y = element_line(color="#283440")
    axis_title = "#000000"

  }

  theme_classic() + theme(

    #grid elements
    plot.background = plot.background,
    panel.background = panel.background,
    axis.line.x.bottom = axis.line.x.bottom,
    axis.line.y = axis.line.y,
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#B6BDCC", linetype = "dashed",
                                    size = 0.15),
    axis.ticks.x = element_line(colour = "#B6BDCC", linetype = "dashed",size = 0.15),
    axis.ticks.y = element_line(colour = "#B6BDCC", linetype = "dashed",size = 0.15),
    axis.ticks.length.x = unit(0.35, "cm"),
    axis.ticks.length.y = unit(0.35, "cm"),


    #text elements
    plot.title = element_text(
      family = font,
      colour = "#0000FF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = colour_sub,
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = color_axis,
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = color_axis,
      size = 12),

    axis.text = element_text(
      colour = color_axis,
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, b = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = legend.text,
    legend.title = legend.title,
    legend.key = legend.key,
    legend.background = legend.background
  )



}

#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @param dark choose dark mode
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_hc()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
statworx_hc <- function(font = "Arial", dark = FALSE){

  if (dark == TRUE){

    plot.background = element_rect(fill = "#000000")
    panel.background = element_rect(fill = "#000000")
    colour_sub = "#FFFFFF"
    legend.key = element_rect(fill = "#000000")
    legend.background = element_rect(fill= "#000000")
    legend.text = element_text(size = 12, family = font, colour = "#B6BDCC")
    legend.title = element_text(size = 12, family = font, colour = "#B6BDCC")
    color_axis = "#B6BDCC"
    axis_title = "#B6BDCC"


  } else {

    plot.background = element_blank()
    panel.background = element_blank()
    colour_sub = "#000000"
    legend.key = element_blank()
    legend.background = element_blank()
    legend.title = element_text(size = 12, family = font)
    legend.text = element_text(size = 12, family = font)
    color_axis = "#283440"
    axis_title = "#000000"

  }

  theme_classic() + theme(

    #grid elements
    panel.background = panel.background,
    plot.background = plot.background,

    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = .1, color = "#B6BDCC") ,
    axis.ticks.x = element_line(colour = "#B6BDCC", size = .1),
    axis.ticks.y = element_line(colour = "#B6BDCC", size = .1),
    axis.line.x.bottom = element_line(color = '#B6BDCC'),
    axis.line.y.left = element_line(color = '#B6BDCC'),
    axis.ticks.length.x = unit(0.35, "cm"),
    axis.ticks.length.y = unit(0.35, "cm"),

    #text elements
    plot.title = element_text(
      family = font,
      colour = "#0000FF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = colour_sub,
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = color_axis,
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = color_axis,
      size = 12),

    axis.text = element_text(
      colour = color_axis,
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, l = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = legend.text,
    legend.title = legend.title,
    legend.key = legend.key,
    legend.background = legend.background
  )



}
#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @param dark choose dark mode
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_hc2()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
statworx_hc2 <- function(font = "Arial", dark = FALSE){

  if (dark == TRUE){

    plot.background = element_rect(fill = "#000000")
    panel.background = element_rect(fill = "#000000")
    colour_sub = "#FFFFFF"
    legend.key = element_rect(fill = "#000000")
    legend.background = element_rect(fill= "#000000")
    legend.text = element_text(size = 12, family = font, colour = "#B6BDCC")
    legend.title = element_text(size = 12, family = font, colour = "#B6BDCC")
    color_axis = "#B6BDCC"
    axis_title = "#B6BDCC"


  } else {

    plot.background = element_blank()
    panel.background = element_blank()
    colour_sub = "#000000"
    legend.key = element_blank()
    legend.background = element_blank()
    legend.title = element_text(size = 12, family = font)
    legend.text = element_text(size = 12, family = font)
    color_axis = "#283440"
    axis_title = "#000000"

  }

  theme_classic() + theme(

    #grid elements
    panel.background = panel.background,
    plot.background = plot.background,
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(size = .1, color = "#B6BDCC") ,
    axis.ticks.x = element_line(colour = "#B6BDCC"),
    axis.ticks.y = element_line(colour = "#B6BDCC", size = .1),
    axis.line.x.bottom = element_line(colour = "#B6BDCC"),
    axis.line.y.left = element_blank(),


    #text elements
    plot.title = element_text(
      family = font,
      colour = "#0000FF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = colour_sub,
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour =color_axis,
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour =color_axis,
      size = 12),

    axis.text = element_text(
      colour = color_axis,
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, l = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = legend.text,
    legend.title = legend.title,
    legend.key = legend.key,
    legend.background = legend.background
  )


}

#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @param dark choose dark mode
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_flip()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }

statworx_flip <- function(font = "Arial", dark = FALSE){

  if (dark == TRUE){

    plot.background = element_rect(fill = "#000000")
    panel.background = element_rect(fill = "#000000")
    colour_sub = "#FFFFFF"
    legend.key = element_rect(fill = "#000000")
    legend.background = element_rect(fill= "#000000")
    legend.text = element_text(size = 12, family = font, colour = "#B6BDCC")
    legend.title = element_text(size = 12, family = font, colour = "#B6BDCC")
    color_axis = "#B6BDCC"
    axis_title = "#B6BDCC"

  } else {

    plot.background = element_blank()
    panel.background = element_blank()
    colour_sub = "#000000"
    legend.key = element_blank()
    legend.background = element_blank()
    legend.title = element_text(size = 12, family = font)
    legend.text = element_text(size = 12, family = font)
    color_axis = "#283440"
    axis_title = "#000000"
  }


  theme_classic() + theme(

    #grid elements
    panel.background = panel.background,
    plot.background = plot.background,
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(size = .1, color = "#B6BDCC") ,
    axis.ticks.x = element_line(colour = "#B6BDCC"),
    axis.ticks.y = element_line(colour = "#B6BDCC", size = .1),
    axis.line.x.bottom = element_line(colour = "#B6BDCC"),
    axis.line.y.left = element_blank(),


    #text elements
    plot.title = element_text(
      family = font,
      colour = "#0000FF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = colour_sub,
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = color_axis,
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = axis_title,
      size = 12),

    axis.text = element_text(
      colour = color_axis,
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, l = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = legend.text,
    legend.title = legend.title,
    legend.key = legend.key,
    legend.background = legend.background
  )


}

#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_bw}}
#' @param font set default font
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_box()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }

statworx_box <- function(font = "Arial"){

  theme_bw() + theme(

    #grid elements
    panel.background = element_rect(colour = "#283440", size=0.4),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_line(colour = "#283440"),
    axis.ticks.y = element_line(colour = "#283440"),


    #text elements
    plot.title = element_text(
      family = font,
      colour = "#0000FF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = "#000000",
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = "#283440",
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = "#000000",
      size = 12),

    axis.text = element_text(
      colour = "#283440",
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, b = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = element_text(size = 12, family = font),
    legend.title = element_text(size = 12, family = font)
  )



}

#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @param dark choose dark mode
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_modern()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
#'
statworx_modern <- function(font = "Arial", dark = FALSE){

  if (dark == TRUE){

    plot.background = element_rect(fill = "#000000")
    panel.background = element_rect(fill = "#000000")
    colour_sub = "#FFFFFF"
    legend.key = element_rect(fill = "#000000")
    legend.background = element_rect(fill= "#000000")
    legend.text = element_text(size = 12, family = font, colour = "#B6BDCC")
    legend.title = element_text(size = 12, family = font, colour = "#B6BDCC")
    color_axis = "#B6BDCC"
    axis_title = "#B6BDCC"
    axis.line.y.left = element_line(colour = "#B6BDCC", size = 0.3)
    axis.line.x.bottom = element_line(colour = "#B6BDCC", size = 0.3)

  } else {

    plot.background = element_blank()
    panel.background = element_blank()
    colour_sub = "#000000"
    legend.key = element_blank()
    legend.background = element_blank()
    legend.title = element_text(size = 12, family = font)
    legend.text = element_text(size = 12, family = font)
    color_axis = "#283440"
    axis_title = "#000000"
    axis.line.y.left = element_line(colour = "#283440", size = 0.3)
    axis.line.x.bottom = element_line(colour = "#283440", size = 0.3)
  }

  theme_classic() + theme(

    #grid elements
    plot.background = plot.background,
    panel.background = panel.background,
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(size = .1, color = "#B6BDCC") ,
    panel.grid.major.y = element_line(size = .1, color = "#B6BDCC") ,
    axis.ticks.x = element_line(colour = "#B6BDCC", size = .1),
    axis.ticks.y = element_line(colour = "#B6BDCC", size = .1),
    axis.line.y.left = axis.line.y.left,
    axis.line.x.bottom = axis.line.y.left,
    axis.ticks.length.x = unit(0.35, "cm"),
    axis.ticks.length.y = unit(0.35, "cm"),


    #text elements
    plot.title = element_text(
      family = font,
      colour = "#0000FF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = colour_sub,
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = "#283440",
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = axis_title,
      size = 12),

    axis.text = element_text(
      colour = color_axis,
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, l = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = legend.text,
    legend.title = legend.title,
    legend.key = legend.key,
    legend.background = legend.background
  )


}


#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_void}}
#' @param font set default font
#' @param dark choose dark mode
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_void()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
#'
#'
statworx_void <- function(font = "Arial", dark = FALSE){

  if (dark == TRUE){

    plot.background = element_rect(fill = "#000000")
    panel.background = element_rect(fill = "#000000")
    colour_sub = "#FFFFFF"
    legend.key = element_rect(fill = "#000000")
    legend.background = element_rect(fill= "#000000")
    legend.text = element_text(size = 12, family = font, colour = "#B6BDCC")
    legend.title = element_text(size = 12, family = font, colour = "#B6BDCC")
    axis_title = "#B6BDCC"

  } else {

    plot.background = element_blank()
    panel.background = element_blank()
    colour_sub = "#000000"
    legend.key = element_blank()
    legend.background = element_blank()
    legend.title = element_text(size = 12, family = font)
    legend.text = element_text(size = 12, family = font)
    axis_title = "#000000"
  }

  theme_void() + theme(

    plot.background = plot.background,
    panel.background = panel.background,
    #text elements
    plot.title = element_text(
      family = font,
      colour = "#0000FF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = colour_sub,
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = axis_title,
      size = 10,
      hjust = 0),


    plot.margin = margin(5, t = 20),


    legend.text = legend.text,
    legend.title = legend.title,
    legend.key = legend.key,
    legend.background = legend.background
  )


}



#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_dark()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
#'
statworx_dark <- function(font = "Arial"){

  theme_classic() + theme(

    #grid elements
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    # axis.title.x.bottom = element_text(colour = "#FFFFFF"),
    #  axis.title.y.left = element_text(colour = "#FFFFFF"),
    axis.ticks.x = element_line(colour = "#FFFFFF"),
    axis.ticks.y = element_line(colour = "#FFFFFF"),
    axis.line.x.bottom = element_line(colour = "#FFFFFF"),
    axis.line.y.left = element_line(colour = "#FFFFFF"),
    #  axis.text.x.bottom = element_text(colour = "#FFFFFF"),
    #  axis.text.y.left = element_text(colour = "#FFFFFF"),

    #text elements
    plot.title = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = "#FFFFFF",
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 12),

    axis.text = element_text(
      colour = "#FFFFFF",
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, l = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = element_text(size = 12, family = font, colour = "#FFFFFF"),
    legend.title = element_text(size = 12, family = font, colour = "#FFFFFF"),
    legend.key = element_rect(fill = "#000000"),
    legend.background = element_rect(fill= "#000000")
  )



}

#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_dark2()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
#'
statworx_dark2 <- function(font = "Arial"){

  theme_classic() + theme(

    #grid elements
    plot.background = element_rect(fill = "#000000"),
    panel.background = element_rect(fill = "#000000"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#B6BDCC", size = 0.1),
    panel.grid.major.y = element_line(colour = "#B6BDCC", size = 0.1),
    axis.ticks.x = element_line(colour = "#B6BDCC", size = 0.1),
    axis.ticks.y = element_line(colour = "#B6BDCC", size = 0.1),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank(),

    #axis.line.x.bottom = element_line(colour = "#B6BDCC", size  = 0.3),
    #axis.line.y.left = element_line(colour = "#B6BDCC", size  = 0.3),

    #text elements
    plot.title = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = "#FFFFFF",
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = "#B6BDCC",
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = "#B6BDCC",
      size = 12),

    axis.text = element_text(
      colour = "#B6BDCC",
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, l = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = element_text(size = 12, family = font, colour = "#B6BDCC"),
    legend.title = element_text(size = 12, family = font, colour = "#B6BDCC"),
    legend.key = element_rect(fill = "#000000"),
    legend.background = element_rect(fill= "#000000")
  )



}

#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_blue()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
#'
statworx_blue <- function(font = "Arial"){

  theme_classic() + theme(

    #grid elements
    plot.background = element_rect(fill = "#0000FF"),
    panel.background = element_rect(fill = "#0000FF"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    # axis.title.x.bottom = element_text(colour = "#FFFFFF"),
    #  axis.title.y.left = element_text(colour = "#FFFFFF"),
    axis.ticks.x = element_line(colour = "#FFFFFF"),
    axis.ticks.y = element_line(colour = "#FFFFFF"),
    axis.line.x.bottom = element_line(colour = "#FFFFFF"),
    axis.line.y.left = element_line(colour = "#FFFFFF"),
    #  axis.text.x.bottom = element_text(colour = "#FFFFFF"),
    #  axis.text.y.left = element_text(colour = "#FFFFFF"),

    #text elements
    plot.title = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = "#FFFFFF",
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 12),

    axis.text = element_text(
      colour = "#FFFFFF",
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, l = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = element_text(size = 12, family = font, colour = "#FFFFFF"),
    legend.title = element_text(size = 12, family = font, colour = "#FFFFFF"),
    legend.key = element_rect(fill = "#0000FF", color = "#0000FF"),
    legend.background = element_rect(fill= "#0000FF")
  )



}

#' statworx Theme based on theme_classic
#' @description Sets a pre-defined theme as the standard ggplot theme via
#' \code{\link[ggplot2:ggtheme]{theme_classic}}
#' @param font set default font
#' @keywords theme
#' @export
#' @examples
#' \dontrun{
#' library(statworxTheme)
#' statworx_blue2()
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point()
#' }
#'
statworx_blue2 <- function(font = "Arial"){

  theme_classic() + theme(

    #grid elements#0000FF
    plot.background = element_rect(fill = "#0000FF"),
    panel.background = element_rect(fill = "#0000FF"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#FFFFFF", size = 0.1),
    panel.grid.major.y = element_line(colour = "#FFFFFF", size = 0.1),
    axis.ticks.x = element_line(colour = "#FFFFFF", size = 0.1),
    axis.ticks.y = element_line(colour = "#FFFFFF", size = 0.1),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank(),

    #axis.line.x.bottom = element_line(colour = "#B6BDCC", size  = 0.3),
    #axis.line.y.left = element_line(colour = "#B6BDCC", size  = 0.3),

    #text elements
    plot.title = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 10,
      hjust = 0,
      vjust = 5.5),

    plot.subtitle = element_text(
      family = font,
      colour = "#FFFFFF",
      face = 'bold',
      size = 18,
      hjust = 0,
      vjust = 2),

    plot.caption = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 10,
      hjust = 0),

    axis.title = element_text(
      family = font,
      colour = "#FFFFFF",
      size = 12),

    axis.text = element_text(
      colour = "#FFFFFF",
      family = font,
      size = 12),

    axis.text.x = element_text(
      margin=margin(5, b = 10)),

    axis.text.y = element_text(
      margin=margin(5, l = 10)),

    plot.margin = margin(5, t = 20),

    legend.text = element_text(size = 12, family = font, colour = "#FFFFFF"),
    legend.title = element_text(size = 12, family = font, colour = "#FFFFFF"),
    legend.key = element_rect(fill = "#0000FF", colour = "#0000FF"),
    legend.background = element_rect(fill= "#0000FF")
  )



}


# Define statworx colors
statworx_colors <- c(
  "Tech Blue" = "#0000FF",
  "Deep Black" = "#000000",
  "Super White" = "#FFFFFF",
  "grey_1" = "#283440",
  "grey_2" = "#6C7D8C",
  "grey_3" = "#B6BDCC",
  "grey_4" = "#EBF0F2",
  "accent_1" = "#FFFF00",
  "accent_2" = "#FE0D6C",
  "accent_3" = "#00C800"
)

#' statworx palettes
statworx_highlights <- c("#0000FF", "#000000", "#FFFFFF", "#283440", "#6C7D8C", "#B6BDCC", "#EBF0F2", "#FE0D6C", "#00C800", "#FFFF00")


statworx_standards_1 <- c("#0000BF", "#9BAEC1", "#9999FF", "#7D8AA4", "#C7014F", "#83FF83", "#FF9EC4",
                          "#009600")



statworx_standards_2 <- c("#0000BF", "#9BAEC1", "#C7014F", "#83FF83", "#9999FF", "#7D8AA4", "#FF9EC4",
                          "#009600")

continous_blue_black <- c("#0000BF", "#000000") # endpoints of continous palette

continous_blue_white <- c("#0000BF", "#FFFFFF") # endpoints of continous palette

continous_grey_white <- c("#9BAEC1", "#FFFFFF") # endpoints of continous palette

continous_grey_black <- c("#9BAEC1", "#000000") # endpoints of continous palette

continous_blue_red <- c("#0000BF", "#C7014F") # endpoints of continous palette





#' Wrap color vector in function.
#'
#' @param colors List of colors
#' @param ... Name of color from "statworx_highlights"
#' @return Hex code of the color
create_color_vector <- function(colors = statworx_highlights, ...) {
  cols <- c(...)

  if (is.null(cols))
    return (colors)

  colors[cols]
}

#' statworx palettes

#' @description Vector of color codes.
#' @export
# Make list of different palettes
statworx_palettes <- list(
  "statworx_highlights" = create_color_vector(colors = statworx_highlights),
  "statworx_standards_1" = create_color_vector(colors = statworx_standards_1),
  "statworx_standards_2" = create_color_vector(colors = statworx_standards_2),
  "continous_blue_black" = create_color_vector(colors = continous_blue_black),
  "continous_blue_white" = create_color_vector(colors = continous_blue_white),
  "continous_grey_white" = create_color_vector(colors = continous_grey_white),
  "continous_grey_black" = create_color_vector(colors = continous_grey_black),
  "continous_blue_red" = create_color_vector(colors = continous_blue_red)
)


#' Create a color palette function.
#' @param ... arguments passed to \code{\link[grDevices]{colorRampPalette}}
#' @param palette A color palette or "custom".
#' @param reverse If true, order of palette is reversed.
#' @param col_list Provide a vector of colors if @param palette = "custom".
#' @return Palette generating function based on "colorRampPalette".
#'
create_statworx_palette <- function(palette = "statworx_standards_1",
                                    reverse = FALSE, col_list = "", ...) {
  if (palette == "custom" & is.null(col_list)) {
    palette = "statworx_palette"
  }

  if (palette == "custom") {
    pal <- col_list
  } else {
    pal <- statworx_palettes[[palette]]
  }

  if (reverse)
    pal <- rev(pal)

  colorRampPalette(pal, ...)
}


#' Create a constructor for ggplot for lines, points, etc.
#'
#' @param ... arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @param palette A color palette.
#' @param discrete If true, discrete palette is returned. Otherwise, continuous palette.
#' @param reverse If true, order of palette is reversed.
#' @param col_list Provide a vector of colors if parameter palette="custom".
#' @return Scale constructor for ggplot.
#' @keywords scale_color_statworx()
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point() + scale_color_statworx()
#'
scale_color_statworx <- function(palette = "statworx_standards_1", discrete = TRUE,
                                 reverse = FALSE, col_list = c(), ...) {

  pal <- create_statworx_palette(palette = palette, reverse = reverse, col_list = col_list)

  if (discrete) {
    discrete_scale("colour", paste0("statworx_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Create a constructor for ggplot for box plots, bar plots, etc.
#'
#' @param ... arguments passed to \code{\link[ggplot2]{discrete_scale}}
#' @param palette A color palette.
#' @param discrete If true, discrete palette is returned. Otherwise, continuous palette.
#' @param reverse If true, order of palette is reversed.
#' @param col_list Provide a vector of colors if parameter palette="custom".
#' @return Scale constructor for ggplot.
#' @keywords scale_fill_statworx()
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point() +  scale_fill_statworx()
#'
scale_fill_statworx <- function(palette = "statworx_standards_1", discrete = TRUE,
                                reverse = FALSE, col_list = c(), ...) {

  pal <- create_statworx_palette(palette = palette, reverse = reverse, col_list = col_list)

  if (discrete) {
    discrete_scale("fill", paste0("statworx_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
