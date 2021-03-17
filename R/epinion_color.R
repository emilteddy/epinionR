#' Extract HEX codes by Epinion name of the color
#'
#' This function gives the user an easy access to the official Epinion colors by their name. This can be used every time you need to specify
#' a color in your code as it just returns the HEX code which in return R converts into its respective color.
#'
#' @param ... Takes the Epinion name of the color as input - i.e. Epinion Red, Dark Blue, Dark Purple, etc.
#' @return A HEX code of the specified color
#' @export
epi_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (epi_colors)

  epi_colors[cols]
}

#' Extract a palette of Epinion colors
#'
#' This function gives the user easy access to different palettes of Epinion colors commonly used.
#'
#' @param palette Defaults to main palette. Other palettes include extra, extra2 and full.
#' @param reverse Defaults to FALSE which implies that the colors are returned in the order they appear in the palette. TRUE reverses this order.
#' @param ... Additional arguments may apply.
#' @return A palette of official Epinion colors
#' @export
epi_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- epi_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Apply Epinion colors to graph
#'
#' This function allows you to add Epinion colors to an existing plot by adding this function at the end of your ggplot2 syntax.
#' The function is especially convenient when the color argument is specified by a variable in your data.
#'
#' @param palette Defaults to main. If you need extra colors or the full palette, please specify extra, extra2 or full.
#' @param discrete Defaults to TRUE. If you have an inherit continous variable, please change to FALSE.
#' @param reverse Defaults to FALSE. If you need to reverse the order of the colors in the palette, change this to TRUE.
#' @param ... Additional arguments may apply.
#' @return A CVI compliant plot
#' @export
epinion_color <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- epi_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("epi_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Apply Epinion colors to graph
#'
#' This function allows you to add Epinion colors to an existing plot by adding this function at the end of your ggplot2 syntax.
#' The function is especially convenient when the fill argument is specified by a variable in your data.
#'
#' @param palette Defaults to main. If you need extra colors or the full palette, please specify extra, extra2 or full.
#' @param discrete Defaults to TRUE. If you have an inherit continous variable, please change to FALSE.
#' @param reverse Defaults to FALSE. If you need to reverse the order of the colors in the palette, change this to TRUE.
#' @param ... Additional arguments may apply.
#' @return A CVI compliant plot
#' @export
epinion_fill <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- epi_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("epi_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
