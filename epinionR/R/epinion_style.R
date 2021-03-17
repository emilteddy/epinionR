#' Add Epinion colors to your graph
#'
#' This function can be added at the end of your ggplot2 syntax. Thus it takes a plot object as an input and adds a layer consisting of
#' the Epinion guidelines for conducting visualisations. In this way, it is convient to be compliant with the CVI when you are on the fly.
#' As a defualt it does not include a lengend if you split your data by a group variable, but this can easily be added be setting the
#' legend argument to TRUE.
#'
#' @param legend Defaults to FALSE. If you want to include a legend, please change this setting to TRUE. It will add the legend at bottom.
#' @return A CVI compliant plot
#' @export
epinion_style <- function(legend = FALSE) {

  skrifttype <- "Arial"

  if (legend) {
  ggplot2::theme(
    plot.title = ggplot2::element_text(family=skrifttype,
                                       size=14,
                                       face="bold",
                                       color="#0F283C"),

    plot.subtitle = ggplot2::element_text(family=skrifttype,
                                          size=12,
                                          color="#0F283C",
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=skrifttype,
                                        size=10,
                                        color="#0F283C"),
    axis.title = ggplot2::element_text(family=skrifttype,
                                       size=12,
                                       color="#0F283C"),
    axis.text = ggplot2::element_text(family=skrifttype,
                                      size=10,
                                      color="#0F283C"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks.x = ggplot2::element_line(size = 0.3,
                                         color = "#0F283C"),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size = 0.5,
                                        color = "#0F283C"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(family = skrifttype,
                                       size  = 10,
                                       color = "#0F283C",
                                       hjust = 0)
  )
  } else {
    ggplot2::theme(
      plot.title = ggplot2::element_text(family=skrifttype,
                                         size=14,
                                         face="bold",
                                         color="#0F283C"),
      plot.subtitle = ggplot2::element_text(family=skrifttype,
                                            size=12,
                                            color="#0F283C",
                                            margin=ggplot2::margin(9,0,9,0)),
      plot.caption = ggplot2::element_blank(),
      legend.position = "none",
      axis.title = ggplot2::element_text(family=skrifttype,
                                         size=12,
                                         color="#0F283C"),
      axis.text = ggplot2::element_text(family=skrifttype,
                                        size=10,
                                        color="#0F283C"),
      axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
      axis.ticks.x = ggplot2::element_line(size = 0.3,
                                           color = "#0F283C"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(size = 0.5,
                                          color = "#0F283C"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill="white"),
      strip.text = ggplot2::element_text(family = skrifttype,
                                         size  = 10,
                                         color = "#0F283C",
                                         hjust = 0)
    )
  }
}
