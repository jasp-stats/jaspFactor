# Draw scree plots from realized eigenvalues; parallel analysis stays in the analysis.
.factorScreePlot <- function(data, nVariables, eigenvaluesAbove, xName, extraTheme) {
  plt <- ggplot2::ggplot(data, ggplot2::aes(x = id, y = ev, linetype = type, shape = type)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(x = xName, y = gettext("Eigenvalue")) +
    ggplot2::geom_hline(yintercept = eigenvaluesAbove)

  pointSize <- 3 + log(10) - log(nVariables)
  if (pointSize > 0)
    plt <- plt + ggplot2::geom_point(na.rm = TRUE, size = max(0, pointSize))

  plt <- plt + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw() +
    ggplot2::scale_x_continuous(breaks = seq(1:nVariables))

  if (extraTheme)
    plt <- plt + jaspGraphs::themeJaspRaw()

  plt + ggplot2::theme(
    legend.position = c(0.99, 0.95), legend.justification = c(1, 1),
    legend.text = ggplot2::element_text(size = 12.5),
    legend.title = ggplot2::element_blank(),
    legend.key.size = ggplot2::unit(18, "pt")
  )
}

.factorMisfitPlot <- function(ggmisfit) {
  misfitplot <-
    ggplot2::ggplot(ggmisfit, ggplot2::aes(x = Var1, y = Var2, fill = value,
                                           label = labels)) +
    ggplot2::geom_tile(na.rm = TRUE) +
    ggplot2::geom_text(color = ifelse(ggmisfit$value > .5, "white", "black"),
                       na.rm = TRUE) +
    ggplot2::scale_y_discrete(limits = rev(levels(ggmisfit$Var1))) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_continuous(low = "#FFFFFF", high = "#000000",
                                   na.value = "transparent",
                                   limits = c(0, 1)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 0)) +
    jaspGraphs::themeJaspRaw()

  return(misfitplot)
}
