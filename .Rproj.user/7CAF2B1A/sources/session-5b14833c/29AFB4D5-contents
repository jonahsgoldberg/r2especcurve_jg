#' Specification Curve for R2E-formatted Replication Mixes
#'
#' This produces a specification curve if given data matching the R2E format.
#'
#' Output is a \code{ggplot2} graph, so graphical customization should be possible after the fact if desired.
#'
#' @param dat Properly formatted data set, limited to a single outcome.
#' @param decision_cols String vector containing the column names with the analytical decisions.
#' @param beta String variable indicating the column with the effect of interest.
#' @param se String variable indicating the column with the standard error.
#' @param decision_labels String vector with the names of the analytical decisions, if not the column names themselves.
#' @param orig_beta_se A two-element vector containing the original specification's beta and SE values. To be used if the original specification is not in the data. Will assign analytical decision values of 0 unless \code{orig_spec_values} is also specified.
#' @param orig_spec_values A vector with the same length as \code{decision_cols} with the values that correspond to the original analysis.
#' @param left_box Width of y-axis title area
#' @param add_hline_at Add a horizontal line at this value. Set to \code{NULL} to omit.
#' @param skip_theme Don't apply the package theme, instead use \code{ggplot2} defaults.
#' @param font_size Set font size in points for the variant box at bottom.
#' @param color_text_labels Use a gradient color scale for the text labels (requires numeric analytic decision columns). Set to \code{TRUE} to include, or to a \code{ggplot2::scale_color} function to specify your own color scale.
#' @param height_ratio The ratio of the top plot to the bottom plot.
#' @param return_separate Return two ggplots separately in a list so you can combine them yourself.
#' @examples
#'
#' # Example data
#' data(cholera_dat)
#' spec_curve(cholera_dat[cholera_dat$outcome == 3,],
#'   decision_cols = c('analytical_decision_1','analytical_decision_2','analytical_decision_3'),
#'   decision_labels = c('Specification','Bandwidth','Controls'))
#'
#' @export
spec_curve = function(spec_data, decision_cols,
                      beta = 'beta', se = 'se',
                      decision_labels = NULL,
                      orig_beta_se = NULL,
                      orig_spec_values = NULL,
                      left_box = 50,
                      add_hline_at = 0,
                      skip_theme = FALSE,
                      font_size = 8,
                      color_text_labels = NULL,
                      height_ratio = c(4,1),
                      return_separate = FALSE) {
  takennames = c('ci90_bot','ci90_top','ci95_bot','ci95_top',
                 'order','Original')
  if (any(takennames %in% names(spec_data))) {
    stop(paste0('You cannot have the following column names in your data: ',
                paste(takennames, collapse = ', ')))
  }
  if (any(c(beta,se) %in% decision_cols)) {
    stop('You cannot have your beta or se columns in your decision_cols columns.')
  }
  if (!is.null(decision_labels)) {
    if (length(decision_cols) != length(decision_labels)) {
      stop('decision_labels and decision_cols must have the same length.')
    }
  }
  dat = as.data.table(spec_data)
  setnames(dat,beta,'beta')
  setnames(dat,se,'se')
  if (!is.null(orig_beta_se) & !is.null(orig_spec_values)) {
    stop('Please only specify one of orig_beta_se and orig_spec_values')
  }
  dat[, Original := FALSE]
  if (!is.null(orig_spec_values)) {
    isorig = lapply(1:length(decision_cols),
                    \(x) dat[[decision_cols[x]]] == orig_spec_values[x])
    for (i in 2:length(decision_cols)) {
      isorig[[1]] = isorig[[1]]*isorig[[i]]
    }
    dat[, Original := as.logical(isorig[[1]])]
  }
  if (!is.null(orig_beta_se)) {
    if (is.null(orig_spec_values)) {
      orig_spec_values = rep(0,length(decision_cols))
    }
    origspec = data.table(beta = orig_beta_se[1],
                          se = orig_beta_se[2])
    for (i in 1:length(orig_spec_values)) {
      origspec[[decision_cols[i]]] = orig_spec_values[i]
    }
    origspec[, Original := TRUE]
    dat[, Original := FALSE]
    dat = rbind(dat, origspec, fill = TRUE)
  }

  dat[, ci90_bot := beta - 1.65*se]
  dat[, ci90_top := beta + 1.65*se]
  dat[, ci95_bot := beta - 1.96*se]
  dat[, ci95_top := beta + 1.96*se]
  setorder(dat, beta)
  dat[, order := 1:.N]

  p1 = ggplot2::ggplot(dat, ggplot2::aes(x = order, y = beta,
                  ymin = ci95_bot, ymax = ci95_top,
                  xmin = order - .5, xmax = order + .5,
                  fill = Original)) +
    ggplot2::geom_rect(alpha = .2) +
    ggplot2::geom_rect(alpha = .5,
                       ggplot2::aes(ymin = ci90_bot, ymax = ci90_top)) +
    ggplot2::geom_point(size = 1, color = 'black',
                        show.legend = FALSE)+
    ggplot2::scale_fill_manual(values = c('#00BDD0','salmon'),
                               labels = c('Variant','Original'))
  if (!is.null(add_hline_at)) {
    p1 = p1 + ggplot2::geom_hline(yintercept = add_hline_at,
                                  color = 'black')
  }
  if (sum(dat$Original) == 0) {
    p1 = p1 + ggplot2::guides(fill = 'none')
  }


  if (!skip_theme) {
    p1 = p1 +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 0, hjust = 0, vjust = 1),
                     panel.grid.major.y = ggplot2::element_line(linewidth = .25,
                                                                 linetype = 'dashed'),
                     legend.position = 'top') +
      ggplot2::labs(y = "Effect",
                    caption = '90% and 95% confidence intervals shown.',
                    fill = NULL)
  }
  p1 = p1 +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

  p2dat = melt(subset(dat, select = c('order',
                                      decision_cols)), id.vars = 'order')
  if (!is.null(decision_labels)) {
    for (i in 1:length(decision_cols)) {
      p2dat[variable == decision_cols[i], variable := decision_labels[i]]
    }
    p2dat[, variable := factor(variable, levels = rev(decision_labels))]
  } else {
    p2dat[, variable := factor(variable, levels = rev(decision_cols))]
  }

  if (is.null(color_text_labels)) {
    p2 = ggplot2::ggplot(p2dat, ggplot2::aes(x = order,
                                           y = variable,
                                           label = value)) +
      ggplot2::geom_text(size = font_size/ggplot2::.pt) +
      ggplot2::theme_void() +
      ggplot2::theme(axis.text.y = ggplot2::element_text())
  } else {
    p2 = ggplot2::ggplot(p2dat, ggplot2::aes(x = order,
                                             y = variable,
                                             label = value,
                                             color = factor(value))) +
      ggplot2::geom_text(size = font_size/ggplot2::.pt) +
      ggplot2::theme_void() +
      ggplot2::theme(axis.text.y = ggplot2::element_text()) +
      ggplot2::guides(color = 'none')
    if (!is.logical(color_text_labels)) {
      p2 = p2 + color_text_labels
    }
  }

  if (return_separate) {
    return(list(p1,p2))
  }
  p1/p2 + patchwork::plot_layout(widths = c(1,1), heights = height_ratio)
}

