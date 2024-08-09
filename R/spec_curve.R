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
#' @param orig_beta_se A three-element vector containing the original specification's beta, SE, and CV values. To be used if the original specification is not in the data. Will assign analytical decision values of 0 unless \code{orig_spec_values} is also specified.
#' @param orig_spec_values A vector with the same length as \code{decision_cols} with the values that correspond to the original analysis.
#' @param left_box Width of y-axis title area
#' @param ylim A two-element vector that limits the y-axis with effect size. Set to \code{NULL} to use full range of estimates.
#' @param add_hline_at Add a horizontal line at this value. Set to \code{NULL} to omit.
#' @param skip_theme Don't apply the package theme, instead use \code{ggplot2} defaults.
#' @param font_size Set font size in points for the variant box at bottom.
#' @param color_text_labels Use a gradient color scale for the text labels (requires numeric analytic decision columns). Set to \code{TRUE} to include, or to a \code{ggplot2::scale_color} function to specify your own color scale.
#' @param height_ratio The ratio of the top plot to the bottom plot.
#' @param return_separate Return two ggplots separately in a list so you can combine them yourself.
#' @param additional_highlights_spec_values A named list of named lists, including additional categories to highlight on the graph. The name of each list is the name of the category, and the list contains vectors indicating the values to highlight, in the same form as \code{orig_spec_values}. For example, setting this to \code{list('Orig. Robustness' = list(c(1,1,2),c(1,1,3)), 'Comparative' = list(c(1,2,1)))} would add two new colors to the graph, one for "Orig. Robustness", for the analytic decision values 1, 1, 2 and 1, 1, 3, and one for "Comparative" for the original values 1, 2, 1.
#' @param additional_highlights_beta_se The same as \code{additional_highlights_spec_values} but instead of accepting vectors for original analytic decision values, takes \code{c(beta, se)} vectors of exact effects to add.
#' @examples
#'
#' # Example data
#' data(cholera_dat)
#' spec_curve(cholera_dat[cholera_dat$outcome == 3,],
#'   decision_cols = c('analytical_decision_1','analytical_decision_2','analytical_decision_3'),
#'   decision_labels = c('Specification','Bandwidth','Controls'),
#'   orig_spec_values = c(1,1,1))
#'
#' @export
spec_curve = function(spec_data, decision_cols,
                      beta = 'beta', se = 'se', cv = 'cv',
                      decision_labels = NULL,
                      orig_beta_se = NULL,
                      orig_spec_values = NULL,
                      left_box = 50,
                      ylim = NULL,
                      add_hline_at = 0,
                      skip_theme = FALSE,
                      font_size = 8,
                      color_text_labels = NULL,
                      height_ratio = c(4,1),
                      return_separate = FALSE,
                      additional_highlights_spec_values = NULL,
                      additional_highlights_beta_se = NULL) {
  takennames = c('ci95_bot','ci95_top',
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
  if (!is.null(orig_beta_se) & !is.null(orig_spec_values)) {
    stop('Please only specify one of orig_beta_se and orig_spec_values')
  }
  if (!is.null(additional_highlights_spec_values) & !is.null(additional_highlights_beta_se)) {
    stop('Please only specify one of additional_highlights_spec_values and additional_highlights_beta_se')
  }

  dat = as.data.table(spec_data)
  setnames(dat,beta,'beta')
  setnames(dat,se,'se')
  setnames(dat,cv,'cv')

  # Recast the analytical decisions as numeric
  for (i in 1:length(decision_cols)) {
    if (!is.numeric(dat[[decision_cols[i]]])) {
      convtab = sort(unique(dat[[decision_cols[i]]]))
      dat[[decision_cols[i]]] = unname(sapply(dat[[decision_cols[i]]],
                                       \(x) which(convtab == x)))
    }
  }
  dat[, (decision_cols) := lapply(.SD, as.numeric), .SDcols = decision_cols]
  dat = copy(dat)

  dat[, Original := FALSE]
  if (!is.null(orig_spec_values)) {
    isorig = lapply(1:length(decision_cols),
                    \(x) dat[[decision_cols[x]]] == orig_spec_values[x])
    if (length(decision_cols) > 1) {
      for (i in 2:length(decision_cols)) {
        isorig[[1]] = isorig[[1]]*isorig[[i]]
      }
    }
    dat[, Original := as.logical(isorig[[1]])]
  }
  if (!is.null(orig_beta_se)) {
    if (is.null(orig_spec_values)) {
      orig_spec_values = rep(0,length(decision_cols))
    }
    origspec = data.table(beta = orig_beta_se[1],
                          se = orig_beta_se[2],
                          cv = orig_beta_se[3])
    for (i in 1:length(orig_spec_values)) {
      origspec[[decision_cols[i]]] = orig_spec_values[i]
    }
    origspec = copy(origspec)
    origspec[, Original := TRUE]
    dat[, Original := FALSE]
    dat = rbind(dat, origspec, fill = TRUE)
  }


  newpal = c('#00BDD0','salmon', "#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666","#1B9E77")
  newlabs = c('Variant','Original')
  lettertrack = 2
  if (!is.null(additional_highlights_spec_values)) {
    dat[, Original := fifelse(Original, 'B', 'A')]
    for (i in 1:length(additional_highlights_spec_values)) {
      lettertrack = lettertrack + 1
      thiscat = names(additional_highlights_spec_values)[i]
      newlabs = c(newlabs, thiscat)
      for (j in 1:length(additional_highlights_spec_values[[i]])) {
        isorig = lapply(1:length(decision_cols),
                        \(k) dat[[decision_cols[k]]] == additional_highlights_spec_values[[i]][[j]][k])
        if (length(decision_cols) > 1) {
          for (k in 2:length(decision_cols)) {
            isorig[[1]] = isorig[[1]]*isorig[[k]]
          }
        }
        dat[, Original := fifelse(as.logical(isorig[[1]]), LETTERS[lettertrack], Original)]
      }
    }
  }
  if (!is.null(additional_highlights_beta_se)) {
    dat[, Original := fifelse(Original, 'B', 'A')]
    for (i in 1:length(additional_highlights_beta_se)) {
      lettertrack = lettertrack + 1
      thiscat = names(additional_highlights_beta_se)[i]
      newlabs = c(newlabs, thiscat)
      thisspec = data.table(beta = additional_highlights_beta_se[[i]][[1]][1],
                            se = additional_highlights_beta_se[[i]][[1]][2])
      if (length(additional_highlights_beta_se[[i]]) > 1) {
        for (j in 2:length(additional_highlights_beta_se[[i]])) {
          thisspec = rbind(thisspec, data.table(beta = additional_highlights_beta_se[[i]][[j]][1],
                                                 se = additional_highlights_beta_se[[i]][[j]][2]))
        }
      }
      for (j in 1:length(decision_cols)) {
        thisspec[[decision_cols[j]]] = 0
      }
      thisspec = copy(thisspec)
      thisspec[, Original := LETTERS[lettertrack]]
      dat = rbind(dat, thisspec, fill = TRUE)
    }
  }
  newpal = newpal[1:length(newlabs)]

  dat[, ci95_bot := beta - cv*se]
  dat[, ci95_top := beta + cv*se]
  setorder(dat, beta)
  dat[, order := 1:.N]


  p1 = ggplot2::ggplot(dat, ggplot2::aes(x = order, y = beta,
                  ymin = ci95_bot, ymax = ci95_top,
                  xmin = order - .5, xmax = order + .5,
                  fill = Original)) +
    ggplot2::geom_rect(alpha = .4) +
    ggplot2::geom_point(size = 1, color = 'black',
                        show.legend = FALSE)+
    ggplot2::scale_fill_manual(values = newpal,
                               labels = newlabs) +
    ggplot2::coord_cartesian(ylim=ylim)

  if (!is.null(add_hline_at)) {
    p1 = p1 + ggplot2::geom_hline(yintercept = add_hline_at,
                                  color = 'black')
  }
  if (is.logical(dat$Original)) {
    if (sum(dat$Original) == 0) {
      p1 = p1 + ggplot2::guides(fill = 'none')
    }
  }


  if (!skip_theme) {
    p1 = p1 +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 0, hjust = 0, vjust = 1),
                     panel.grid.major.y = ggplot2::element_line(linewidth = .75,
                                                                 linetype = 'dashed'),
                     legend.position = 'top') +
      ggplot2::labs(y = "Effect",
                    caption = '95% confidence intervals shown.',
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

#' Decision Label Table
#'
#' This produces a table showing the correspondence between the numeric values of the analytical decisions and their labels.
#'
#' Output is a \code{ggplot2} graph, so graphical customization should be possible after the fact if desired.
#'
#' @param dat Properly formatted data set, limited to a single outcome.
#' @param decision_cols String vector containing the column names with the analytical decisions.
#' @param decision_labels String vector with the names of the analytical decisions, if not the column names themselves.
#' @param filename File name to save the table to. Must end in '.csv'. Set to \code{NULL} to not export to file.
#' @examples
#'
#' # Example data
#' data(cholera_dat)
#' spec_curve(cholera_dat[cholera_dat$outcome == 3,],
#'   decision_cols = c('analytical_decision_1','analytical_decision_2','analytical_decision_3'),
#'   decision_labels = c('Specification','Bandwidth','Controls'),
#'   orig_spec_values = c(1,1,1))
#'
#' @export
decision_label_table = function(dat, decision_cols, decision_labels,
                                filename = 'decision_label_table.csv') {
  if (substr(filename,nchar(filename)-3,nchar(filename)) != '.csv') {
    stop('Filename must end in .csv')
  }
  dat = as.data.table(dat)
  outtab = list()
  # Recast the analytical decisions as numeric
  for (i in 1:length(decision_cols)) {
    if (!is.numeric(dat[[decision_cols[i]]])) {
      convtab = sort(unique(dat[[decision_cols[i]]]))
      outtab[[i]] = data.table(a = 1:length(convtab),
                               b = convtab)
      setnames(outtab[[i]],paste0(decision_labels[i],c(' Value',' Label')))
      outtab[[i]] = outtab[[i]][, lapply(.SD, as.character)]
    } else {
      outtab[[i]] = as.data.table(vtable::labeltable(dat[[decision_cols[i]]], out = 'return'))
      setnames(outtab[[i]],paste0(decision_labels[i],c(' Value',' Label')))
      outtab[[i]] = outtab[[i]][, lapply(.SD, as.character)]
    }
  }
  # make them all the same length
  max_len = max(sapply(outtab, nrow))
  final_output = data.table()
  for (i in 1:length(outtab)) {
    if (nrow(outtab[[i]]) < max_len) {
      outtab[[i]] = rbindlist(list(outtab[[i]],
                              data.table(a = rep('',max_len - nrow(outtab[[i]])),
                                         b = rep('',max_len - nrow(outtab[[i]])))),
                              use.names = FALSE)
    }
    final_output = cbind(final_output, outtab[[i]])
  }
  if (!is.null(filename)) {
    fwrite(final_output, filename)
  }
  return(final_output)
}
