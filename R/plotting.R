#' Plot 1d density
#'
#' Creates density for U-Pb data.
#'
#' @param dat data.frame
#' @param bw density bandwidth
#' @param type 'kde': traditional KDE 'pdd': detrial zircon PDD
#' @param age_range range over which to calculate density
#' @param facet logical, facet samples?
#'
#' @return  ggplot2 1d density plot with histogram
plot_dens <- function(dat, bw=30, type='kde',
                           age_range=c(0, 4560), facet=FALSE) {
  if (facet) {
    l <- lapply(split(dat, factor(dat$sample)), calc_dens, bw=bw,
                type=type, age_range=age_range)
    dens <- do.call(rbind.data.frame, l)
    gplot <- ggplot2::ggplot()
    gplot <- gplot + ggplot2::geom_path(data=dens,
                                        ggplot2::aes_string(x='x', y='y'))
    gplot <- gplot + ggplot2::facet_grid(sample ~ ., scale='free_y')
    gplot <- gplot + plot_labels(ylab = 'Density') + plot_bw_theme() +
      plot_axis_lim(xlim=age_range)
    return(gplot)
  } else {
    dens <- calc_dens(dat, bw=bw, type=type,
                           age_range=age_range)
    gplot <- ggplot2::ggplot()
    gplot <- gplot + ggplot2::geom_path(data=dens,
                                        ggplot2::aes_string(x='x', y='y'))
    gplot <- gplot + plot_labels(ylab = 'Density') + plot_bw_theme() +
      plot_axis_lim(xlim=age_range)
    return(gplot)
  }
}

#' Plot 1d density with histogram
#'
#' Creates density and histogram plot of U-Pb data.
#'
#' @param dat data.frame
#' @param bw density bandwidth
#' @param binwidth histogram binwidth
#' @param type 'kde': traditional KDE 'pdd': detrial zircon PDD
#' @param age_range range over which to calculate density
#' @param facet logical, facet samples?
#'
#' @return  ggplot2 1d density plot with histogram
plot_dens_hist <- function(dat, bw=30, binwidth=50, type='kde',
                              age_range=c(0, 4560), facet=FALSE) {
  if (facet) {
    l <- lapply(split(dat, factor(dat$sample)), calc_dens_hist, bw=bw,
                binwidth=binwidth, type=type, age_range=age_range)
    dens <- do.call(rbind.data.frame, l)
    gplot <- ggplot2::ggplot() + ggplot2::geom_histogram(
      data=dat,
      ggplot2::aes_string(x='age'),
      color='grey90',
      fill='grey60',
      breaks=seq(0, 4560,
                 binwidth))
    gplot <- gplot + ggplot2::facet_grid(sample ~ ., scale='free_y')
    gplot <- gplot + ggplot2::geom_path(data=dens,
                                        ggplot2::aes_string(x='x', y='y'))
    gplot <- gplot + plot_labels(ylab = 'Count') + plot_bw_theme() +
      plot_axis_lim(xlim=age_range)
    return(gplot)
    } else {
      dens <- calc_dens_hist(dat, binwidth=binwidth, bw=bw, type=type,
                             age_range=age_range)
      gplot <- ggplot2::ggplot() + ggplot2::geom_histogram(
        data=dat,
        ggplot2::aes_string(x='age'),
        color='grey90',
        fill='grey60',
        breaks=seq(0, 4560,
                   binwidth))
      gplot <- gplot + ggplot2::geom_path(data=dens,
                                          ggplot2::aes_string(x='x', y='y'))
      gplot <- gplot + plot_labels(ylab = 'Count') + plot_bw_theme() +
        plot_axis_lim(xlim=age_range)
      return(gplot)
  }
}

#' Labels for ggplot2 plots
#'
#' Convenience function to label ggplot2
#'
#' @param xlab X-axis label
#' @param ylab Y-axis label
#'
#' @return Returns ggplot2 labels
plot_labels <- function(xlab = 'Age (Ma)', ylab = 'Density') {
   p_labels <- list(ggplot2::xlab(xlab),
                    ggplot2::ylab(ylab))
   return(p_labels)
}

#' Axes limits for ggplot2
#'
#' Convenience function to change axes limits for ggplot2
#'
#' @param xlim x-axis limit
#' @param ylim y-axis limit
#'
#' @return list of ggplot2::coord_cartesian object
#' @export
#'
plot_axis_lim <- function(xlim = c(0, 4560), ylim=NULL) {
  p_x_lim <- list(ggplot2::coord_cartesian(xlim=xlim, ylim=ylim))
}

#' Stripped down theme for ggplot2
#'
#' @return Returns ggplot2 theme
plot_bw_theme <- function() {
  p_bw_theme <- ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                         panel.grid.minor=ggplot2::element_blank())
  return(p_bw_theme)
}

#' Plot ecdf
#'
#' Plot ecdf for U-Pb age or Lu-Hf model age data.
#'
#' @param dat data.frame
#' @param mult_ecdf logical, plot several ecdfs in same plot
#' @param column which column to use
#' @param conf logical, plot confidence bands
#' @param guide logical, show legend
#' @param alpha alpha-level used for confidence bands
#'
#' @export
#'
plot_ecdf <- function(dat, mult_ecdf=FALSE, column='age', conf=FALSE,
                      guide=TRUE, alpha=0.05) {
  if (mult_ecdf) {
    l <- lapply(split(dat, factor(dat$sample)), calc_dkw, column=column,
                alpha=alpha)
    conf_data <- do.call(rbind.data.frame, l)
    gplot <- ggplot2::ggplot() +
      ggplot2::geom_line(data=conf_data,
                         ggplot2::aes_string(x='x', y='y',
                                             color='sample')) +
      plot_bw_theme() + plot_labels(ylab = 'Probabilty')
    if (conf) {
      gplot <- gplot +
        ggplot2::geom_line(data=conf_data,
                           ggplot2::aes_string(x='x', y='low', color='sample'),
                           linetype=2) +
        ggplot2::geom_line(data=conf_data,
                           ggplot2::aes_string(x='x', y='high', color='sample'),
                           linetype=2)
    }
  } else {
    conf_data <- calc_dkw(dat, column=column, alpha=alpha)
    gplot <- ggplot2::ggplot()
    gplot <- gplot + ggplot2::geom_line(data=conf_data,
                                ggplot2::aes_string(x='x', y='y')) +
      plot_bw_theme() + plot_labels(ylab = 'Probabilty')
    if (conf) {
      gplot <- gplot +
        ggplot2::geom_line(data=conf_data,
                           ggplot2::aes_string(x='x', y='low'), linetype=2) +
        ggplot2::geom_line(data=conf_data,
                           ggplot2::aes_string(x='x', y='high'), linetype=2)
    }
  }
  if (guide == FALSE) {
    gplot <- gplot + ggplot2::guides(color=FALSE)
  }
  gplot
}

#' Plot Lu-Hf data
#'
#' Plot Lu-Hf data as both epsilon-Hf vs. age and 176/177Hf vs. age.
#'
#' @param dat data.frame
#' @param range range to display
#' @param plot_type 'ehf'=epsilon-Hf; 'hfhf'=176/177Hf
#' @param guide logical, show legend?
#' @param contours logical, plot contours?
#' @param contour_data data.frame containing data to contour
#' @param combine_contours logical combine contouring data
#' @param constants vector of constants which must be in the order
#' decay constant 176Lu, 176/177Hf CHUR, 176Lu/177Hf CHUR, 176/177Hf DM and
#' 176Lu/177Hf DM
#'
#' @export
#'
plot_hf <- function(dat, range=c(0, 4560), plot_type='ehf', guide=TRUE,
                    contours=FALSE, contour_data=NULL, combine_contours=FALSE,
                    constants) {
  if (plot_type == 'ehf') {
    lines <- hf_lines(range=range, plot_type='ehf', constants)
    gplot <- ggplot2::ggplot()
    gplot <- gplot +
      ggplot2::geom_line(data=lines, ggplot2::aes_string(x='x', y='line',
                                                   linetype='type'))
    gplot <- gplot + ggplot2::scale_linetype_manual(values=c(1, 1))
    gplot <- gplot + plot_bw_theme() +
      plot_labels(xlab='Age (Ma)',
                  ylab=expression(paste('Initial ', epsilon["Hf"])))
    if (contours & !is.null(contour_data)) {
      old_names <- c('sample', 'age', 'ehf_i')
      new_names <- c('s', 'a', 'e')
      names(contour_data)[which(names(contour_data) %in% old_names)] <-
        new_names
      if (combine_contours) {
        gplot <- gplot + ggplot2::geom_density2d(data=contour_data,
                                                 ggplot2::aes_string(x='a',
                                                                     y='e'),
                                                 h=c(30, 2.5)*4)
          gplot <- gplot + ggplot2::guides(color=FALSE)
      } else {
        gplot <- gplot + ggplot2::geom_density2d(data=contour_data,
                                                 ggplot2::aes_string(x='a',
                                                                     y='e',
                                                                     color='s'),
                                                 h=c(30, 2.5)*4)
        gplot <- gplot + ggplot2::guides(color=FALSE)
      }
    }
    gplot <- gplot +
      ggplot2::geom_point(data=dat,
                          ggplot2::aes_string(x='age', y='ehf_i',
                                              fill='sample'),
                          shape=21,
                          color='black',
                          size=3)
    gplot <- gplot + ggplot2::guides(linetype=FALSE)
  } else {
    if (plot_type == 'hfhf') {
      lines <- hf_lines(range=range, plot_type='hfhf', constants=constants)
      gplot <- ggplot2::ggplot() +
        ggplot2::geom_line(data=lines, ggplot2::aes_string(x='x', y='line',
                                                           linetype='type'))
      gplot <- gplot + ggplot2::scale_linetype_manual(values=c(1, 1))
      gplot <- gplot + plot_bw_theme() +
        plot_labels(xlab='Age (Ma)',
                    ylab=expression(paste("Initial "^{176},"Hf/",
                                          ""^{177},"Hf")))
      if (contours & !is.null(contour_data)) {
        old_names <- c('sample', 'age', 'hf_i')
        new_names <- c('s', 'a', 'h')
        names(contour_data)[which(names(contour_data) %in% old_names)] <-
          new_names
        if (combine_contours) {
          gplot <- gplot + ggplot2::geom_density2d(data=contour_data,
                                                   ggplot2::aes_string(x='a',
                                                                       y='h'),
                                                   h=c(30, 0.00025)*4) +
            ggplot2::guides(color=FALSE)
        } else {
          gplot <- gplot + ggplot2::geom_density2d(
            data=contour_data,
            ggplot2::aes_string(x='a', y='h', color='s'),
            h=c(30, 0.00025)*4)
          gplot <- gplot + ggplot2::guides(color=FALSE)

        }

      }
      gplot <- gplot +
        ggplot2::geom_point(data=dat,
                            ggplot2::aes_string(x='age',
                                                y='hf_i',
                                                fill='sample'),
                            color='black', shape=21, size=3)
      gplot <- gplot + ggplot2::guides(linetype=FALSE)
      }
  }
  if (guide == FALSE) {
    gplot <- gplot + ggplot2::guides(fill=FALSE)
  }
  gplot
}

#' Plot quantiles
#'
#' @param dat data.frame
#' @param column which column in data.frame to use
#' @param conf logical, plot confidence interval
#' @param alpha alpha-level
#' @param type type of quantile calculation (passed on to stats::quantile)
#' @param guide logical, show legend?
#' @param mix logical, add mixing model
#' @param mix_data mixing model data
#'
#' @export
#'
plot_quantiles <- function(dat, column='t_dm2', conf=FALSE, alpha=0.05, type=8,
                           guide=TRUE, mix=FALSE, mix_data=NULL) {
  quants <- calc_quantiles(dat=dat, column=column, alpha=alpha, type=type)
  line <- data.frame(x=c(0, 4560), y=c(0, 4560))
  gplot <- ggplot2::ggplot() +
    ggplot2::geom_line(data=line, ggplot2::aes_string(x='x', y='y'))
  if (mix & !is.null(mix_data)) {
    gplot <- gplot + ggplot2::geom_line(data=mix_data,
                                        ggplot2::aes_string(x='lq', y='uq'),
                                        color='red')
  }
  if (conf) {
    l <- lapply(split(dat, factor(dat$sample)), quant_bounds, column=column,
                alpha=alpha)
    quant_data <- do.call(rbind.data.frame, l)
    gplot <- gplot + ggplot2::geom_errorbar(data=quant_data,
                                            ggplot2::aes_string(x='x',
                                                                ymin='ymin',
                                                                ymax='ymax',
                                                                width=0))
    gplot <- gplot + ggplot2::geom_errorbarh(data=quant_data,
                                             ggplot2::aes_string(x='x',
                                                                 y='y',
                                                                 xmin='xmin',
                                                                 xmax='xmax',
                                                                 height=0))
  }
  gplot <- gplot +
    ggplot2::geom_point(data=quants, ggplot2::aes_string(x='twentyfive',
                                                         y='seventyfive',
                                                         fill='sample'),
                        color='black',
                        shape=21,
                        size=3)
  gplot <- gplot + plot_bw_theme() + plot_labels(xlab='Lower quartile (Ma)',
                                                 ylab='Upper quartile (Ma)')
  if (guide == FALSE) {
    gplot <- gplot + ggplot2::guides(fill=FALSE)
  }
  gplot
}