#' Check concordancy of input ages
#'
#' Check the concordancy of the U-Pb data and return the data within the desired
#' discordancy limit.
#'
#' @param dat data.frame containing at least ages and percentage of discordancy
#' @param disc_lim Discordancy limit
#' @return Concordant data
#' @export
check_conc <- function(dat, disc_lim = 10) {
  if ('disc' %in% names(dat)) {
    llim <- -disc_lim
    ulim <- disc_lim
    conc <- dat[(dat$disc >= llim & dat$disc <= ulim), ]
  } else {
    stop('Requires column with name disc')
  }
}

#' Calculate 1D density of age data
#'
#' Calculate the 1d density of U-Pb age data using KDE or PDD.
#'
#' @param dat data.frame containing at least ages and percentage of discordancy
#' @param bw Bandwidth
#' @param type Type to calculate 'kde': proper KDE; 'pdd': detrital zircon PDD
#' @param age_range Range over which to calculate density
#' @return Density
#' @export
calc_dens <- function(dat, bw=30, type='kde', age_range=c(0, 4560)) {
  n <- age_range[2] - age_range[1]
  if (type == 'kde') {
    dens <- stats::density(dat$age, bw=bw, n=n, from=age_range[1],
                           to=age_range[2])
    x <- dens$x
    y <- dens$y
   }
  if (type == 'pdd') {
    if ('uncert' %in%  names(dat)) {
      x <- seq(age_range[1], age_range[2])
      y <- sapply(x, FUN=function(x) sum(stats::dnorm(x, dat$age, dat$uncert)))
      y <- y / length(x)
    } else {
      stop('Cannot calculate pdd without individual uncertainty each grain')
    }
  }
  dens <- data.frame(x=x, y=y)
  if(!is.null(dat$sample)) {
    dens$sample <- rep(dat$sample[1], nrow(dens))
  }
  return(dens)
}

#' Calculate scaled 1d density
#'
#' Calculates 1d density of age data and scales it so that it can be plotted
#' in the same plot of a histogram of the age data
#'
#' @param dat data.frame
#' @param binwidth Histogram binwidth
#' @param bw Density Bandwith
#' @param type 'kde': KDE; 'pdd': detrital zircon PDD
#' @param age_range Age range to calculated density over
#'
#' @return Returns density
#' @export
calc_dens_hist <- function(dat, binwidth=50, bw=30, type='kde',
                           age_range=c(0, 4560)) {
  dens <- calc_dens(dat, bw, type, age_range)
  max_y <- max(dens$y)
  # binmin <- find_plot_min(dat$age, accuracy=binwidth)
  # binmax <- find_plot_max(dat$age, accuracy=binwidth)
  # breaks <- seq(binmin, binmax, binwidth)
  hist_data <- graphics::hist(dat$age, breaks=seq(0, 4560, binwidth),
                              plot=FALSE, right=TRUE)
  dens$y <- dens$y * (max(hist_data$counts) / max_y)
  return(dens)
}

#' Find minimum value for plotting
#'
#' Find the minumum value for histogram plotting.
#'
#' @param x vector of values
#' @param accuracy round to nearest
#' @export
find_plot_min <- function(x, accuracy=100) {
  minimum <- floor(min(x) / accuracy) * accuracy
}

#' Find maximum value for plotting.
#'
#' Find the maximum value for histogram plotting.
#'
#' @param x vector of values
#' @param accuracy round to nearest
#' @export
find_plot_max <- function(x, accuracy=100) {
  maximum <- ceiling(max(x) / accuracy) * accuracy
}

#' Wrapper function for \code{find_plot_min} and \code{find_plot_max}
#'
#' Find the minimum and maximum values for histogram plotting.
#'
#' @param x Age data
#' @param accuracy Round to nearest
#'
#' @return Returns vector of minimum and maximum plotting values
#' @export
find_plot_min_max <- function(x, accuracy=100) {
  return(c(find_plot_min(x, accuracy), find_plot_max(x, accuracy)))
}

#' Calculate 1d likeness of detrital zircon populations
#'
#' Calculates the likeness of detrital zircon populations in 1 dimenson after
#' Satoski et al. (2013).
#'
#' @param x vector
#' @param y vector
#' @param bw bandwidth
#' @param digits number, round result to significant digits
#'
#' @references Satkoski, A.M., Wilkinson, B.H., Hietpas, J., Samson, S.D., 2013.
#' Likeness among detrital zircon populations - An approach to the comparison of
#' age frequency data in time and space. GSA Bulletin 125, 1783-1799.
#' @export
satkoski_1d <- function(x, y, bw=30, digits=3) {

  a <- calc_dens(x, bw=bw)
  b <- calc_dens(y, bw=bw)

  L <- 1 - (sum(abs(a$y - b$y)) / 2)
  round(L, digits)
}

#' Calculate 2d (age and Lu-Hf) likeness of detrital zircon populations
#'
#' Calculates the likeness of detrital zircon populationsin 2 dimensions after
#' Satoski et al. (2013).
#'
#' @param x vector
#' @param y vector
#' @param bw vector of density bandwidths
#' @param digits number, round result to significant digits
#'
#' @references Satkoski, A.M., Wilkinson, B.H., Hietpas, J., Samson, S.D., 2013.
#' Likeness among detrital zircon populations - An approach to the comparison of
#' age frequency data in time and space. GSA Bulletin 125, 1783-1799.
#' @export
satkoski_2d <- function(x, y, bw=c(30, 2.5), digits=3) {
  if (all(is.na(x$ehf_i)) | all(is.na(y$ehf_i))) {
    return(NA)
  } else {
    n <- 100
    bw <- bw * 4
    lims <- c(0, 4560, -30, 30)
    x <- x[!is.na(x$ehf_i), ]
    y <- y[!is.na(y$ehf_i), ]
    a <- MASS::kde2d(x=x$age, y=x$ehf_i, h=bw, n=n, lims=lims)$z
    b <- MASS::kde2d(x=y$age, y=y$ehf_i, h=bw, n=n, lims=lims)$z
    a <- a / sum(a)
    b <- b / sum(b)
    L <- 1 - (sum(abs(a - b)) / 2)
    round(L, digits)
  }
}

#' Pairwise Satkoski likeness
#'
#' Populate a matrix with pairwise Satkoski 1d likeness.
#'
#'@param dat data.frame
#'@param bw density bandwidth
#'@param digits number, round result to significant digits
#' @references Satkoski, A.M., Wilkinson, B.H., Hietpas, J., Samson, S.D., 2013.
#' Likeness among detrital zircon populations - An approach to the comparison of
#' age frequency data in time and space. GSA Bulletin 125, 1783-1799.
#' @export
satkoski_1d_matrix <- function(dat, bw=30, digits=3) {
  populate_matrix(dat, FUN=satkoski_1d, bw=bw, digits=3)
}

#' Pairwise 2d Satkoski likeness
#'
#' Populate a matrix with pairwise Satkoski 12 likeness.
#'
#'@param dat data.frame
#'@param bw vector of density bandwidths
#'@param digits number, round result to significant digits
#'
#' @references Satkoski, A.M., Wilkinson, B.H., Hietpas, J., Samson, S.D., 2013.
#' Likeness among detrital zircon populations - An approach to the comparison of
#' age frequency data in time and space. GSA Bulletin 125, 1783-1799.
#' @export
satkoski_2d_matrix <- function(dat, bw=c(30, 2.5), digits=3) {
  populate_matrix(dat, FUN=satkoski_2d, bw=bw, digits=digits)
}

#' Populate matrix
#'
#' @param dat data.frame
#' @param FUN Function used to populate matrix
#' @param ... Additional parameters passed to function
#'
#' @return Populated matrix
#' @export
populate_matrix <- function(dat, FUN, ...) {
  n <- length(unique(dat$sample))
  if (n < 2) stop('Select more samples')
  name <- as.character(unique(dat$sample))
  len <- length(name)
  mat <- matrix(nrow=n, ncol=n)
  colnames(mat) <- name
  rownames(mat) <- name
  for (i in 1:(len - 1)) {
    for (j in 2:len) {
      if (!(i == j) & ((is.na(mat[i, j])) | is.na(mat[j, i]))) {
        L <- FUN(dat[dat$sample == name[i], ],
                         dat[dat$sample == name[j], ], ...)
        mat[i, j] <- L
        mat[j, i] <- L
      }
    }
  }
  return(mat)
}

#' Calculate hafnium values.
#'
#' Calculates the initial 176Hf/177Hf values, the initial epsilon hafnium
#' values, the model age using the measured 176Lu/177Hf value and
#' the model age assuming the parental magma was produced from an average continental crust
#' (176Lu/177Hf = 0.015) that originally was derived from the depleted mantle
#' (Griffin, 2004).
#'
#' @param dat data.frame, list or matrix of hafnium values
#' @param constants vector of constants which must be in the order
#' decay constant 176Lu, 176/177Hf CHUR, 176Lu/177Hf CHUR, 176/177Hf DM,
#' 176Lu/177Hf DM and 176Lu/177Hf value used for two-stage depleted mantle
#' model age calculations
#' @export
#' @references Bouvier, A., Vervoort, J.D. & Patchett, P.J. 2008.
#' The Lu-Hf and Sm-Nd isotopic composition of CHUR:
#' Constraints from unequilibrated chondrites and implications for the bulk composition of terrestrial planets.
#' Earth And Planetary Science Letters 273(1-2), 48-57.
#' @references Griffin, W., Belousova, E., Shee, S., Pearson, N. and O'Reilly, S. 2004.
#' Archean crustal evolution in the northern Yilgam Craton:
#' U-Pb and Hf-isotope evidence from detrital zircons. Precambrian Research, 231-282.
#' @references Soderlund, U., Patchett, J., Vervoort, J. & Isachsen, C. 2004.
#' The Lu-176 decay constant determined by Lu-Hf and U-Pb isotope systematics of Precambrian mafic intrusions.
#' Earth And Planetary Science Letters 219(3-4), 311-324.
calc_hf <- function(dat, constants) {
  lambda_lu <- constants[1]
  hfhf_chur <- constants[2]
  luhf_chur <- constants[3]
  hfhf_dm <- constants[4]
  luhf_dm <- constants[5]
  luhf_zrc <- constants[6]
  if (!('hfhf' %in% names(dat))) return(dat)
  if (is.vector(dat) || ncol(dat) < 3) {
    stop('Data must contain at least three columns')
  }
  hf_chur <- hfhf_chur - (luhf_chur * (exp(lambda_lu * (dat$age * (10^6))) - 1))
  if ('ehf_i' %in% names(dat)) {
    hf_i <- hf_chur * ((dat$ehf_i / 10^4) + 1)
  } else {
    if ('hfhf' %in% names(dat)) {
      hf_i <- dat$hfhf - (dat$luhf * (exp(lambda_lu * (dat$age * (10^6))) - 1))
      ehf_i <- ((hf_i / hf_chur) - 1) * 10^4
    }
  }
  if ('hfhfse' %in% names(dat)) {
    hf_2se <- 2 * dat$hfhfse
    ehf_2se <- ((((hf_i + hf_2se) / (hfhf_chur - (luhf_chur * (exp(lambda_lu *
                                                 (dat$age * (10^6))) - 1)))) -
                                                  1)*10^4) - ehf_i
  } else {
    hf_2se <- rep(NA, nrow(dat))
    ehf_2se <- hf_2se
  }
  hf_dm <- hf_i + (luhf_zrc * (exp(lambda_lu * (dat$age * (10^6))) -1))
  t_dm2 <- ((1 / (lambda_lu)) *
              log(((hf_dm - hfhf_dm) / (luhf_zrc - luhf_dm)) + 1)) / 10^6
  if ('ehf_i' %in% names(dat)) {
    dat <- cbind(dat, ehf_2se, hf_i, hf_2se, hf_chur, t_dm2)
  } else {
    if ('hfhf' %in% names(dat)) {
      t_dm <- (((1 / (lambda_lu)) *
                  log(((dat$hfhf - hfhf_dm) / (dat$luhf - luhf_dm)) + 1)) / 10^6)
      dat <- cbind(dat, hf_i, hf_2se, ehf_i, ehf_2se, t_dm, t_dm2)
    }
  }
}

#' Dvoretzky-Kiefer-Wolfowitz inequality
#'
#' Calculate confidence bands for ecdfs using the Dvoretzky-Kiefer-Wolfowitz
#' inequality.
#'
#' @param dat data.frame
#' @param column which column to use
#' @param alpha Desired alpha level
#'
#' @return data.frame with ecdf and confidence bands
#' @export
#'
#' @references Dvoretzky, A., Kiefer, J., Wolfowitz, J., 1956. Asymptotic
#' Minimax Character of the Sample Distribution Function and of the Classical
#' Multinomial Estimator. Ann. Math. Stat. 27, 642-669.
#' doi:10.1214/aoms/1177728174
calc_dkw <- function(dat, column='age', alpha=0.05) {
  if ('sample' %in% names(dat)) {
    sample_name <- dat$sample[1]
  } else {
    sample_name <- NA
  }
  x <- dat[, column]
  x <- x[!is.na(x)]
  y <- stats::ecdf(x)(sort(x))
  n <- length(x)
  x <- c(0, sort(x), 4560)
  y <- c(0, y, 1)
  x_out <- seq(0, 4560)
  y <- stats::approx(x, y, xout=x_out)$y
  epsilon <- sqrt(log(2 / alpha) / (2 * n))
  low <- pmax(y - epsilon, 0)
  high <- pmin(y + epsilon, 1)
  sample <- rep(sample_name, length(x_out))
  data.frame(x=x_out, y=y, low=low, high=high, sample=sample)
}

#' Calculate 1-O
#'
#' @param dat1 data.frame
#' @param dat2 data.frame
#' @param column string of name of column to use ('age' or 't_dm2')
#' @param alpha alpha level
#' @param digits number of digits
#'
#' @return 1-O
#' @export
#' @references Andersen, T., Elburg, M., Cawthorn-Blazeby, A., 2015.
#' U-Pb and Lu-Hf zircon data in young sediments reflect sedimentary recycling
#' in eastern South Africa. J. Geol. Soc. London. 2006-2015.
#' doi:10.1144/jgs2015-006
#'
calc_o_param <- function(dat1, dat2, column, alpha=0.05, digits=2) {
  x <- dat1[, column]
  y <- dat2[, column]
  if (all(is.na(x)) | all(is.na(y))) {
    return(NA)
  }
  else {
    epsilon_one <- sqrt(log(2 / alpha) / (2 * length(x)))
    epsilon_two <- sqrt(log(2 / alpha) / (2 * length(y)))
    x_sort <- sort(x)
    x_y <- stats::ecdf(x_sort)(x_sort)
    y_sort <- sort(y)
    y_y <- stats::ecdf(y_sort)(y_sort)
    x_sort <- c(0, x_sort, 4560)
    x_y <- c(0, x_y, 1)
    y_sort <- c(0, y_sort, 4560)
    y_y <- c(0, y_y, 1)
    x_out <- seq(0, 4560)
    interpolated_one <- stats::approx(x_sort, x_y, xout=x_out)$y
    interpolated_two <- stats::approx(y_sort, y_y, xout=x_out)$y
    interpolated_one_low <- pmax(interpolated_one - epsilon_one, 0)
    interpolated_one_up <- pmin(interpolated_one + epsilon_one, 1)
    interpolated_two_low <- pmax(interpolated_two - epsilon_two, 0)
    interpolated_two_up <- pmin(interpolated_two + epsilon_two, 1)
    up <- ifelse((((interpolated_two_up <= interpolated_one_up) &
                     (interpolated_two_up >= interpolated_one_low)) |
                    ((interpolated_one_up <= interpolated_two_up) &
                       (interpolated_one_up >= interpolated_two_low))), 1, 0)
    low <- ifelse((((interpolated_two_low >= interpolated_one_low) &
                      (interpolated_two_low <= interpolated_one_up)) |
                     ((interpolated_one_low >= interpolated_two_low) &
                        (interpolated_one_low <= interpolated_two_up))), 1, 0)
    O <- (length(up[up == 1]) + length(low[low == 1])) / (2 * length(x_out))
    round(1 - O, digits)
  }
  # delta <- epsilon_one + epsilon_two
  # absolute <- pmax((abs(interpolated_one - interpolated_two) - delta), 0)
  # absolute <- absolute[absolute != 0]
  # round(1- (1 - length(absolute) / length(x_out)), digits)
}

#' Populate matrix with age 1-O
#'
#' @param dat data.frame
#' @param alpha alpha level
#' @param digits number of digits
#'
#' @return matrix of 1-O for ages
#' @export
#' @references Andersen, T., Elburg, M., Cawthorn-Blazeby, A., 2015.
#' U-Pb and LuHf zircon data in young sediments reflect sedimentary recycling
#' in eastern South Africa. J. Geol. Soc. London. 2006-2015.
#' doi:10.1144/jgs2015-006
o_param_matrix_age <- function(dat, alpha=0.05, digits=2) {
  populate_matrix(dat, FUN=calc_o_param, column='age', alpha=alpha,
                  digits=digits)
}

#' Populate matrix with model age 1-O
#'
#' @param dat data.frame
#' @param alpha alpha level
#' @param digits number of digits
#'
#' @return matrix of 1-O for model ages
#' @export
#' @references Andersen, T., Elburg, M., Cawthorn-Blazeby, A., 2015.
#' U-Pb and Lu-Hf zircon data in young sediments reflect sedimentary recycling
#' in eastern South Africa. J. Geol. Soc. London. 2006-2015.
#' doi:10.1144/jgs2015-006
o_param_matrix_tdm <- function(dat, alpha=0.05, digits=2) {
  populate_matrix(dat, FUN=calc_o_param, column='t_dm2', alpha=alpha,
                  digits=digits)
}

#' Combine two square matrices
#'
#' @param mat1 Matrix for upper triangle
#' @param mat2 Matrix for lower triangle
#'
#' @export
#'
combine_matrices <- function(mat1, mat2) {
  mat1[lower.tri(mat1)] <- NA
  mat1[is.na(mat1)] <- mat2[is.na(mat1)]
  mat1
}

#' Produce CHUR and DM lines
#'
#' Calculate CHUR and DM lines used for epsilon-Hf vs. age and 176/177Hf vs. age
#' plots.
#'
#' @param range range over which to calculate lines
#' @param plot_type 'ehf' = epsilon-Hf; any thing else gives 176/177Hf
#' @param constants vector of constants which must be in the order
#' decay constant 176Lu, 176/177Hf CHUR, 176Lu/177Hf CHUR, 176/177Hf DM and
#' 176Lu/177Hf DM
#'
#' @export
#'
#' @references Griffin, W., Pearson, N., Belousova, E., Jackson, S., van
#' Achterbergh, E., O'Reilly, S. and Shee, S. 2000. The Hf isotope composition
#' of cratonic mantle:
#' LAM-MC-ICPMS analysis of zircon megacrysts in kimberlites.
#' Geochimica et Cosmochimica Acta 64(1), 133-147.
#' @references Soderlund, U., Jonathan Patchett, P., Vervoort, J.D. and Isachsen
#' C.E. 2004. The 176Lu decay constant determined by Lu-Hf and U-Pb isotope
#' systematics of Precambrian mafic intrusions. Earth and Planetary Science
#' Letters 219, 311-324.
#' @references Bouvier, A., Vervoort, J.D. and Jonathan Patchett P. 2008. The
#' Lu-Hf and Sm-Nd isotopic composition of CHUR: Constraints from unequilibrated
#' chondrites and implications for the bulk composition of terrestrial planets.
#' Earth and Planetary Science Letters 273, 48-57.
hf_lines <- function(range=c(0, 4560), plot_type='ehf', constants) {
  lambda_lu <- constants[1]
  hfhf_chur <- constants[2]
  luhf_chur <- constants[3]
  hfhf_dm <- constants[4]
  luhf_dm <- constants[5]
  x <- seq(range[1], range[2], by=1)
  length_x <- length(x)
  #CHUR (Bouvier,2008)
  chur <- hfhf_chur - luhf_chur * ((exp(lambda_lu * x * 10^6)) - 1)
  #DM (Griffin,2000)
  dm <- hfhf_dm - luhf_dm *((exp(lambda_lu * x * 10^6)) - 1)
  if (plot_type == 'ehf') {
    dm <- ((dm / chur) - 1) * 10^4
    chur <- rep(0, length_x)
  }
  type_chur <- rep('CHUR', length_x)
  type_dm <- rep('DM', length_x)
  type <- c(type_chur, type_dm)
  x <- rep(x, 2)
  line <- c(chur, dm)
  linedata <- data.frame(x=x, line=line, type=type)
}


#' Calculate quantiles
#'
#' Split up data.frame by sample-column and calculate quantiles
#'
#' @param dat data.frame
#' @param column which column in data.frame to use
#' @param alpha alpha-level (not yet used)
#' @param type type of quantile calculation (passed on to stats::quantile)
#'
#' @export
#'
calc_quantiles <- function(dat, column='t_dm2', alpha=0.05, type=8) {
  x <- dat[, column]
  quantiles <- stats::aggregate(x=x, by=list(dat$sample),
                                FUN=stats::quantile, type=type, na.rm=TRUE)
  x <- as.data.frame(quantiles$x)
  x$iqr <- x$`75%` - x$`25%`
  x$sample <- quantiles$Group.1
  names(x) <- c('zero', 'twentyfive', 'fifty', 'seventyfive', 'onehundred',
                'iqr', 'sample')
  return(x)
}

#' Calculate confidence bands for lower and upper quartile
#'
#' @param dat data.frame
#' @param column column to use for calculations
#' @param alpha alpha-level
#'
#' @export
#'
quant_bounds <- function(dat, column='t_dm2', alpha=0.05) {
  column <- dat[, column]
  sample <- dat$sample[1]
  if (all(is.na(column))) {
    data.frame(x=NA, y=NA, ymin=NA, ymax=NA, xmin=NA, xmax=NA, sample=sample)
  } else {
    sort_column <- sort(column)
    y <- stats::ecdf(column)(sort(column))
    epsilon <- sqrt(log(2 / alpha) / (2 * length(column)))
    low <- pmax(y - epsilon, 0)
    high <- pmin(y + epsilon, 1)
    sort_age_low <- data.frame(x=sort(column), y=low)
    sort_age_high <- data.frame(x=sort(column), y=high)
    ll <- stats::approx(x=sort_age_low$y, y=sort_age_low$x, xout=c(0.25))$y
    lu <- stats::approx(x=sort_age_low$y, y=sort_age_low$x, xout=c(0.75))$y
    if(is.na(lu)) lu <- max(sort_age_low$x)
    ul <- stats::approx(x=sort_age_high$y, y=sort_age_high$x, xout=c(0.25))$y
    if(is.na(ul)) ul <- min(sort_age_high$x)
    uu <- stats::approx(x=sort_age_high$y, y=sort_age_high$x, xout=c(0.75))$y
    lq_dist <- stats::quantile(column, probs=c(0.25), type=8, na.rm=TRUE)
    uq_dist <- stats::quantile(column, probs=c(0.75), type=8, na.rm=TRUE)
    data.frame(x=lq_dist, y=uq_dist, ymin=ll, ymax=lu, xmin=ul, xmax=uu, sample)
  }
}

#' Calculate mixing model
#'
#' Gaussian mixing model for detrital zircon data, using lower quantile upper
#' quantile plot
#'
#' @param mu1 first mean
#' @param sig1 first standard deviation
#' @param mu2 second mean
#' @param sig2 second standard deviation
#'
#' @examples
#' dzr_mix(500, 50, 1000, 100)
#' @export
#'
dzr_mix <- function(mu1, sig1, mu2, sig2) {
  X <- rep(NA, 100)
  S <- rep(NA, 100)
  lq <- matrix(nrow=11, ncol=100)
  uq <- lq
  for (j in seq(1, 100, 1)) {
    for (i in seq(1, 11, 1)) {
      X[1: (100 - 10 * (i - 1))] <- mu1
      X[(101 - 10 * (i - 1)): 100] <- mu2
      S[1: (100 - 10 * (i - 1))] <- sig1
      S[(101 - 10 * (i - 1)): 100] <- sig2

      x <- stats::rnorm(100, mean=X, sd=S)
      y <- stats::ecdf(x)(x)
      lq[i, j] <- stats::quantile(x, probs=0.25, type=8)
      uq[i, j] <- stats::quantile(x, probs=0.75, type=8)
    }
  }
  data.frame(lq=rowMeans(lq), uq=rowMeans(uq))
}

#' Ready 1-O matrix for tile plot
#'
#' @param x 1-O parameter vector
#'
#' @export
tile_func <- function(x) {
  if (is.na(x)) {
    return(NA)
  }
  if (x >= 0.05) {
    return(2)
  }
  if (x > 0 & x < 0.05) {
    return(1)
  } else {
    return(x)
  }
}

#' Apply tile_func to vector
#'
#' @param z 1-O parameter vector
#'
#' @export
#'
tiling <- function(z) {
  sapply(z, tile_func)
}

#' Produce data.frame of 1-O matrix suitable for geom_tile
#'
#' @param dat data.frame
#' @param type What to calculate
#'
#' @export
#'
make_tiling <- function(dat, type) {
  if (type == 'age') {
    mat <- o_param_matrix_age(dat)
    mat <- mat[, rev(seq_len(ncol(mat)))]
    tile_mat <- data.frame(x=rownames(mat)[row(mat)], y=colnames(mat)[col(mat)],
                           z=as.factor(tiling(c(mat))))
    tile_mat$x <- factor(tile_mat$x, levels=rownames(mat))
    tile_mat$y <- factor(tile_mat$y, levels=colnames(mat))
  }
  if (type == 'tdm') {
    mat <- o_param_matrix_tdm(dat)
    mat <- mat[, rev(seq_len(ncol(mat)))]
    tile_mat <- data.frame(x=rownames(mat)[row(mat)], y=colnames(mat)[col(mat)],
                           z=as.factor(tiling(c(mat))))
    tile_mat$x <- factor(tile_mat$x, levels=rownames(mat))
    tile_mat$y <- factor(tile_mat$y, levels=colnames(mat))
  }
  if (type == 'combine') {
    mat_age <- o_param_matrix_age(dat)
    mat_tdm <- o_param_matrix_tdm(dat)
    mat_age[lower.tri(mat_age)] <- NA
    mat <- mat_age
    mat[is.na(mat)] <- mat_tdm[is.na(mat)]
    mat <- t(mat)
    mat <- mat[, rev(seq_len(ncol(mat)))]
    tile_mat <- data.frame(x=rownames(mat)[row(mat)], y=colnames(mat)[col(mat)],
                           z=as.factor(tiling(c(mat))))
    tile_mat$x <- factor(tile_mat$x, levels=rownames(mat))
    tile_mat$y <- factor(tile_mat$y, levels=colnames(mat))
  }
  tile_mat
}

#' Calculate upper and lower concordia intercepts from discordant detrital zircon data
#'
#' @param dat data.frame
#' @param step Chord spacing
#'
#' @export
#'
#'@references Reimink, J. R., Davies, J. H. F. L., Waldron, J. W. F., & Rojas, X. (2016).
#' Dealing with discordance: a novel approach for analysing U–Pb detrital zircon datasets.
#' Journal of the Geological Society, 2015–114. https://doi.org/10.1144/jgs2015-114
reimink <- function(dat, step=5) {
  t1 <- seq(0, 4500 - step, step)
  t2 <- seq(step, 4500, step)
  grid <- expand.grid(t1, t2)
  grid <- grid[grid$Var2 > grid$Var1,]
  dat$sigma75 <- mean(dat$sigma75)
  dat$sigma68 <- mean(dat$sigma68)
  result <- calc_p_apply(dat, grid$Var2, grid$Var1)
  lower <- aggregate(result$p_disc, by=list(result$t1), max)
  upper <- aggregate(result$p_disc, by=list(result$t2), max)
  names(lower) <- c('x', 'y')
  names(upper) <- c('x', 'y')
  lower$x <- as.numeric(as.character(lower$x))
  upper$x <- as.numeric(as.character(upper$x))
  lower$type <- rep('lower', nrow(lower))
  upper$type <- rep('upper', nrow(upper))
  out <- rbind(lower, upper)
}


#' Calculate intercepts and associated p-value
#'
#' @param dat data.frame
#' @param t2 upper intercept age
#' @param t1 lower intercept age
#'
#' @export
#'
calc_p_apply <- function(dat, t2, t1) {
  lambda238 <- 1.55125 * (10^-10)
  X <- dat$r75
  Y <- dat$r68
  sigma75 <- dat$sigma75
  sigma68 <- dat$sigma68
  rho <- dat$rho
  age <- dat$age * 10^6
  disc <- 1 - (Y / ((exp(lambda238 * age)-1)))
  ab <- calc_ab(t2, t1)
  slope <- ab$a
  yintercept <- ab$b
  n <- nrow(dat)
  p_disc <- rep(NA, length(slope))
  for (i in seq_len(length(slope))) {
    S <- atan2((2 * rho * sigma75 * sigma68) / (sigma75^2 - sigma68^2), 2)
    a <- ((slope[i] * cos(S)) - sin(S)) / (cos(S) + (slope[i] * sin(S)))
    b <- (yintercept[i] + (slope[i] * X) - Y) / (cos(S) + (slope[i] * sin(S)))
    B <- (sigma75 / sigma68) * a
    A <- b / sigma68
    p <- (1 / (2 * pi * sigma75 * sigma68)) * exp(-0.5 * (A^2 / (1 + B^2)))
    p <- p / n
    p_disc[i] <- sum(p * disc)
  }
  return(data.frame(t1, t2, p_disc))
}


#' Calculate slope and intercept
#'
#' @param t2 upper intercept
#' @param t1 lower intercept
#'
#' @export
#'
calc_ab <- function (t2, t1) {
  y2 <- concY(t2)
  y1 <- concY(t1)
  x2 <- concX(t2)
  x1 <- concX(t1)
  a = (y2 - y1) / (x2 - x1)
  b = y2 - a * x2
  ab <- data.frame(a=a, b=b)
}

#' Calculate U235 at given age
#'
#' @param age input age
#'
#' @export
#'
concX <- function(age) {
  age <- age * 10^6
  lambda235 <- 9.8485 * (10^-10)
  cx <- exp(lambda235*age) - 1
  return(cx)

}

#' Calculate U238 at given age
#'
#' @param age
#'
#' @export
#'
concY <- function(age) {
  age <- age * 10^6
  lambda238 <- 1.55125 * (10^-10)
  cy <- exp(lambda238*age) - 1
  return(cy)
}
