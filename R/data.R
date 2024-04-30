#' Specification curve data from Ambrus et al. (2020)
#'
#' @format A data frame with 121 rows and 10 variables
#' \describe{
#'   \item{outcome}{The major outcome of interest}
#'   \item{beta}{The effect estimate}
#'   \item{pval}{p-value from the effect estimate}
#'   \item{se}{Standard error from the effect estimate}
#'   \item{beta_orig,pval_orig,se_orig}{Original beta/p-value/SE estimate}
#'   \item{analytical_decision_1,analytical_decision_2,analytical_decision_3}{The analytical decisions varied in the reproduction: functional form for outcome 3, the bandwidth selection procedure, and the use of control variables.}
#' }
#' @encoding UTF-8
"cholera_dat"
