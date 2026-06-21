#' Replications needed for a target coverage Monte Carlo SE
#'
#' Inverts `MCSE = sqrt(p(1-p)/nsim)` to give the `nsim` such that a coverage
#' cell near `p` has Monte Carlo SE no larger than `target_mcse`.
#'
#' @param target_mcse Positive numeric target Monte Carlo SE.
#' @param p Assumed coverage proportion (default 0.95, the nominal level).
#' @return Integer number of replications (ceiling).
#' @examples
#' medsim_nsim_for_mcse(0.005, 0.95)  # 1900
#' @export
medsim_nsim_for_mcse <- function(target_mcse, p = 0.95) {
  if (!is.numeric(target_mcse) || length(target_mcse) != 1 || target_mcse <= 0) {
    stop("target_mcse must be a single positive number")
  }
  as.integer(ceiling(p * (1 - p) / target_mcse^2 - 1e-10))
}
