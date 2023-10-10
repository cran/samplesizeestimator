#' Sample size for testing relative risk
#' @description
#' When we try to associate multiple exposures to an outcome, we need to know the
#' relative risk (RR) of a particular exposure in the presence of other exposures
#' and test their importance in the model using a significance test based on RR.
#' This function computes sample size based on testing RR for a cohort study design.
#'
#' @param RR anticipated relative risk
#' @param p0 probability of outcome among the unexposed
#' @param alp level of significance or accepted level of probability of type I error
#' @param pwr desired level of power
#' @param k number of unexposed for each exposed
#' @import stringi stats
#' @export
#' @return a list object with minimum required  sample size along with description for reporting
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @examples
#' testRR(p0 = 0.2, RR = 1.5, alp = 0.05, pwr = 0.84, k = 1)
testRR <- function(RR, p0, alp, pwr, k = 1) {

  p1 <- RR * p0
  p <- (p1 + k * p0) / (1 + k)
  n <- (qnorm(1 - alp / 2) * sqrt(p * (1 - p) * (1 + 1 / k)) + (qnorm(pwr) * sqrt(p1 * (1 - p1) + p0 * (1 - p0) / k)))^2 / (p1 - p0)^2
  n <- ceiling(n)
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is not installed.")
  }
  s <- stringi::stri_paste("Description: The study would require ",
                  n,
                  " exposed with ",
                  k,
                  " unexposed(s) per exposed, if an anticipated relative risk of ",
                  RR,
                  " is considered important with power ",
                  pwr*100,
                  " percentage")
  out <- list(Sample_Size = n,stringi::stri_pad(stringi::stri_wrap(s), side='both'))
  return(out)
}
