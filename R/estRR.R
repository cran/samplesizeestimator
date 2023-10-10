#' Sample size for estimation of Relative Risk
#' @description
#' Relative risks are estimated in a cohort study design to assess the association of
#' exposure with the outcome. This function estimates the sample size needed to estimate the true relative
#' risk with specified precision.
#'
#' @param p0 Probability of outcome among unexposed
#' @param RR anticipated Relative Risk (RR)
#' @param alp level of significance or probability of claiming
#' the association exists when in fact there is no association
#' @param prec Precision desired on either side of RR
#' @param k the number of unexposed for each exposed
#' @import stringi stats
#' @export
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @return a list object, the required minimum sample size along with description for reporting
#' @examples
#' estRR(p0=0.2, RR=2, alp=0.05, prec=0.25, k=1)
estRR <- function(p0, RR, alp, prec, k) {

  p1 <- (RR * p0)
  n <- ((qnorm(1 - alp / 2))^2 / (log(1 - prec))^2) * (((1 - p0) / (k * p0 )) + ((1 - p1) / p1))
  n <- ceiling(n)
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is not installed.")
  }
  s<-stringi::stri_paste("Description: \n The study would require",
      n,
      " exposed with ",
      k,
      " unexposed per exposed, in order to assure that the estimated relative
      risk will not exceed the true relative risk by not more than",
      prec * 100,
      "% with 95% confidence level.\n")
  out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
  return(out)
}
