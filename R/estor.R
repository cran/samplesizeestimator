#' Sample size for estimation of Odds Ratio with specified precision
#' @description Odds ratios are estimated in a case-control study design to assess the association of
#' outcome with past exposure. This function estimates the sample size needed to estimate the true odds
#' ratio with specified precision.
#' @param p0 Probability of exposure among the controls
#' @param or Anticipated Odds Ratio (OR)
#' @param alp level of significance or probability of claiming the association exists when in fact there is no association
#' @param prec Precision desired on either side of OR
#' @param k the number of controls for each case
#' @import stringi stats
#' @export
#' @return a list object, the required minimum sample size along with description for reporting
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @references Lwanga, S. K., Lemeshow, S., & World Health Organization. (1991). Sample size determination in health studies: a practical manual. World Health Organization.
#' @examples estor(p0 = 0.35, or = 2, alp = 0.05, prec = 0.25, k = 1)
estor <- function(p0, or, alp, prec, k) {
  p1 <- (or * p0) / (or * p0 + (1 - p0))
  n <- ((qnorm(1 - alp / 2))^2 / (log(1 - prec))^2) * ((1 / (k * p0 * (1 - p0))) + (1 / (p1 * (1 - p1))))
  n <- ceiling(n)
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is not installed.")
  }
  s<-stringi::stri_paste("Description: \n The study would require",
  n,
  " cases with ",
  k,
  " control(s) per case, in order to assure that the estimated odds ratio will not exceed the true odds
  ratio by not more than",
  prec * 100,
  "% with 95% confidence level.\n")
  out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
  return(out)
}
