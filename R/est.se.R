#' Sample size for estimating sensitivity
#' @description
#' In diagnostic studies, the test yields a binary outcome and accuracy is evaluated by sensitivity
#' and specificity. This function calculates sample size for estimating sensitivity when the diagnostic
#' test yields a binary outcome.
#'
#' @param p Prevalence of disease
#' @param se anticipated sensitivity of the test
#' @param prec Precision required on either side of the true sensitivity
#' @param alp level of significance or accepted level of probability of type I error
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @return a list of total sample size based on sensitivity along with reporting
#' @import stringi stats
#' @export
#' @examples
#' est.se(p = 0.10, se = 0.99, prec = 0.03, alp = 0.05)
#' @references Hajian-Tilaki, K. (2014). Sample size estimation in diagnostic test studies of biomedical informatics. Journal of biomedical informatics, 48, 193-204.
est.se <- function(p, se, prec, alp) {
  n <- ceiling(((qnorm(1 - alp / 2))^2 * se * (1 - se)) / (prec^2 * p))
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is not installed.")
  }

  s<-stringi::stri_paste("Description: \n The study would require a total sample size of",
      n,
      " with anticipated sentivity of ",
      se,
      " and prevalence of disease as",
      p,
      "with marginal error of estimate does not exceed from",
      prec * 100,
      "% with 95% confidence level.\n")
  out <- list(Sample_Size = n,stringi::stri_pad(stringi::stri_wrap(s), side='both'))
  return(out)
}
