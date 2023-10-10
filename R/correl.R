#' @title Sample Size for Testing Correlation Coefficient
#' @description
#' Calculates minimum sample size needed to detect at
#' least rho0-rho1 units difference in the hypothesized and reported correlation
#' coefficient for desired level of significance and power
#'
#' @param rho0 magnitude of relationship between the two variables under study, set at null hypothesis
#' @param rho1 anticipated magnitude of relationship between the two variables under study
#' @param alp level of significance or accepted level of probability of type I error
#' @param pwr desired level of power
#'
#' @return a list object with minimum required sample size along with description for reporting
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @import stringi stats
#' @export
#' @examples correl(rho0 = 0.5, rho1 = 0.7, alp = 0.05, pwr = 0.8)
#' @references Bujang, M. A., & Baharum, N. (2016). Sample size guideline for correlation analysis. World Journal of Social Science, 3(1), 37-46.
correl <- function(rho0, rho1, alp, pwr) {
  n <- ceiling((qnorm(1 - alp / 2) + qnorm(pwr))^2 / (0.5 * log((1 + rho1) / (1 - rho1)) - 0.5 * log((1 + rho0) / (1 - rho0))))
    if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is not installed.")
  }
  s<-stringi::stri_paste("Description: Sample size is estimated with an expected correlation coefficient between X and Y as ",
                rho1,
                " with an expected correlation at the population level as ",
                rho0,
                " at ",
                alp*100,
                "% significance level and ",
                pwr*100,
                "% power.The estimated sample size is ",
                n, "\n")
  out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
  return(out)
}
