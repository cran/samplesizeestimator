#' Sample Size for Estimating LR negative of a Single Diagnostic Test
#' @description
#' Calculate sample size(cases) based on negative likelihood ratio an unified index for
#' comparing the accuracy of two diagnostic tests
#' @param se anticipated sensitivity of the diagnostic test
#' @param sp anticipated specificity of the diagnostic test
#' @param lrneg anticipated LR negative value
#' @param alp level of significance
#' @param pwr desired level of power
#' @param k number of control(s) per case
#' @import stringi stats
#' @export
#' @return a list object with minimum required sample size with reporting
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @examples
#' LRneg(se=0.9, sp=0.5,lrneg=0.4,alp=0.05, pwr=0.8,k=1)
#' @references Simel, D.L., Samsa, G.P. Matchar, D. B. (1991). Likelihood ratio with
#' confidence: sample size estimation for diagnostic test studies. J Clin Epidemiol. 44: 763-70.
LRneg<-function(se, sp, lrneg, alp, pwr,k=1) {

  p1<-1-se; p2<-sp;
  n<-ceiling((qnorm(1 - alp / 2)*sqrt((1-p1)/p1+ (1-p2)/(k*p2)))^2 / (log(lrneg)-log(p1/p2))^2)
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is not installed.")
  }
  s <- stringi::stri_paste("Description: Sample size is estimated based on estimating a 'negative likelihood ratio' of a single diagnostic test by
         anticipating a sensitivity and specificity of ",
                  se, " and ", sp, " respectively. The study would require ",
                  n,
                  " cases with a two-sided ",
                  (1-alp) * 100,
                  " % confidence interval and ",
                  pwr*100, " % power with ",
                  k, " control(s) per case.\n")
  out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
  return(out)
}
