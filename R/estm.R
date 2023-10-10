#' Sample size for estimating single mean
#' @description This function computes adequate sample size based on the method of
#' estimating mean with absolute or relative precision. It can be used for descriptive studies
#' where the researcher wishes to describe the distribution of one or more quantitative outcome variables
#' without looking at their causal relationship and hypothesis testing.
#'
#' @param mean anticipated population mean (required if relative precision is desired otherwise not required)
#' @param sig anticipated population standard deviation
#' @param prec desired level of precision on either side of the population mean
#' @param alp level of significance or accepted level of probability of type I error
#' @param relative a logical argument indicating relative or absolute precision (\code{FALSE} gives absolute precision)
#' @import stringi stats
#' @export
#' @return number needed to estimate mean within the desired precision level
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @references
#' Lwanga, S. K., Lemeshow, S., & World Health Organization. (1991). Sample size determination in health studies: a practical manual. World Health Organization.
#' @import stringi
#' @examples
#' estm(sig=6.3,prec=1.2,alp=0.05,relative=FALSE)
#' estm(mean=14,sig=8,prec=0.1,alp=0.05,relative = TRUE)

estm<-function(mean,sig,prec,alp,relative=FALSE) {

  if(relative==FALSE)
  {
    n<-ceiling(( (qnorm(1-alp/2))^2*sig^2)/prec^2)

    if (!requireNamespace("stringi", quietly = TRUE)) {
      stop("Package 'stringi' is not installed.")
    }
    s <- stringi::stri_paste("Description: \n Sample size is estimated with an expected standard deviation of the 'outcome of interest' among the 'population' as ", sig, " units at ", alp*100,  "% level of significance with an absolute precision of ",prec, " units, the estimated sample size is ",n, ".\n")
    out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
    return(out)
  }
  else
  {
    n<-ceiling(( (qnorm(1-alp/2))^2*sig^2)/(mean*prec)^2)

    if (!requireNamespace("stringi", quietly = TRUE)) {
      stop("Package 'stringi' is not installed.")
    }
    s <- stri_paste("Description: Sample size is estimated with an expected standard deviation of the 'outcome of interest' among the 'population' as ", sig, " units at ", alp*100,  "% level of significance with a Relative precision of ",prec, " units, the estimated sample size is ", n , ".\n")
    out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
    return(out)
}
  }


