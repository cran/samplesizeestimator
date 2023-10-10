#' Sample size for testing Odds Ratio
#' @description When we try to associate multiple exposures to an outcome, we need
#' to caluclate the odds ratio (OR) of a particular exposure in the presence of other exposures
#' and test their relative importance in the model using a significance test based on OR. This
#' function computes sample size based on testing OR for a case-control study design
#' @param p0 Probability of exposure among the controls
#' @param or Anticipated Odds Ratio
#' @param alp Probability of type I error
#' @param pwr Desired level of power
#' @param k ratio of number of cases to controls to cases
#' @import stringi stats
#' @export
#' @return a list object, the required minimum sample size along with description for reporting
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @examples testor(p0=0.042,or=2.5,alp=0.05,pwr=0.8,k=1)
#' @references Lwanga, S. K., Lemeshow, S., & World Health Organization. (1991). Sample size determination in health studies: a practical manual. World Health Organization.

testor<-function(p0,or,alp,pwr,k)
{

  p1=(or*p0)/(or*p0+(1-p0))
  p=(p1+k*p0)/(1+k)
  n <-(qnorm(1-alp/2)*sqrt(p*(1-p)*(1+1/k))+(qnorm(pwr)*sqrt(p1*(1-p1)+p0*(1-p0)/k)))^2/(p1-p0)^2
  n<-ceiling(n)
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is not installed.")
  }
  s <- stringi::stri_paste("Description: The study would require ",
  n,
  " cases with ",
  k,
  " control(s) per case, if an anticipated odds ratio of ",
  or,
  " is considered important with power ",
  pwr*100,
  " percentage")
  out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
  return(out)
}
