#' Sample size for estimating Area Under the ROC curve
#'
#' @param auc anticipated AUC of the diagnostic marker or test
#' @param alp level of significance or accepted level of probability of type I error
#' @param d Precision required on either side of the true AUC
#' @import stringi stats
#' @return a list of total sample size based on AUC along with reporting
#' @export
#' @examples est.auc(auc=0.7,alp=0.05,d=0.07)
#' @references Hajian-Tilaki, K. (2014). Sample size estimation in diagnostic test studies of biomedical informatics. Journal of biomedical informatics, 48, 193-204.

est.auc<-function(auc, alp, d){
  a<-qnorm(auc)
  vauc<-(0.0099*exp(-a^2/2))*(6*a^2+16)
  n<-ceiling((qnorm(1-alp/2)*vauc)/d^2)
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is not installed.")
  }
  s <- stringi::stri_paste("Description: Sample size is estimated using the 'sample size for estimating AUC'.
        Considering an anticipated AUC of the diagnostic marker  as ",
                  auc, " with ", alp*100,  " % (two-tailed) level of significance, the estimated total sample size is ", n , ".\n")
  out<-list(Sample_Size = n,stringi::stri_pad(stringi::stri_wrap(s), side='both'))
  return(out)
}

