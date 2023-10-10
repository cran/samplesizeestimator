#' Sample Size for Estimation of Single Proportion
#' @description
#' This function may be used in case of a descriptive study design where the researcher
#' wishes to describe the distribution of one or more categorical outcome variables without looking at their
#' causal relationship and hypothesis testing.
#'
#'
#' @param prop Anticipated proportion of outcome or characteristic of interest in the population
#' @param prec Precision required on either side of the population proportion
#' @param alp Level of significance or accepted level of probability of type I error
#' @param relative a logical argument indicating relative or absolute precision (\code{FALSE} gives absolute precision)
#' @references Lwanga, S. K., Lemeshow, S., & World Health Organization. (1991). Sample size determination in health studies: a practical manual. World Health Organization.
#' @import stringi stats
#' @export
#' @return a list object with minimum required sample size along with description for reporting
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @examples estp(prop = 0.8, prec = 0.1, alp = 0.01, relative = FALSE)
estp <- function(prop, prec, alp=0.05, relative = FALSE) {

  if (!is.null(prop) && !is.null(prec) && !is.null(alp)) {
    dp <- c()
    dm <- c()
    pp <- c()
    pm <- c()
    eve <- 0.0
    for (i in 1:5) {
      dp[i] <- prec + eve
      dm[i] <- prec - eve
      pp[i] <- prop + eve
      pm[i] <- prop - eve
      eve <- eve + 0.01
    }
    D <- sort(unique(c(dm, dp)))
    P <- sort(unique(c(pm, pp)))
    if (relative == FALSE) {
      n <- ceiling((qnorm((1 - alp / 2))^2 * prop * (1 - prop)) / prec^2)
      A <- matrix(nrow=length(D), ncol=length(P))
      for (i in 1:length(D)) {
        for (j in 1:length(P)) {
          A[i, j] <- ceiling((qnorm((1 - alp / 2))^2 * P[j] * (1 - P[j])) / D[i]^2)
        }
      }
      rownames(A) <- D
      colnames(A) <- P
      names(dimnames(A)) <- c("precision", "Anticipated Proportion")
      if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("Package 'stringi' is not installed.")
      }
      s<-stringi::stri_paste("Description: \n Sample size is estimated with an expected proportion of 'outcome of interest' \n among the 'population' as ", prop*100, " % with ", prec*100," % Absolute precision and \n",alp*100, "% significance level. The estimated sample size is ", n, "\n")
      out <- list(Sample_Size = n,
                  Table_of_Sample_Size = A, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
      return(out)
    }
    else
    {
      n <- ceiling((qnorm((1 - alp / 2))^2 * prop * (1 - prop)) / (prec * prop)^2)
      A <- matrix(nrow=length(D), ncol=length(P))
      for (i in 1:length(D)) {
        for (j in 1:length(P)) {
          A[i, j] <- ceiling((qnorm((1 - alp / 2))^2 * P[j] * (1 - P[j])) / (D[i] * P[j])^2)
        }
      }
      rownames(A) <- D
      colnames(A) <- P
      names(dimnames(A)) <- c("precision", "Anticipated Proportion")
      if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("Package 'stringi' is not installed.")
      }
      s <- stringi::stri_paste("Description: \n Sample size is estimated with an expected proportion of 'outcome of interest' \n among the 'population' as ", prop*100, "% with ", prec*100,"% Relative precision and \n",alp*100, "% significance level. The estimated sample size is ", n, "\n")
      out <- list(Sample_Size = n,
                  Table_of_Sample_Size = A, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
      return(out)
      }
}
  else
  {
    return("The arguments 'prop','prec','alp' must be non-NULL")
  }
  }

