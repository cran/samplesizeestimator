#' Estimate sample size for hypothesis testing on proportions
#' @description
#' This function computes the sample size based on two different methods i) comparing proportion with a specified (reference)
#' value ii) comparing two independent proportions
#'
#' @param p1  hypothesized or reported proportion
#' @param p2  anticipated proportion in the population of interest
#' @param alp level of significance or accepted level of probability of type I error
#' @param pwr desired level of power
#' @param type character string stating number of groups i.e. \code{one} or \code{two} (default)
#' @param alternative a character string specifying the alternative hypothesis, must be one of \code{two.sided} (default) or \code{one.sided}
#' @param k ratio of number of subjects in the two groups \code{k=1} (default)
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @return a list object, the required minimum sample size along with description for reporting
#' @import stringi stats
#' @export
#' @examples
#' nprop(p1=0.5, p2=0.4, alp=0.05, pwr=0.90, type="one",
#' alternative="one.sided", k=1)
#' nprop(p1=0.05, p2=0.15, alp=0.05, pwr=0.90, type="two",
#' alternative="one.sided", k=1)
nprop <- function(p1, p2, alp, pwr, type = "two", alternative = "two.sided", k = 1) {

  if (type == "two") {
    if (alternative == "two.sided") {
      p <- (p1 + p2) / 2
      n <- ceiling((qnorm(1 - alp / 2) * sqrt(p * (1 - p) * (1 + 1 / k)) + qnorm(pwr) * sqrt(p1 * (1 - p1) + p2 * (1 - p2) / k))^2 / (p1 - p2)^2)
      if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("Package 'stringi' is not installed.")
      }
      s <- stringi::stri_paste("Description: Sample size is estimated using the 'sample size for comparing two independent proportions'.
        Considering an anticipated difference in the proportion of outcome of interest between the groups as ",
                      abs((p1 - p2)) * 100,
                      " % with ",
                      alp * 100,
                      " % (two-tailed) level of significance, the estimated sample size is ",
                      n, " in each group with ", k, " as the ratio of controls to experimental subjects.\n")
      out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
      return(out)
    }
    else if (alternative == "one.sided") {
      p <- (p1 + p2) / 2
      n <- ceiling((qnorm(1 - alp) * sqrt(p * (1 - p) * (1 + 1 / k)) + qnorm(pwr) * sqrt(p1 * (1 - p1) + p2 * (1 - p2) / k))^2 / (p1 - p2)^2)
      if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("Package 'stringi' is not installed.")
      }
      s <- stringi::stri_paste("Description: Sample size is estimated using the 'sample size for comparing two independent proportions'.
        Considering an anticipated difference in the proportion of outcome of interest between the groups as ",
                      abs((p1 - p2)) * 100, " % with ",
                      alp * 100,
                      " % (one-tailed) level of significance, the estimated sample size is ",
                      n, " in each group with ", k, " as the ratio of controls to experimental subjects.\n")
      out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
      return(out)
    }
  }
  else if (type == "one") {
    if (alternative == "two.sided") {
      n <- ceiling((qnorm(1 - alp / 2) * sqrt(p1 * (1 - p1)) + qnorm(pwr) * sqrt(p2 * (1 - p2)))^2 / (p1 - p2)^2)

      if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("Package 'stringi' is not installed.")
      }
      s <- stringi::stri_paste(
        "Sample Size is calculated based on sample size for comparing proportion with a # nolint
      reference value. A total of ",
        n,
        "subjects is needed to detect ",
        ceiling(abs(p1 - p2) * 100),
        "% difference between anticipated and reported proportion of event rate among the population of interest, at ",
        ceiling(alp * 100),
        "% (two-sided) level of significance and power",
        ceiling(pwr * 100)
      )
      out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
      return(out)
    } else if (alternative == "one.sided") {
      n <- ceiling((qnorm(1 - alp) * sqrt(p1 * (1 - p1)) + qnorm(pwr) * sqrt(p2 * (1 - p2)))^2 / (p1 - p2)^2)
      if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("Package 'stringi' is not installed.")
      }
            s <- stringi::stri_paste(
        "Description: Sample Size is calculated based on sample size for comparing proportion with a
reference value. A total of ", n, " subjects is needed to detect ", ceiling(abs(p1 - p2) * 100), "% difference
between anticipated and reported proportion of event among the population of interest, at ", ceiling(alp * 100),
        "% (one-sided) level of significance and power ", ceiling(pwr * 100), "%."
      )
      out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
      return(out)
    }
  }
}
