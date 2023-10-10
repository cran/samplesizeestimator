#' Sample Size for Comparing Independent and dependent means
#' @description This function computes the sample size based on three different methods
#' i) comparing mean with a specified value
#' ii) comparing two  independent means
#' iii) comparing two dependent means
#' @param delta anticipated difference between the two groups
#' @param sd anticipated standard deviation
#' @param alp anticipated level of significance or accepted level of type I error \code{alp=0.05} is default
#' @param pwr desired power \code{pwr=0.80} is default
#' @param type string specifying the type of sample (one or two) \code{type=two} is default
#' @param alternative one or two sided alternative hypothesis \code{"two.sided"} is default
#' @param k the ratio of control to experimental patients \code{k=1} is default
#' @param paired a logical argument indicating whether the sample is independent or dependent \code{FALSE} is default
#' @import stringi stats
#' @export
#' @return a list object, the required minimum sample size along with description for reporting
#' @author R. Amala, Scientist-C, ICMR-VCRC, Puducherry  & G. Kumarapandiyan, Asst. Prof., Madras Christian College, Chennai
#' @references Lwanga, S. K., Lemeshow, S., & World Health Organization. (1991).
#' Sample size determination in health studies: a practical manual. World Health Organization.
#' @examples n.means(delta = 1.5, sd = 1, alp = 0.05, pwr = 0.9, type ="two",
#' alternative= "two.sided", k = 1, paired = FALSE)
n.means <- function(delta, sd, alp = 0.05, pwr = 0.80, type = "two", alternative = "two.sided", k = 1, paired = FALSE) # nolint
{

  if (type == "two") {
    if (paired == FALSE) {
      if (alternative == "two.sided") {
        n <- ceiling((((1 + k) / k) * (qnorm(1 - alp / 2) + qnorm(pwr))^2 * sd^2) / delta^2)
        if (!requireNamespace("stringi", quietly = TRUE)) {
          stop("Package 'stringi' is not installed.")
        }
        s <- stri_paste(
          "Description: Sample size is estimated using the 'sample size'", # nolint
          "for comparing two independent means'. Considering anticipated difference in the mean of", # nolint
          "the outcome of interest between the groups as ", delta, " units and ", alp * 100, # nolint
          " % (two-tailed) level of significance, the estimated sample size is ", n, ".\n"
        )
        out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
        return(out)
      } else if (alternative == "one.sided") {
        n <- ceiling((((1 + k) / k) * (qnorm(1 - alp) + qnorm(pwr))^2 * sd^2) / delta^2) # nolint
        if (!requireNamespace("stringi", quietly = TRUE)) {
          stop("Package 'stringi' is not installed.")
        }
        s <- stringi::stri_paste(
          "Description: Sample size is estimated using the 'sample size for ", # nolint
          "comparing two independent means'. Considering anticipated difference in the mean of outcome", # nolint
          " of interest between the groups as ",
          delta,
          " units and ",
          alp * 100,
          " % (one-tailed) level of significance, the estimated sample size is ",
          n, ".\n"
        )
        out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
        return(out)
      }
    } else if (paired == TRUE) {
      if (alternative == "two.sided") {
        n <- ceiling(((qnorm(1 - alp / 2) + qnorm(pwr))^2 * sd^2) / delta^2 + (qnorm(1 - alp / 2))^2 / 2) # nolint
        if (!requireNamespace("stringi", quietly = TRUE)) {
          stop("Package 'stringi' is not installed.")
        }
        s <- stringi::stri_paste(
          "Sample Size is calculated based on comparing two dependent means. Anticipating a mean difference of ", delta, # nolint
          " with standard deviation ", sd, " and ", ceiling(alp * 100), " % (two-sided) level of significance and power ", ceiling(pwr * 100), "%."
        ) # nolint
        out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
        return(out)
      }
      else if (alternative == "one.sided") {
        n <- ceiling(((qnorm(1 - alp) + qnorm(pwr))^2 * sd^2) / delta^2 + (qnorm(1 - alp))^2 / 2) # nolint
        if (!requireNamespace("stringi", quietly = TRUE)) {
          stop("Package 'stringi' is not installed.")
        }
        s <- stringi::stri_paste(
          "Sample Size is calculated based on comparing two dependent means. Anticipating a mean difference of ", delta, # nolint
          " with standard deviation ", sd, " and ", ceiling(alp * 100), " % (two-sided) level of significance and power ", ceiling(pwr * 100), "%."
        ) # nolint
        out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
        return(out)
      }
    }
  }
  else if (type == "one") {
    if (alternative == "two.sided") {
      n <- ceiling(((qnorm(1 - alp / 2) + qnorm(pwr))^2 * sd^2) / delta^2) # nolint
      if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("Package 'stringi' is not installed.")
      }
      s <- stringi::stri_paste(
        "Sample Size is calculated based on comparing mean of a population with a
		reference value. A total of ",
        n,
        " subjects is needed to detect ",
        delta,
        " unit difference",
        # nolint
        " between the anticipated and hypothesized mean of the outcome of interest with standard deviation ", # nolint
        sd,
        " and ",
        ceiling(alp * 100),
        " % (two-sided) level of significance and power ",
        ceiling(pwr * 100),
        "%."
      )
      out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
      return(out)
    }
    else if (alternative == "one.sided") {
      n <- ceiling(((qnorm(1 - alp) + qnorm(pwr))^2 * sd^2) / delta^2) # nolint
      if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("Package 'stringi' is not installed.")
      }
      s <- stringi::stri_paste(
        "Sample Size is calculated based on comparing mean of a population with a reference value. A total of ",
        n,
        " subjects is needed to detect ", delta, " unit difference between the anticipated and hypothesized mean of the outcome of interest with SD ", # nolint
        sd,
        " and ",
        ceiling(alp * 100),
        " % (one-sided) level of significance and power ",
        ceiling(pwr * 100),
        "%.")
      out <- list(Sample_Size = n, stringi::stri_pad(stringi::stri_wrap(s), side='both'))
      return(out)
    }
  }
}
