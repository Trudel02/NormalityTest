#' test.normality
#'
#' `test.normality` permet de faire le test de normalité des variables
#' numeriques d'un dataframe.
#'
#' @param x tested variable
#'
#' @param output if "none", nothing is displayed, if "figure", a figure is displayed, if "message" a message is displayed, and  if "all", a message and a figure are displayed. Try and see !
#'
#' @param var.name the name to be displayed. The default value is the varname of x. Try and see !
#'
#' @return retourne une liste de deux elements a savoir:
#' * `pval` p-value of the normality test
#' * `result` text containing the results from the normality test
#'
#' @importFrom graphics par
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @importFrom graphics hist
#' @importFrom graphics curve
#' @importFrom stats qqnorm
#' @importFrom stats qqline
#' @importFrom graphics legend
#' @importFrom graphics mtext
#' @importFrom stats shapiro.test


test.normality <- function(x, output = c("none", "figure", "message", "all"), var.name = deparse(substitute(x))) {
  print(var.name)
  output <- match.arg(output)

  if (!is.numeric(x)) {
    text <- paste0("The variable ", var.name, " is not numerical")
    if (output %in% c("message", "all")) {
      cat(text, "\n \n")
    }
    return(list(pval = NA, result = text))
  }
  if (output %in% c("figure", "all")) {
    par(mfrow = c(1, 2))
    test <- dnorm(x, mean = mean(x), sd = sd(x))
    histo <- hist(x, plot = FALSE)
    limiteY <- max(test, histo$density)
    hist(x, prob = TRUE, col = "lightgrey", main = "", plot = TRUE, ylim = c(0, limiteY), xlab = "", ylab = "Density")
    curve(dnorm(x, mean = mean(x), sd = sd(x)), col = "blue", add = TRUE)
    qqnorm(x, main = "")
    qqline(x)
    legend("topleft", legend = paste(c("Mean:", "Standard deviation:"), round(c(mean(x), sd(x)), 2)))
    mtext(paste("Normality test for the variable", var.name), 3, outer = TRUE, line = -2, cex = 1.5)
  }
  pvalue <- shapiro.test(x)$p.value
  if (pvalue > .05) {
    text <- paste0("The variable ", var.name, " is normal according to the Shapiro Wilk test (p-value=", round(pvalue, 5), ")")
  } else {
    text <- paste0("The variable ", var.name, " is not normal according to the Shapiro Wilk test (p-value=", round(pvalue, 5), ")")
  }
  if (output %in% c("message", "all")) {
    cat(text, "\n \n")
  }
  return(list(pval = pvalue, result = text))
}

#' normality.df
#'
#' `normality.df` permet de faire le test de normalité de toutes les variables
#' numeriques d'un dataframe.
#'
#' @param df the data frame to be tested
#'
#' @param output if "none", nothing is displayed, if "figure", a figure is displayed, if "message" a message is displayed, and  if "all", a message and a figure are displayed. Try and see !
#'
#' @return retourne une liste de deux elements a savoir:
#' * `pval` p-value of the normality test
#' * `result` text containing the results from the normality test
#'
#'
#' @examples
#'
#' # Example 1:
#' #---------------------------------------------
#' # apply normality.df to a data.frame
#' #---------------------------------------------
#' normality.df(diamonds2, output = "all")
#'
#' @export

normality.df <- function(df, output = c("none", "figure", "message", "all")) {
  try(df<-data.frame(df), silent = TRUE)
  if (!exists("df", mode = "list")) stop("Please enter a valid data.frame!")
  output <- match.arg(output)
  pvalues <- c()
  results <- c()
  for (i in 1:ncol(df)) {
    res <- test.normality(df[, i], output = output, var.name = names(df)[i])
    pvaluei <- res$pval
    resulti <- res$result
    names(pvaluei) <- names(df)[i]
    names(resulti) <- names(df)[i]
    pvalues <- c(pvalues, pvaluei)

    results <- c(results, resulti)
  }
  return(list(pvalues = pvalues, results = results))
}




