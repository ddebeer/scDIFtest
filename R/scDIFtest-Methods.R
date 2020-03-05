#' Methods for the scDIFtest-class
#'
#' \code{print}, \code{summary}, and \code{plot} methods for objects of the
#' \code{scDIFtest-class}, as returned by \code{\link{scDIFtest}}. See details
#' for more information about the methods.
#'
#' The \code{print} method, when\code{item_selection = NULL}, gives a summary
#' of all the tests that were executed (i.e., for all items). When specific
#' items are selected, the \code{print} method is called repeatedly for each
#' individual \code{sctest} corresponding with the selected items.
#'
#' The \code{summary} method computes a data frame with a row for each item
#' that was included in the test. The columns are:
#'   \describe{
#'      \item{item_type}{The estimated IRT model per item}
#'      \item{n_est_pars}{The number of estimated parameters per item}
#'      \item{stat}{The value for the used statistic per item}
#'      \item{p_value}{The p-value per item}
#'      \item{p_fdr}{The corrected p-value controling the false discovery rate
#'      (Benjamini & Hochberg, 1995). See \code{\link[stats]{p.adjust}} for
#'      details.}
#'    }
#'
#'  The \code{plot} method call the \code{plot} method repeatedly for the
#'  \code{gepf} that corresponds with the exicuted score test for each of the
#'  selected items. When no items are selected, the \code{plot} method results
#'  in an error.
#'
#' @references Benjamini, Y., and Hochberg, Y. (1995). Controlling the false
#' discovery rate: a practical and powerful approach to multiple testing.
#' \emph{Journal of the Royal Statistical Society Series B, 57,} 289-300.
#' \url{http://www.jstor.org/stable/2346101.}
#'
#'
#' @param x an object of class \code{scDIFtest}
#' @param object an object of class \code{scDIFtest}
#' @param item_selection either \code{NULL} or an integer vector selecting the
#'         item numbers. When \code{items = NULL} (the default), the DIF test
#'         is done for all items.
#' @param method one of the strings in \code{p.adjust.methods}.
#' @param ... other argements passed to the method.
#'
#' @importFrom graphics plot
#' @importFrom stats p.adjust
#'
#' @name scDIFtest-Methods
#'
#' @rdname scDIFtest-Methods
#' @name print.scDIFtest
#' @export
print.scDIFtest <- function(x, item_selection = NULL, ...){

  tests <- x$tests
  test_info <- x$info$test_info

  if(is.null(item_selection)){
    out <- summary(x, ...)
    cat("\n")
    cat(strwrap(paste0("Score Based DIF-tests for ", dim(out)[1], " items"),
                prefix = "\t"), sep = "\n")
    cat(strwrap(paste0("Person covariate: ", test_info$DIF_covariate_name),
                prefix = "\t"), sep = "\n")
    cat(strwrap(paste0("Test statistic type: ", test_info$stat_name),
                prefix = "\t"), sep = "\n")
    cat("\n")
    print(out, ...)


  } else {

    item_names <- names(tests)
    if(is.numeric(item_selection)) item_selection <- item_names[item_selection]
    if(length(item_selection) == 1 && item_selection == "all") item_selection <- item_names
    stopifnot(item_selection %in% item_names)

    for(item in item_selection){
      single_test <- tests[[item]]$single_test
#      cat("\n")
      cat(strwrap(paste0("DIF-test for ", item), prefix = "\t"), sep = "\n")
      cat(strwrap(paste0("Person covariate: ", test_info$order_name), prefix = "\t"), sep = "\n")
      cat(strwrap(paste0("Test statistic type: ", test_info$stat_name), prefix = "\t"), sep = "\n")
      print(single_test,...)
    }
  }
}


#' @rdname scDIFtest-Methods
#' @name summary.scDIFtest
#' @export
summary.scDIFtest <- function(object, method = "fdr", ...){
  tests <- object$tests
  item_info <- object$info$item_info
  summary <- as.data.frame(do.call(rbind, lapply(tests, function(test)
    unlist(test$single_test[1:2]))))


  summary <- cbind(item_type = item_info$item_type ,
                   n_est_pars = sapply(item_info$colNrs, length),
                   summary, p.adjust(summary[,2], method = method))
  names(summary) <- c("item_type", "n_est_pars", "stat", "p_value",
                      paste0("p_", method))
  summary
}


#' @rdname scDIFtest-Methods
#' @name plot.scDIFtest
#' @export
plot.scDIFtest <- function(x, item_selection = NULL, ...){
  if(is.null(item_selection))
    stop("Choose an item for which the empirical fluctuation process shoud ",
         "be plotted using the 'item_selection' argument.")
  tests <- x$tests
  item_names <- names(tests)
  if(is.numeric(item_selection)) item_selection <- item_names[item_selection]
  if(length(item_selection) == 1 && item_selection == "all") item_selection <- item_names
  stopifnot(item_selection %in% item_names)

  gefp <- x$gefp
  colNrs_list <- x$info$item_info$colNrs


  for(item in item_selection){
    single_gefp <- gefp
    functional <- x$tests[[item]]$functional
    single_gefp$process <- suppressWarnings(
      gefp$process[, colNrs_list[[item]], drop = FALSE])
    single_gefp$type.name = paste0(gefp$type.name, " for item ", item)
    plot(single_gefp, functional = functional, ...)
  }

}

