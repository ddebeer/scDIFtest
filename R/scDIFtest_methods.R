#' Methods for class "scDIFtest"
#'
#'
#'
#'
#' @param x an object of class \code{scDIFtest}
#' @param item_selection either \code{NULL} or an integer vector selecting the
#'         item numbers. When \code{items = NULL} (the default), the DIF test
#'         is done for all items.
#' @param ... other argements passed to the method.
#'
#' @importFrom graphics plot
#' @exportMethod print.scDIFtest
#' @exportMethod summary.scDIFtest
#' @exportMethod plot.scDIFtest
#'
#'
#'

# print method
print.scDIFtest <- function(x, item_selection = NULL, ...){

  tests <- x$tests
  test_info <- x$info$test_info

  if(is.null(item_selection)){
    out <- summary(x, ...)
    cat("\n")
    cat(strwrap(paste0("Score Based DIF-tests for ", dim(out)[1], " items"),
                prefix = "\t"), sep = "\n")
    cat(strwrap(paste0("Person covariate: ", test_info$order_name),
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


# summary method
summary.scDIFtest <- function(x, method = "fdr", ...){
  tests <- x$tests
  item_info <- x$info$item_info
  summary <- as.data.frame(do.call(rbind, lapply(tests, function(test)
    unlist(test$single_test[1:2]))))


  summary <- cbind(item_type = item_info$item_type ,
                   n_est_pars = sapply(item_info$colNrs, length),
                   summary, p.adjust(summary[,2], method = method))
  names(summary) <- c("item_type", "n_est_pars", "statistic", "p.value",
                      paste0("p.", method))
  summary
}


# plot method
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

