#' A score-based item-wise DIF test
#'
#' A function that executes item-wise score-based DIF tests. After fitting an
#' IRT model with \code{\link[mirt]{mirt}}, the fitted object can be used to
#' assess and test measurement invariance, using
#' \code{\link[strucchange]{sctest}}. However, by default, all parameters of
#' the fitted model are tested simultaneously. This function applies the
#' \code{\link[strucchange]{sctest}} to test for item-wise DIF, in an efficient
#' way.
#'
#' For more information about the functional see the documentation of
#' \code{\link[strucchange]{sctest.default}} or
#' \code{\link[strucchange]{sctest.formula}}. When \code{functional = NULL}
#' (which is the default), the functional is chosen based on the class of
#' \code{DIF_covariate}. In this case, for \code{integer} and \code{numeric} vectors
#' the Double Maximum (\code{"DM"}) is used; for \code{ordered} vectors the
#' Maximum Lagrange Multiplier Test for Ordered Groups (\code{"maxLMo"}) is
#' used; and for \code{factor}, \code{character}, and \code{logical} vectors the
#' Lagrange Multiplier Test for Unordered Groups is used.
#'
#'
#'
#' @param object a fitted model object of class \code{\link[mirt]{SingleGroupClass-class}}
#'  or \code{\link[mirt]{MultipleGroupClass-class}}, resulting from an IRT analysis
#'  using the \code{mirt}-package.
#' @param DIF_covariate a vector with the person covariate to use for the DIF-test.
#'  The covariate can be categorical, ordered categorical or numerical.
#' @param functional a character specifying the functional (or test statistic) to
#'  be used. See details for more information.
#' @param item_selection either \code{NULL} or an integer vector selecting the item
#' numbers. When \code{items = NULL} (the default), the DIF test is done for all
#' items.
#' @param decorrelate a logical. Should the process be decorrelated?
#' @param ... other arguments passed to the \code{sctest} method.
#'
#' @return An object of class \code{scDIFtest} \link{scDIFtest-Methods}, which is a list with three elements
#'    \describe{
#'      \item{tests}{A named list with a number of elements equal to the number of
#'                   the items for which DIF should be detected. Each element
#'                   contains information both about the test \code{single_test}
#'                   as well as the \code{\link[strucchange]{efpFunctional}}}
#'      \item{info}{A named list with two elements. \code{test_info} contains
#'                  information such as used test statistic and the used covariate.
#'                  \code{item_info} contains information about the items such as
#'                  the item types as well as the column numbers of the score matrix
#'                  that correspond to the estimated parameters of the items.}
#'      \item{gefp}{The Generalized Empirical M-Fluctuation Process (\code{gefp})
#'                  based on the complete model with all the estimated parameters
#'                  (see \code{\link[strucchange]{gefp}}).}
#'    }
#'
#' @examples
#'   # Note that the Generalized Empirical M-Fluctuation Process (gefp) based on all
#'   #   the estimated parameters in the model is an element of the resulting
#'   #   scDIFtest object. This means that one can use this gefp to test the
#'   #   general hypothesis of measurement invariance with respect to the
#'   #   chosen covariate.
#'   \dontrun{
#'   strucchange::sctest(DIF$gefp)
#'   }
#'
#'
#' @export scDIFtest
#' @exportClass scDIFtest
#' @importFrom zoo zoo
#' @importFrom sandwich estfun
#' @importFrom strucchange sctest
#' @importFrom mirt extract.mirt estfun.AllModelClass
#'
scDIFtest <- function(object, DIF_covariate = NULL, functional = NULL, item_selection = NULL,
                      decorrelate = TRUE, ...)
  {
  # extract from mirt-object
  allItemNames <- extract.mirt(object, "itemnames")
  nItem <- extract.mirt(object, "nitems")
  data.name <- 'if'(is.name(object@Call$data), object@Call$data, format(object@Call$data))
  itemtype <- extract.mirt(object, "itemtype")

  # compute the score contributions for all item parameters
  scores <- estfun.AllModelClass(object)

  # get the column numbers of the scores for each whichItem
  colNrs_list <- get_colNrs(object, item_selection)

  # dimensions of the scores: number of contributions, and number of components
  n <- NROW(scores)
  k <- NCOL(scores)

  if (is.null(DIF_covariate))
    DIF_covariate <- seq_len(n)
  DIF_covariate_name <- deparse(substitute(DIF_covariate))
  index <- order(DIF_covariate)
  scores <- scores[index, , drop = FALSE]
  DIF_covariate <- DIF_covariate[index]
  z <- DIF_covariate
  if(is.character(z)) z <- as.factor(z)
  z <- as.numeric(z)

  # compute Information Matrix
  VAR <- (crossprod(scores)) / n
  J12 <- strucchange::root.matrix(VAR)

  # compute cumsums to make process
  process <- rbind(0, scores)
  index <- c(z[1] - as.numeric(diff(z[1:2])), z)
  process <- apply(process, 2, cumsum) / sqrt(n)
  colnames(process) <- colnames(scores)

  # decorrelate
  if (decorrelate)
    process <- process %*% chol2inv(chol(J12))
  else {
    process <- process / outer(rep(1, n), sqrt(diag(VAR)))
  }

  # create a gefp
  gefp <- list(process = suppressWarnings(zoo::zoo(process, index)),
               nreg = k,
               nobs = n,
               call = match.call(),
               fit = object@Fit,
               fitted.model = object,
               scores = scores,
               par = NULL,
               lim.process = "Brownian bridge",
               type.name = "M-fluctuation test",
               order.name = DIF_covariate_name,
               J12 = J12)
  class(gefp) <- "gefp"

  ## set up functional if specified as character
  if(is.null(functional)){
    class <- class(DIF_covariate)
    functional <- if("ordered" %in% class) {
      "maxLMo"
    } else if (class %in% c("factor", "logical", "character")){
      "LMuo"
    } else if (class %in% c("integer", "numeric")) "dm"
  }

  if(is.character(functional)) {
    functional <- tolower(functional)
    functional <- switch(functional,
                         "dmax" = "dm",
                         "maxlm" = "suplm",
                         "mosum" = "maxmosum",
                         functional
    )
    if(is.null(DIF_covariate) & functional %in% c("lmuo", "wdmo", "maxlmo")) {
      stop("'DIF_covariate' must provide a grouping of the observations")
    }
    
  }
  
  stat_name <- 'if'(is.character(functional), functional, substitute(functional))
  
  if(tolower(stat_name) %in% c("dm", "cvm", "maxlm", "lmuo", "wdmo", "maxlmo")){
    stat_name <- switch(stat_name,
                        dm = "Double Maximum Test",
                        cvm =  "Cramer-von Mises Test",
                        maxlm = "Maximum Lagrange Multiplier Test",
                        lmuo = "Lagrange Multiplier Test for Unordered Groups",
                        wdmo = "Weighted Double Maximum Test for Ordered Groups",
                        maxlmo = "Maximum Lagrange Multiplier Test for Ordered Groups")
  }



  ## function that computes the score based statistic for a set of columns of the
  ## cumsum process.
  colNrs_sctest <- function(colNrs, ...){
    functional <- switch(functional,
                         "dm" = strucchange::maxBB,
                         "cvm" = strucchange::meanL2BB,
                         "suplm" = strucchange::supLM(...),
                         "range" = strucchange::rangeBB,
                         "lmuo" = strucchange::catL2BB(factor(DIF_covariate)),
                         "wdmo" = strucchange::ordwmax(factor(DIF_covariate)),
                         "maxlmo" = strucchange::ordL2BB(factor(DIF_covariate), nproc = length(colNrs), nobs = n, ...),
                         "maxmosum" = strucchange::maxMOSUM(...),
                         stop("Unknown efp functional.")
    )

    # functional should be of class "efpFunctional)
    stopifnot(class(functional) == "efpFunctional")

    gefp$process <- suppressWarnings(
      gefp$process[, colNrs, drop = FALSE])
    if(length(colNrs) > 2 & !decorrelate)
      return("No Result: The limiting process for this item is not a Brownian bridge. Use 'decorrelate = TRUE'.")
    single_test <- sctest(gefp, functional = functional, ...)
    single_test$data.name <- data.name
    list(single_test = single_test, functional = functional)
  }

  # do this function above for all items
  tests <- lapply(colNrs_list, colNrs_sctest, ...)
  names(tests) <- names(colNrs_list)
  out <- list(tests = tests,
            info = list(
              test_info = list(stat_name = stat_name,
                               DIF_covariate = DIF_covariate, 
                               DIF_covariate_name = DIF_covariate_name),
              item_info = list(
                colNrs = colNrs_list,
                item_type = itemtype[allItemNames %in% names(tests)])),
            gefp = gefp)
  class(out) <- "scDIFtest"
  return(out)
}




## Function to extract the column numbers of the score matrix (returned by
##  estfun) that correspond with the estimated paramters specific items.
## Returns a named list of number vectors.
## This function was written with help and contributions from Lennart Schneider.
get_colNrs <- function(object, item_selection = NULL){

  ## extract information from mirt object
  allItemNames <- mirt::extract.mirt(object, "itemnames")
  nest <- mirt::extract.mirt(object, "nest")
  constraints <- mirt::extract.mirt(object, "constrain")

  ## select item names
  if(is.null(item_selection)) item_selection <- allItemNames
  else if(is.numeric(item_selection)) item_selection <- allItemNames[item_selection]
  else stopifnot(item_selection %in% allItemNames)

  ## get the model structure
  all_pars <- mirt::mod2values(object)

  ## keep only estimated parameters
  all_pars <- all_pars[all_pars$est == TRUE, ]

  ## replace the repeated parameter with the reference parameters
  ## (via the constraints) and keep track of which parameters are repeated.
  all_pars$repeated <- FALSE
  for(constraint in constraints){
    reference <- constraint[1]
    whichRepeated <- all_pars$parnum %in% constraint[-1]

    all_pars$parnum[whichRepeated] <- reference
    all_pars$repeated[whichRepeated] <- TRUE
  }

  ## remove the repeated parameters
  est_pars <- all_pars[!all_pars$repeated, ]

  ## sanity check: number of estimated parameters
  if(dim(est_pars)[1] != nest) stop("Shit! Something is wrong!")

  ## give new parameter numbers
  est_pars$newParnum <- seq.int(nest)

  ## add new parameter numbers
  all_pars <- merge(all_pars, est_pars[,c("parnum", "newParnum")],
                    all.x = TRUE, sort = FALSE)

  ## for every item name, list the newParnum
  colNrs <- lapply(item_selection, function(itemName)
    unique(with(all_pars, newParnum[item == itemName])))

  names(colNrs) <- item_selection
  return(colNrs)
}


