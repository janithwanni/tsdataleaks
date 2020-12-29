#' Correlation calculation based on rolling window with overlapping observations.
#'
#' @param lstx list of time series
#' @param h length of forecast horizon
#' @param cutoff benchmark value for corr, default 1
#' @param boost Logical value indicating whether to boost performance by using RCpp. For small datasets setting boost=TRUE would not be efficient.
#' @importFrom  utils tail
#' @importFrom plyr ldply
#' @importFrom purrr map
#' @importFrom  stats na.omit
#' @useDynLib tsdataleaks
#' @importFrom Rcpp sourceCpp
#' @importFrom RcppParallel RcppParallelLibs
#' @return list of matching quantities
#' @export
find_dataleaks <- function(lstx, h, cutoff=1,boost=TRUE){
  n <- length(lstx)
  if (is.null(names(lstx)) == TRUE){names(lstx) <- 1:n} # This is important when displying the final results
  result <- list()

  #TODO Call RCpp function getLeakIndices and get a list of matrices
  for (i in 1:n){

    y = utils::tail(lstx[[i]], h)
    result[[i]] <- purrr::map(lstx, ts.match, y=y,boost=boost)

  }
  #get list of list of tibbles for each series
  # print("result")
  # print(result)
  result.list <- purrr::map(result, plyr::ldply, data.frame)
  #get list of tibbles with id added to identify series
  # print("result.list")
  # print(result.list)
  n.result.list <- length(result.list)
  resul.list.clean <- list()
  for(i in 1:n){

    resul.list.clean[[i]] <- result.list[[i]]
  }

  names(resul.list.clean) <- names(lstx)
  # print("resul.list.clean")
  # print(resul.list.clean)
  nonmissinglist <- purrr::map(resul.list.clean, stats::na.omit)
  #nonmissinglist
  # print("nonmissinglist")
  # print(nonmissinglist)

  namesx <- names(nonmissinglist)


  a <- list(length(nonmissinglist))
  for (i in 1: length(nonmissinglist)){
    a[[i]] <- which(nonmissinglist[[i]]$.id == namesx[i])
  }
  # print("a")
  # print(a)

  selfcalculationindex <- purrr::map(a, function(temp){temp[length(temp)]})
  # print("selfcalculationindex")
  # print(selfcalculationindex)
  for (i in 1: length(nonmissinglist)){
    nonmissinglist[[i]] <- nonmissinglist[[i]][-selfcalculationindex[[i]], ]
  }
  # print("nonmissinglist")
  # print(nonmissinglist)

  # Remove empty entries
  isEmpty <- function(y){nrow(y)==0}

  nonempty.list <-  purrr::map(nonmissinglist, isEmpty)
  # print("nonempty.list")
  # print(nonempty.list)

  nonmissinglist[unlist(nonempty.list)==FALSE]

}
#' @examples
#' a = rnorm(15)
#'lst <- list(
#'  a = a,
#'  b = c(a[10:15], rnorm(10), a[1:5], a[1:5]),
#'  c = c(rnorm(10), a[1:5])
#')
#'find_dataleaks(lst, h=5)
#'#' a = rnorm(15)
#'lst <- list(
#'  x= a,
#'  y= c(rnorm(10), a[1:5])
#')
#'
#'find_dataleaks(lst, h=5)
#'
#'# List without naming elements
#' lst <- list(
#'  a,
#'  c(rnorm(10), a[1:5], a[1:5]),
#'  rnorm(10)
#')
#'find_dataleaks(lst, h=5)

