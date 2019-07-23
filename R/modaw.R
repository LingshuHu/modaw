
## get all residual combinations and exclude those without common items.
form_resi_cor <- function(comb) {
  split1 <- unlist(strsplit(comb[1], split = ".", fixed = TRUE))
  split2 <- unlist(strsplit(comb[2], split = ".", fixed = TRUE))
  if(any(split1 %in% split2)) {
    combs <- paste0(comb[1], " ~~ ", comb[2])
    return(combs)
  } else {
    return(NULL)
  }
}


#' Write scripts of latent variables and/or residual correlations of items
#'
#' Automatically generate the scripts of latent variables and/or residual correlations of items used for lavaan model
#'
#' @param variable_name Charater. Name of the variable.
#' @param item_names Charater or charater vector. Names of the items.
#' @param resi_cor Logical. Whether include residual correlations.
#' @param item Integer. The column number of items in a data frame
#' @param data The input data frame
#'
#' @return Characters.
#'
#' @examples
#' ## item names
#' item_names <- c("em1.an1", "em1.an2", "em1.an3", "em2.an1", "em2.an2", "em2.an3",
#'                 "em3.an1", "em3.an2", "em3.an3")
#'
#' ## get the scripts used for lavaan model by entering the item names
#' auto_var(variable_name = "E.A", item_names = item_names)
#'
#' ## creat a data frame with 9 items
#' df <- data.frame(
#'   em1.an1 = c(3, 4, 5, 1),
#'   em1.an2 = c(1, 2, 5, 3),
#'   em1.an3 = c(1, 4, 5, 3),
#'   em2.an1 = c(1, 2, 5, 5),
#'   em2.an2 = c(5, 2, 5, 5),
#'   em2.an3 = c(5, 1, 4, 5),
#'   em3.an1 = c(4, 1, 4, 1),
#'   em3.an2 = c(2, 1, 1, 1),
#'   em3.an3 = c(2, 3, 2, 1),
#'   stringsAsFactors = FALSE
#' )
#'
#' ## get the scripts used for lavaan model by entering the column number of the data frame
#' auto_var(variable_name = "E.A", item = 1:9, data = df)
#'
#' @export
auto_var <- function(variable_name, item_names = NULL, resi_cor = TRUE, item = NULL, data = NULL) {
  if(!is.null(item_names)) {
    variable <- paste0(item_names, collapse = " + ")
    variable <- paste0(variable_name, " =~ ", variable)
    if(resi_cor) {
      comb <- utils::combn(item_names, m = 2, simplify = FALSE)
      comb <- lapply(comb, form_resi_cor) ## use form_resi_cor to exclude non-common items
      comb <- Filter(Negate(is.null), comb) ## filter null results
      cat(variable, sep = "\n")
      cat("# correlation residuals", sep = "\n")
      cat(unlist(comb), sep = "\n")
    } else {
      cat(variable, sep = "\n")
    }
  } else {
    item_names <- colnames(data[item])
    variable <- paste0(item_names, collapse = " + ")
    variable <- paste0(variable_name, " =~ ", variable)
    if(resi_cor) {
      comb <- utils::combn(item_names, m = 2, simplify = FALSE)
      comb <- lapply(comb, form_resi_cor) ## use form_resi_cor to exclude non-common items
      comb <- Filter(Negate(is.null), comb) ## filter null results
      cat(variable, sep = "\n")
      cat("# correlation residuals", sep = "\n")
      cat(unlist(comb), sep = "\n")
    } else {
      cat(variable, sep = "\n")
    }
  }
}


