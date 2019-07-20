
auto_reco <- function(moderator_name, item_names) {
  comb <- utils::combn(item_names, 2, simplify = TRUE)
  comb <- apply(comb, 2, function(x) paste0(x[1], " ~~ ", x[2]))
  moderator <- paste0(item_names, collapse = " + ")
  moderator <- paste0(moderator_name, " =~ ", moderator)
  cat(moderator, sep = "\n")
  cat("# correlation residuals", sep = "\n")
  cat(comb, sep = "\n")
}
