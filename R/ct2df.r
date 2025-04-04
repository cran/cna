
ct2df <- function(ct){
  if (is.null(attr(ct, "n", exact = TRUE))) return(as.data.frame(ct, warn = FALSE))
  n <- nrow(ct)
  df <- as.data.frame(ct, warn = FALSE)[rep(seq_len(n), attr(ct, "n")), , drop = FALSE]
  rownames(df) <- unlist(attr(ct, "cases"), use.names = FALSE, recursive = FALSE)
  attributes(df) <- attributes(df)[c("names", "row.names", "class")]
  df
}
