
tt2df <- function(tt){
  if (is.null(attr(tt, "n", exact = TRUE))) return(as.data.frame(tt))
  n <- nrow(tt)
  df <- as.data.frame(tt)[rep(seq_len(n), attr(tt, "n")), , drop = FALSE]
  rownames(df) <- unlist(attr(tt, "cases"), use.names = FALSE, recursive = FALSE)
  attributes(df) <- attributes(df)[c("names", "row.names", "class")]
  df
}
