
exhaustive <- function(cond, x, ...){
  out <- exfaith(cond, x, num = FALSE)[, "exhaustiveness", drop = FALSE]
  setNames(as.vector(out), rownames(out))
}
