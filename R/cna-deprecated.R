
# Function deprecated from 4.0.0
cscna <- function(...){
  .Deprecated("cna", package = "cna")
  cna(..., type = "cs")
}
mvcna <- function(...){
  .Deprecated("cna", package = "cna")
  cna(..., type = "mv")
}
fscna <- function(...){
  .Deprecated("cna", package = "cna")
  cna(..., type = "fs")
}

csct <- function(...){
  .Deprecated("configTable", package = "cna")
  configTable(..., type = "cs")
}
mvct <- function(...){
  .Deprecated("configTable", package = "cna")
  configTable(..., type = "mv")
}
fsct <- function(...){
  .Deprecated("configTable", package = "cna")
  configTable(..., type = "fs")
}

cscond <- function(...){
  .Deprecated("condition", package = "cna")
  condition(..., type = "cs")
}
mvcond <- function(...){
  .Deprecated("condition", package = "cna")
  condition(..., type = "mv")
}
fscond <- function(...){
  .Deprecated("condition", package = "cna")
  condition(..., type = "fs")
}

