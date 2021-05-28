
# Function .onLoad
.onLoad <- function(libname, pkgname) {
  options(spaces = c("<->", "->", "+"), 
          useCtiList = "auto", 
          ctiListLimit = 256)
  invisible()
}
