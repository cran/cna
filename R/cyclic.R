
# function cyclic():
# Determine if a csf is cyclic
#   x must be a character vector
# Returns a logical vector
cyclic <- function(x, cycle.type = c("factor", "value"), use.names = TRUE, verbose = FALSE){
  stopifnot(is.character(x))
  x <- noblanks(x)

  # Check input conditions [identical code in is.submodel()]
  px <- lapply(x, tryparse)
  ok <- !vapply(px, is.null, logical(1)) 
  tt_type <- if (any(grepl("=", x, fixed = T))) "mv" else "cs"
  if (tt_type == "cs"){
    vals <- unique.default(unlist(lapply(px, all.vars)))
  } else {
    vals <- rapply(px, .call2list, how = "unlist",
                   stopOps = c("==", "<", ">", "<=", ">="), 
                   validOps = c("<-", "<<-", "=", "&", "|", "(", "-"))
    vals <- unique.default(vals[!vapply(vals, is.symbol, FUN.VALUE = TRUE)])
    vals <- sub(" == ", "=", vapply(vals, deparse, character(1)))
  }
  cond_type <- .qcondType(x, values = vals, tt_type = tt_type, stdComplex.multiple.only = FALSE)
  ok <- ok & cond_type %in% c("stdAtomic", "stdComplex")
  if (any(!ok)) 
    stop("Invalid input to cyclic:\n", paste0("  ", x[!ok], collapse = "\n"),
         call. = FALSE)

  # check cyclicity
  out <- vapply(x, .cyclic1, cycle.type = cycle.type, verbose = verbose, FUN.VALUE = logical(1),
                USE.NAMES = FALSE)
  if (use.names) names(out) <- x
  out
}
# Process one csf
.cyclic1 <- function(x, cycle.type = c("factor", "value"), verbose = FALSE){
  cycle.type <- match.arg(cycle.type)
  asf <- extract_asf(x)[[1]]
  r <- rhs(asf)
  l <- strsplit(lhs(asf), "\\+|\\*")
  
  if (cycle.type == "factor"){
    hasEq <- grepl("=", c(r, l), fixed = TRUE)
    if (length(hasEq)==0 | any(hasEq != hasEq[1]))
      stop("Incorrect input condition: ", x, call. = FALSE)
    if (hasEq[1]){  # mv case
      r <- sub("=.+", "", r)
      l[] <- lapply(l, FUN = sub, pattern = "=.+", replacement = "")
    } else {
      r <- toupper(r)
      l[] <- lapply(l, toupper)
    }
    l <- lapply(l, unique.default)
  }
  ul <- unlist(l, use.names = FALSE)
  
  # "causal successors" of all factors
  successors <- split.default(rep(r, lengths(l)), ul)
  successors <- append(successors, rep_names(list(""), setdiff(r, ul)))
  successors[] <- lapply(successors, unique.default)
  # Auxiliary function step()
  .step <- function(s){
    current <- vapply(s, "[", 1, FUN.VALUE = character(1))
    succ <- successors[current]
    out <- mapply(c, unlist(succ, use.names = FALSE), rep(s, lengths(succ)), 
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    out <- unique(out)
    out
  }
    
  # starting points
  to_check <- intersect(r, ul)  
  found_cycle <- FALSE
  
  if (verbose) cat("---", x, "---\n")

  # Expand "causal paths"
  while (length(to_check) && !found_cycle){

    states <- to_check[[1]]
    visited <- states  # Filter(nzchar, unique(unlist(s2)))
    
    repeat {
      states_new <- .step(states)
      to_follow <- nzchar(current <- vapply(states_new, "[", 1, FUN.VALUE = character(1)))
      
      if (!any(to_follow)){  # -> ok so far, continue with to_check
        break
      }
      if (any(unlist(lapply(states_new, duplicated), use.names = FALSE))){ # -> cyclic!!
        found_cycle <- TRUE
      }
      visited_new <- Filter(nzchar, union(visited, unlist(states_new, use.names = FALSE)))

      states <- states_new
      visited <- visited_new
      
      if (found_cycle)
        break
    }
    
    if (!found_cycle) to_check <- setdiff(to_check, visited)

    if (verbose){
      writeLines(vapply(states, function(x) C_concat(rev(Filter(nzchar, x)), " > "),
                        FUN.VALUE = character(1)))
    }
  }
  if (verbose) cat("\n")
  found_cycle
}

# Aux fun
rep_names <- function(x, nms){
  out <- rep_len(x, length(nms))
  names(out) <- nms
  out
}
