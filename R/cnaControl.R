
cnaControl <- function(
   inus.only = TRUE, inus.def = c("implication", "equivalence"), 
   type = "auto", con.msc = NULL, 
   rm.const.factors = FALSE, rm.dup.factors = FALSE, 
   cutoff = 0.5, border = "up", asf.selection = c("cs", "fs", "none"),
   only.minimal.msc = TRUE, only.minimal.asf = TRUE, maxSol = 1e6){
  # Select inus.def (choices: "implication", "equivalence")
  inus.def <- match.arg(inus.def)
  # Select asf.selection (choices: "none", "cs", "fs")
  asf.selection <- match.arg(asf.selection)
  # Select border (choices: "up", "down" -- warn if "drop")
  border <- resolveBorder(border)
  # output
  out <- 
    list(inus.only = inus.only, inus.def = inus.def, type = type, 
         con.msc = con.msc, rm.const.factors = rm.const.factors, 
         rm.dup.factors = rm.dup.factors, cutoff = cutoff, border = border,
         asf.selection = asf.selection, only.minimal.msc = only.minimal.msc, 
         only.minimal.asf = only.minimal.asf, maxSol = maxSol)
  class(out) <- "cnaControl"
  out
}


resolveCnaControl <- function(control, .con, ..., 
                              control_input_null){
  if (control_input_null) control <- cnaControl(...)
  if (!control_input_null && ...length()){
    stop("Fine-tune cna() using either argument 'control' or the dots '...'.", 
         "\nUsing both is potentially ambiguous and therefore not admissible.", 
         call. = FALSE)
  }
  if (!inherits(control, "cnaControl")){
    stop("'control' must be defined using cnaControl().", 
         call. = FALSE)
  }
  # resolve inus.only in case it contains an inus definition
  if (is.character(control$inus.only)){
    control$inus.def <- 
      match.arg(control$inus.only, c("implication", "equivalence"))
    warning("Setting inus.only=\"implication\" or inus.only=\"equivalence\" is deprecated;",
            "\nuse the parameter inus.def instead. See ?cnaControl for help.",
            call. = FALSE)
    control$inus.only <- TRUE
  } else {
    control$inus.only <- control$inus.only
    control$inus.def <- "implication"
  }
  # con.msc
  if (is.null(control$con.msc)) control$con.msc <- .con
  # return
  control
}

# ------------------------------------------------------------------------------

checkCnaParams <- function(env = parent.frame()){
  # logical scalars
  logParams <- c(
    mget(c("strict", "acyclic.only", "verbose"), env, inherits = FALSE),
    env$control[c("inus.only", "rm.const.factors", "rm.dup.factors", 
                  "only.minimal.msc", "only.minimal.asf")])
  # numeric scalars
  numParams <- c(mget(c("con", "cov"), env, inherits = FALSE),
                 env$control[c("con.msc", "cutoff")])
  # pos integer scalar
  intParams <- env$control[c("maxSol")]
  # char scalar
  charParams <- mget("what", env, inherits = FALSE)
  invalidMsgs <- c(.chkLogical(logParams), .chkNumeric01(numParams), 
                   .chkPosInt(intParams), .chkChar(charParams))
  l <- length(invalidMsgs)
  if (l > 0){
    # sort in the order formal args in cna() and cnaControl():
    ord <- match(names(invalidMsgs), 
                 c(names(formals(cna)), names(formals(cnaControl))))
    invalidMsgs <- invalidMsgs[order(ord)]
    # issue error message and abort
    stop("Invalid input ", if (l>1) "s", " to cna():\n  ", 
         paste(format(paste0(names(invalidMsgs), ":")), 
               invalidMsgs, collapse = "\n  "), 
         call. = FALSE)
  }
}
.chkLogical <- function(x){
  # check that elements are logical scalar
  ok <- vapply(x, function(e) e %in% 0:1 && length(e) == 1 && !is.na(e), 
               logical(1))
  if (!all(ok)){
    out <- rep("expecting a logical value (TRUE/FALSE or 0/1)", sum(!ok))
    names(out) <- names(x[!ok])
    out
  }
}
.chkNumeric01 <- function(x){
  # check that elements are nueric scalar >=0 and <=1
  ok <- vapply(x, function(e) is.numeric(e) && length(e) == 1 & is.finite(e) & 
                 e >= 0 & e <= 1, 
               logical(1))
  if (!all(ok)){
    out <- rep("expecting a numeric value between 0 and 1", sum(!ok))
    names(out) <- names(x[!ok])
    out
  }
}
.chkPosInt <- function(x){
  # check that elements are nueric scalar >=0 and <=1
  ok <- vapply(x, function(e) is.numeric(e) && length(e) == 1 && !is.na(e) &&
                 e >= 1 & e %% 1 == 0,
               logical(1))
  if (!all(ok)){
    out <- rep("expecting a positive integer value", sum(!ok))
    names(out) <- names(x[!ok])
    out
  }
}
.chkChar <- function(x){
  # check that elements are nueric scalar >=0 and <=1
  ok <- vapply(x, function(e) is.character(e) && length(e) == 1 && !is.na(e),
               logical(1))
  if (!all(ok)){
    out <- rep("expecting a character string", sum(!ok))
    names(out) <- names(x[!ok])
    out
  }
}


