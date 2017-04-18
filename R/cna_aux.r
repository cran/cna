
# fs2cs: 
# ======
# transforms a fs-tt into a sc-tt
# Note: fs2cs is designed to work with a data.frame, not a rtuthTab!!
fs2cs <- function(tt, cutoff = 0.5, border = c("down", "up", "drop")){
  border <- match.arg(border)
  tt.r <- tt
  tt.r[] <- round(tt.r + (0.5 - cutoff))
  if (border == "up") 
    tt.r[tt%%1 == cutoff] <- tt.r[tt%%1 == cutoff] + 1
  if (border == "drop") 
    tt.r <- tt.r[!apply(tt%%1 == cutoff, 1, any), , drop = FALSE]
  tt.r
}

# tt.info: 
# ========
# Auxiliary function that extracts useful informations from a truthTab
tt.info <- function(tt, cutoff = 0.5, border = c("down", "up", "drop")){
  stopifnot(inherits(tt, "truthTab"))
  type <- attr(tt, "type")
  border <- match.arg(border)
  
  if (type == "cs") {
    uniqueValues <- lapply(tt, function(x) 1:0)
    resp_nms <- names(tt)
    factMat <- function(x) 
      structure(cbind(tt[[x]], 1 - tt[[x]]), 
                .Dimnames = list(NULL, c(x, tolower(x))))
    valueId <- 2 - as.matrix(tt)
  } else if (type == "mv") {
    uniqueValues <- lapply(tt, function(x) sort(unique.default(x)))
    resp_nms <- mapply(paste, names(tt), uniqueValues, MoreArgs = list(sep = "="), 
                       SIMPLIFY = FALSE)
    resp_nms <- unlist(resp_nms, use.names = FALSE)
    factMat <- function(x) {
      v <- eval(parse(text = sub("\\=", "==", x), keep.source = FALSE), tt)
      matrix(as.integer(v), dimnames = list(NULL, x))
    }
    valueId <- mapply(match, tt, uniqueValues)
  } else if (type == "fs") {
    tt.r <- fs2cs(as.data.frame(tt), cutoff = cutoff, border = border)
    uniqueValues <- lapply(tt.r, function(x) 1:0)
    resp_nms <- names(tt)
    factMat <- function(x) 
      structure(cbind(tt[[x]], 1 - tt[[x]]), 
                .Dimnames = list(NULL, c(x, tolower(x))))
    valueId <- 2 - as.matrix(tt.r)
  }
  stopifnot(resp_nms == toupper(resp_nms))
  nVal <- vapply(uniqueValues, length, integer(1))
  valueId <- sweep(valueId, 2, c(0, cumsum(nVal[-length(nVal)])), "+")
  stopifnot(valueId%%1 == 0)
  mode(valueId) <- "integer"
  config <- split(seq_len(sum(nVal)), 
                  rep(seq_along(nVal), nVal))
  names(config) <- names(tt)
  scores <- do.call(cbind, lapply(resp_nms, factMat))
  mget(c("resp_nms", "nVal", "uniqueValues", "config", "valueId", "scores", "factMat"))
}


# function check.ordering
# =======================
check.ordering <- function(ordering, tt){
  if (is.null(ordering)) return(NULL)
  if (!is.list(ordering))
    stop("ordering must be NULL or a list.")
  # set all names to uppercase
  ordering <- rapply(ordering, toupper, how = "replace")
  ord.vars <- unlist(ordering, use.names = FALSE)
  if(!all(ord.vars %in% colnames(tt))){
    stop("Factor listed in ordering does not exist: ",
         setdiff(ord.vars, colnames(tt)))
  }
  if (anyDuplicated(unlist(ordering)))
    stop("At least one factors appears twice in the ordering.")
  if (!all(colnames(tt) %in% ord.vars)){
    ordering <- c(list(setdiff(colnames(tt), ord.vars)), ordering)
  }
  ordering
}


# potential.effects:
# ==================
potential.effects <- function (x, zname, ordering = NULL, strict = FALSE){
  poteff0 <- setdiff(names(x), zname)
  if (is.null(ordering)){
    out <- poteff0
  } else {
    rownames(x) <- NULL
    ordLevelZ <- which(vapply(ordering, function(x) any(zname %in% x), logical(1)))
    poteff <- unlist(ordering[seq_len(ordLevelZ - strict)], use.names = FALSE)
    out <- poteff[match(poteff0, poteff, 0)]  # same order as columns in x
  }
  if (attr(x, "type") == "mv"){ # Case mv with expanded truthTab... [nonstandard!!]
    vname <- sub("\\=[0-9]+$", "", zname)
    out <- setdiff(out, vname)
  }
  out
}



# intList, intList2:
# ------------------
# An "intList" is a representation of a formula as a list of integers,
# where the integers refer to colnames in scores
# Example: If the colnames in scores are c("A", "a", "B", "b", "C", "c") then
#   "A+b*C" ist then represented as list(1L, c(4L, 5L))
# An "recIntList" is a list of "intList" objects.
is.intList <- function(x){
  is.list(x) && all(vapply(x, is.integer, logical(1)))
}
is.recIntList <- function(x) is.list(x) && all(vapply(x, is.intList, integer(1)))

intList2char <- function(il, nms){
  conjs <- rapply(il, function(i) paste(nms[i], collapse = "*"),
                  how = "replace")
  vapply(conjs, function(l) do.call(paste, c(l, sep = " + ")), character(1))
}

# Meaningful disjunctions of literals (excluding terms like "A+a")
simpleDisj <- function(config, k){
  stopifnot(is.list(config), 
            vapply(config, is.integer, logical(1)), 
            k <= length(config))
  expand_part <- function(x) do.call(expand.grid, config[x])
  ll <- combn(seq_along(config), k, FUN = expand_part, simplify = FALSE)
  ll <- lapply(ll, "colnames<-", make.names(seq_len(k)))
  mm <- do.call(rbind, ll)
  unname(split.default(as.list(unlist(mm, use.names = FALSE)), row(mm)))
}


# All possible structures of asf 
# maxstep integer of length 3:
#   element 1: maximum number of terms in conjunctions
#   element 2: maximum number of conjunctions in disjunction
#   element 3: maximum total number of terms
# examples: 
# struct(c(2, 3, 5))
# struct(c(3, 2, 6))
allStructs <- function(maxstep){
  maxCompl <- maxstep[[3]]
  if (maxCompl == 1) return(list(1L))
  maxnum <- maxstep[[1]]
  maxlen <- maxstep[[2]]
  one.less <- allStructs(c(maxnum, maxlen, maxCompl - 1L))
  out <- c(list(list(rep(1L, maxCompl))),
           lapply(one.less, add12each))
  out <- unlist(out, recursive = FALSE)
  out <- out[sapply(out, max) <= maxnum]
  out <- out[vapply(out, length, integer(1)) <= maxlen]
  out <- lapply(out, sort)
  unique.default(c(one.less, out))
}
# auxiliary function
add12each <- function(x)
  mapply(function(l, i){
    l[i] <- l[i] + 1L
    l
  },
  rep(list(x), length(x)),
  seq_along(x),
  SIMPLIFY = FALSE)



