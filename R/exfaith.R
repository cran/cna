
exfaith <- function(cond, x, ...){
  .exff(x, noblanks(cond), ...)
}

# ==== .exff() ====
# calcuate exhaustiveness/Faithfulness of a cond

# Generic function
# Switch order of first 2 args to provide dispatching on x
.exff <- function(x, cond, ...) UseMethod(".exff")


# ==== Default Method (for matrix or data.frame) ====
# builds 
#   x       configTable
# value:    configTable, mv if original is mv, cs else
.exff.default  <- function(x, cond, ...){
  if (is.matrix(x) || is.data.frame(x)){
    x <- configTable(x, rm.dup.factors = FALSE, rm.const.factors = FALSE, verbose = FALSE)
    .exff.configTable(x, cond, ...)
  } else {
    stop("Invalid specification of arguments")
  }
}


# ==== Method for class 'configTable' ====
# Function suited for interactive use
.exff.configTable <- function(x, cond, ...){
  cti <- ctInfo(x)
  qtypes <- .qcondType(cond, colnames(cti$scores), cti$type,
                        stdComplex.multiple.only = FALSE) 
  ok <- qtypes %in% c("stdBoolean", "stdAtomic", "stdComplex", "constant")
  if (any(!ok)){
    stop("Invalid condition(s):\n", 
         paste0("  ", cond[!ok], collapse = "\n"),
         "\nexfaith() expects valid conditions in standard form.",
         call. = FALSE)
  }
  # Always ctiList!
  cti <- ctiList(unique_cti_cs(cti), cond)
  .exff(cti, cond, ...)
}

# Aux fun unique_cti_cs(): 
# takes cti, removes duplicates and transforms fs to cs
# (used in calculation of exhauxtiveness & faithfulness)
unique_cti_cs <- function(cti){
  cti <- subset(cti, !duplicated(cti$valueId))
  cti$freq[] <- 1L
  if (cti$type == "fs"){
    cti$type <- "cs"
    rnd <- round(cti$scores[, cti$resp_nms])
    cti$scores[] <- as.vector(rbind(rnd, 1-rnd))
  }
  cti
}
if (FALSE){
  # Checks:
  # cs case:
  cti <- ctInfo(csct(d.educate))
  identical(
    cna:::cti2ct(cti) %>% ct2df %>% unique %>% csct(rm.const.fact = F, rm.dup.fact = F) %>% ctInfo,
    unique_cti_cs(cti)
  )
  # fs case:
  cti <- ctInfo(fsct(d.pacts))
  identical(
    cna:::cti2ct(cti) %>% fs2cs %>% ct2df %>% unique %>% csct(rm.const.fact = F, rm.dup.fact = F) %>% 
       ctInfo,
    unique_cti_cs(cti)
  )
  # mv case:
  cti <- ctInfo(mvct(d.pban))
  identical(
    cna:::cti2ct(cti) %>% ct2df %>% unique %>% 
      mvct(rm.const.fact = F, rm.dup.fact = F) %>% ctInfo,
    unique_cti_cs(cti)
  )
}


# ==== Method for class 'cti' ====
# There is NO method for cti, calculation of exhaustiveness and faithfulness is 
# always performed at 'ctiList' level.


# Calculation of exhaustiveness and faithfulness:
#   Calculation is based on the assumption that 
#   every tbls-comp of ctiList contains only the factors
#   that are present in the respective cond's
calcExff <- function(cti, cti.full, cond, 
                     num = TRUE, names = TRUE){
  # Number of factor configurations in actual data:
  n_data <- sum(!duplicated(cti$valueId))
  # Number of factor configurations in actual data matching cond:
  n_data_cond <- colSums(qcond_csf(cond, unique.matrix(cti$scores), force.bool = TRUE))
  # Number of factor configurations in full data matching cond:
  n_cond <- colSums(qcond_csf(cond, unique.matrix(cti.full$scores), force.bool = TRUE))
  # Output
  out <- cbind(
    exhaustiveness = n_data_cond / n_cond,
    faithfulness = n_data_cond / n_data)
  if (names) rownames(out)  <- cond
  if (!num){
    out[] <- abs(1-out) < 1e-12
    mode(out) <- "logical"
  }
  out
}

