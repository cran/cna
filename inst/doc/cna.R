### R code from vignette source 'cna.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: data examples
###################################################
library(cna)
cna(d.educate)
cna(d.women)


###################################################
### code chunk number 3: configTable1
###################################################
configTable(d.women)


###################################################
### code chunk number 4: configTable2 (eval = FALSE)
###################################################
## pact.ct <- configTable(d.pacts, case.cutoff = 2)
## ct2df(pact.ct)


###################################################
### code chunk number 5: simul1 (eval = FALSE)
###################################################
## allCombs(c(2, 2, 2)) - 1 
## allCombs(c(3, 4, 5))
## full.ct("A + B*c")
## full.ct(6)
## full.ct(list(A = 1:2, B = 0:1, C = 1:4)) 


###################################################
### code chunk number 6: simul1 (eval = FALSE)
###################################################
## dat1 <- allCombs(c(2, 2, 2)) - 1 
## selectCases("A + B <-> C", dat1)
## selectCases("(h*F + B*C*k + T*r <-> G)*(A*b + H*I*K <-> E)")
## target <- randomCsf(full.ct(6))
## selectCases(target)


###################################################
### code chunk number 7: simul3 (eval = FALSE)
###################################################
## makeFuzzy(selectCases("Hunger + Heat <-> Run"), 
##    fuzzvalues = seq(0, 0.4, 0.05))


###################################################
### code chunk number 8: simul4 (eval = FALSE)
###################################################
## dat3 <- allCombs(c(3, 4, 5))
## dat4 <- selectCases("A=1*B=3 + A=3 <-> C=2", configTable(dat3))
## some(dat4, n = 10, replace = FALSE)
## some(dat4, n = 1000, replace = TRUE)


###################################################
### code chunk number 9: showMeasures
###################################################
showConCovMeasures()


###################################################
### code chunk number 10: showDetails (eval = FALSE)
###################################################
## cna(d.women, measures = c("PAcon", "PACcov"), details = c("scon", "scov",
##    "ccon", "ccov", "AAcov", "AACcon"))


###################################################
### code chunk number 11: frscore (eval = FALSE)
###################################################
## library(frscore)
## frscored_cna(d.pban, fit.range = c(1, 0.8), granularity = 0.1)


###################################################
### code chunk number 12: cons1 (eval = FALSE)
###################################################
## cna(d.jobsecurity, outcome = "JSR", con = 1, cov = 1)
## cna(d.jobsecurity, outcome = "JSR", con = .9, cov = .9)


###################################################
### code chunk number 13: cons2 (eval = FALSE)
###################################################
## cna(d.jobsecurity, outcome = "JSR", con = .75, cov = .75)
## cna(d.jobsecurity, outcome = "JSR", con = .75, cov = .75,
##    measures = c("PAcon", "PACcov"))


###################################################
### code chunk number 14: outcome1 (eval = FALSE)
###################################################
## cna(d.volatile, outcome = "VO2")


###################################################
### code chunk number 15: ordering1
###################################################
dat.aut.1 <- d.autonomy[15:30, c("AU","EM","SP","CO")]
ana.aut.1 <- cna(dat.aut.1, ordering = "EM, SP, CO < AU", strict = TRUE,
   con = .9, cov = .9)
printCols <- c("condition", "con", "cov")
csf(ana.aut.1)[printCols]


###################################################
### code chunk number 16: ordering2
###################################################
ana.aut.2 <- cna(dat.aut.1, ordering = "EM, SP, CO < AU", strict = FALSE, 
   con = .9, cov = .9)
csf(ana.aut.2)[printCols]


###################################################
### code chunk number 17: outcome2 (eval = FALSE)
###################################################
## cna(d.pban, ordering = "T, PB", con = .75, cov = .75)
## cna(d.pban, outcome = c("T=2", "PB=1"), ordering = "T, PB",
##    con = .75, cov = .75)


###################################################
### code chunk number 18: exclude1 (eval = FALSE)
###################################################
## cna(d.pban, outcome = c("T=2", "PB=1"), ordering = "T, PB", 
##    con = .75, cov = .75, exclude = c("C=2 -> T=2", "T=1,V=0 -> PB=1"))


###################################################
### code chunk number 19: exclude2 (eval = FALSE)
###################################################
## cna(d.jobsecurity, con = .85, cov = .85, exclude = c("s,c -> JSR",
##    "jsr, L -> R"))


###################################################
### code chunk number 20: maxstep0 (eval = FALSE)
###################################################
## cna(d.highdim, outcome = c("V13", "V11"), con = .8, cov = .8)
## cna(d.highdim, outcome = c("V13", "V11"), con = .8, cov = .8,
##    maxstep = c(2,3,10))


###################################################
### code chunk number 21: maxstep1 (eval = FALSE)
###################################################
## cna(d.volatile, ordering = "VO2", maxstep = c(3,4,10))
## vol1 <- cna(d.volatile, ordering = "VO2", maxstep = c(4,4,10))
## csf(vol1, n.init = 3000)


###################################################
### code chunk number 22: maxstep2 (eval = FALSE)
###################################################
## cna(d.volatile, ordering = "VO2", maxstep = c(8,10,40), suff.only = TRUE)


###################################################
### code chunk number 23: maxstep3
###################################################
ana.jsc.1 <- cna(d.jobsecurity, ordering = "JSR", con = .9, cov = .85)
csf(ana.jsc.1)[printCols]
ana.jsc.2 <- cna(d.jobsecurity, ordering = "JSR", con = .9, cov = .85,
  maxstep = c(3,5,12))
csf(ana.jsc.2)[printCols]


###################################################
### code chunk number 24: notcols
###################################################
ana.aut.3 <- cna(dat.aut.1, outcome = c("au", "em"), con = .88, cov = .82) 
csf(ana.aut.3)[printCols] 
ana.aut.4 <- cna(dat.aut.1, ordering = "AU", con = .88, cov = .82,
   notcols = c("AU", "EM")) 
csf(ana.aut.4)[printCols] 


###################################################
### code chunk number 25: tab2c (eval = FALSE)
###################################################
## dat5 <- allCombs(c(2, 2, 2, 2, 2)) -1
## dat6 <- selectCases("(A + B <-> C)*(A*B + D <-> E)", dat5)
## set.seed(3) 
## tab3c <- makeFuzzy(dat6, fuzzvalues = seq(0, 0.4, 0.01))
## cna(tab3c, con = .8, cov = .8)


###################################################
### code chunk number 26: details (eval = FALSE)
###################################################
## cna(d.educate, details = c("e", "f", "co", "cy", "PAcon", "PACcov"))


###################################################
### code chunk number 27: what (eval = FALSE)
###################################################
## cna(d.educate, what = "tm")
## cna(d.educate, what = "mac")
## cna(d.educate, what = "all")


###################################################
### code chunk number 28: vol1 (eval = FALSE)
###################################################
## vol2 <- cna(d.volatile, ordering = "VO2", con = .9, cov = .9)
## msc(vol2)
## asf(vol2)
## print(asf(vol2), Inf)


###################################################
### code chunk number 29: vol1 (eval = FALSE)
###################################################
## csf(vol2, n.init = 2000)
## csf(vol2, n.init = 100)


###################################################
### code chunk number 30: vol1 (eval = FALSE)
###################################################
## csf(vol2, verbose = TRUE)


###################################################
### code chunk number 31: d.edu1
###################################################
printCols <- c("condition", "con", "cov", "exhaustiveness")
csf(cna(d.educate, details = "exhaust"))[printCols]


###################################################
### code chunk number 32: d.edu2
###################################################
csf(cna(d.educate[-1,], details = "exhaust"))[printCols]


###################################################
### code chunk number 33: d.edu3
###################################################
printCols <- c("condition", "con", "cov", "faithfulness")
csf(cna(d.educate, details = "faithful"))[printCols]


###################################################
### code chunk number 34: d.edu4
###################################################
csf(cna(rbind(d.educate,c(1,1,0,1,0)), con = .8, details = "f"))[printCols]


###################################################
### code chunk number 35: rownames
###################################################
rownames(d.educate) <- 1:8


###################################################
### code chunk number 36: coherence
###################################################
d.edu.exp1 <- rbind(d.educate, c(1,0,1,0,0))
printCols <- c("condition", "con", "cov", "coherence")
csf(cna(d.edu.exp1, con = .8, details = "cohere"))[printCols]


###################################################
### code chunk number 37: cycle
###################################################
printCols <- c("condition", "con", "cov", "cyclic")
csf(cna(d.autonomy, ordering = "AU", con = .9, cov = .94,
   details = "cy", maxstep = c(2, 2, 8)))[printCols]


###################################################
### code chunk number 38: cycle1
###################################################
csf(cna(d.irrigate, con = .75, cov = .75, acyclic.only = F)) |> nrow()
csf(cna(d.irrigate, con = .75, cov = .75, acyclic.only = T)) |> nrow()


###################################################
### code chunk number 39: dat.redun (eval = FALSE)
###################################################
## library(causalHyperGraph)
## ana.d.women <- cna(d.women)
## plot(ana.d.women)


###################################################
### code chunk number 40: dat.redun (eval = FALSE)
###################################################
## causalHyperGraph("(A=1*B=2 + C=0*D=2 <-> E=1)*(E=1 + F=0 <-> G=1)")


###################################################
### code chunk number 41: ambigu1
###################################################
dat7 <- selectCases("a*B + A*b + B*C <-> D")
printCols <- c("condition", "con", "cov", "exhaustiveness","faithfulness")
csf(cna(dat7, details = c("ex", "fa")))[printCols]


###################################################
### code chunk number 42: ambigu2
###################################################
csf(cna(d.pban, cov = .95, maxstep = c(3, 5, 10)))["condition"]


###################################################
### code chunk number 43: ambigu3
###################################################
ana.pban <- cna(d.pban, cov = .95, maxstep = c(6, 6, 10),
   details = c("fa", "ex"))
csf.pban <- csf(ana.pban)
length(csf.pban$condition)


###################################################
### code chunk number 44: ambigu4
###################################################
csf.pban.ex <- subset(csf.pban, exhaustiveness >= .85)
length(csf.pban.ex$condition)


###################################################
### code chunk number 45: ambigu5
###################################################
subset(csf.pban.ex, complexity == min(csf.pban.ex$complexity))


###################################################
### code chunk number 46: back1
###################################################
dat.aut.2 <- d.autonomy[15:30, c("AU","EM","SP","CO","RE","DE")]
ana.aut.5 <- cna(dat.aut.2, outcome = c("EM","AU"), con = .91, cov = .91)
condition(csf(ana.aut.5)$condition, dat.aut.2)


###################################################
### code chunk number 47: back2
###################################################
group.by.outcome(condition(asf(ana.aut.5)$condition, dat.aut.2))$AU


###################################################
### code chunk number 48: details (eval = FALSE)
###################################################
## # Draw a ground truth with outcomes A=1 and B=2.
## fullData <- allCombs(c(4,4,4,4,4)) |> ct2df()
## groundTruth <- randomCsf(fullData, outcome = c("A=1", "B=2"), compl = 3)
## # Generate ideal data for groundTruth.
## idealData <- ct2df(selectCases(groundTruth, fullData))
## # Introduce 20% fragmentation.
## fragData <- idealData[-sample(1:nrow(idealData), nrow(idealData)*0.2), ] 
## # Add 10% random noise (cases incompatible with ground truth).
## incompCases <- dplyr::setdiff(fullData, idealData)
## x <- rbind(incompCases[sample(1:nrow(incompCases), 
##    nrow(fragData) * 0.1), ], fragData)  
## # Run CNA with outcome specification and PAcon/AAcov as evaluation 
## # measures.
## csfs <- csf(cna(x, outcome = c("A=1", "B=2"), con = .75, cov = .75, 
##    maxstep = c(3, 3, 12), measures = c("PAcon", "AAcov")))
## # Check whether no causal error (no false positive) is returned.
## if(length(csfs$condition)==0) {
##    TRUE } else {any(unlist(lapply(csfs$condition,
##    function(x) frscore::causal_submodel(x, groundTruth, fullData))))}


###################################################
### code chunk number 49: redundant4a
###################################################
options(width=80)


###################################################
### code chunk number 50: redundant4
###################################################
printCols <- c("condition", "con", "cov", "inus")
csf(cna(d.autonomy, ordering = "AU", con = .9, cov = .94,
   maxstep = c(2, 2, 8), inus.only = FALSE))[printCols]


###################################################
### code chunk number 51: redundant5
###################################################
condTbl("EM -> SP", configTable(d.autonomy))


###################################################
### code chunk number 52: redundant6
###################################################
csf(cna(d.autonomy, ordering = "AU", con = .9, cov = .94,
   maxstep = c(2, 2, 8), details = "inus"))[printCols]


