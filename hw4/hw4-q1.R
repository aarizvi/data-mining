###############################
### Homework 4 - Question 1 ###
######### Abbas Rizvi #########
###############################
rm(list=ls())
setwd("/Users/aarizvi/Google Drive/STA546/hw4/")
library(gRain)
library(gRbase)

## Consider the "cad1" dataset in the package gRbase. 
## There are 236 observations on fourteen variables from the Danish Heart Clinic
## A structural learning algorithm has identified the "optimal network" as given below.
## For simplicity, not all variables in the networks are represented

## Load CAD Data
data(cad1)

# ##### PART A ######
# ## Construct this network in R and infer the Conditional Probability Tables using the cad1 data.
# ny <- c("no", "yes")
# table(cad$Sex)  # are they males?      # no - 47 ; yes - 189
# table(cad$Smoker)                      # no - 51 ; yes - 185
# table(cad$Inherit)                     # no - 162; yes - 74
# table(cad$Hyperchol)                   # no - 108; yes - 128
# table(cad$SuffHeartF)                  # no - 167; yes - 69
# table(cad$CAD)                         # no - 129; yes - 107
# 
# ## Construct CPT tables
# sex <- cptable(~sex, values=c(47,189), levels=ny)
# smoker.sex <- cptable(~smoker|sex, values=c(51,185,47,189), levels=ny)
# shf <- cptable(~suffheartf, values=c(167,69), levels=ny)
# inherit.smoker <- cptable(~inherit|smoker, values=c(162,74,51,185), levels=ny)
# hc.shf.smoke <- ortable(~hyperchol+suffheartf+smoker, levels=ny)
# cad.hc.inherit <- ortable (~cad+hyperchol+inherit, levels=ny)
# 
# ## Compile CPT tables
# plist <- compileCPT(list(sex, smoker.sex, shf, inherit.smoker, hc.shf.smoke, cad.hc.inherit))
# net1 <- grain(plist) #'grain' builds a graphical independence network
# dev.off()
# plot(net1)


###### Part A ######
cad.dag <- dag(~CAD:Inherit:Hyperchol + Hyperchol:SuffHeartF + Inherit:Smoker + Hyperchol:Smoker + Smoker:Sex)
cad.cpt <- extractCPT(cad1, cad.dag, smooth = 0.1) #smooth = 0.1 to avoid zeros in the CPTs
plist <- compileCPT(cad.cpt)
cad.net <- grain(plist)
plot(cad.net)

## Identify any d-separations in the graph
## Specify the DAG
cadg <- list(~Sex, ~Smoker|Sex, ~Inherit|Smoker, ~SuffHeartF, ~Hyperchol|Suffheartf:Smoker, ~CAD|Hyperchol:Inherit)
## To use dSep it needs to be the cadg needs to be as an adjacency matrix
caddag <- dagList(cadg, "matrix")


dSep(caddag, "Inherit", "Sex", "Smoker") #inherit and sex are d-separated given evidence of smoker
dSep(caddag, "Hyperchol", "Sex", "Smoker") #Hyperchol and Sex are d-separated given evidence of smoker
dSep(caddag, "Smoker", "CAD", c("Inherit", "Hyperchol")) #Smoker and CAD are d-separated given evidence of inherit/hyperchol
dSep(caddag, "SuffHeartF", "CAD", "Hyperchol") #SuffHeartF and CAD are d-separated given evidence of Hyperchol

dSep(caddag, "CAD", "Hyperchol", "SuffHeartF")
dSep(caddag, "CAD", "Inherit", c("SuffHeartF", "Smoker"))
dSep(caddag, "CAD", "Inherit", c("SuffHeartF", "Smoker"))

dSep(caddag, "Inherit", "Hyperchol", "Smoker") #inherit and hyperchol are d-separated given information about smoker


##### PART B #####
## Suppose it is known that a new observation is female with hypercholesterolemia (high cholesterol)
## Absorb this evidence into the graph and revise the probabilities
## How does the probability of heart-failure and CAD change after this info is taken into account?

cad.net.evidence <- setEvidence(cad.net, evidence=list(Sex="Female", Hyperchol="Yes"))
querygrain(cad.net)$CAD 
## CAD probability is 54% no and 46% yes
querygrain(cad.net)$SuffHeartF 
## Heart failure probability is 70.7% and 29.3%
querygrain(cad.net.evidence)$CAD 
## CAD probability is 39.3% no and 60.7% yes with new evidence
querygrain(cad.net.evidence)$SuffHeartF 
## Heart-failure probability is 61.7% no 38.3% yes with new evidence

cad.net.evidence <- setEvidence(cad.net, evidence=list(Sex="Female", Hyperchol="Yes"))
##### PART C #####
## Simulate a new data set with new observations conditional upon this new evidence (part B).
## Save this new data as *.txt and submit it with your assignment
## Use the new data to determine the joint distribution of "smoker" and "cad" given this evidence
## (hint: use "simulate in the gRain packages (for help: ?simulate.grain))
#Simulation
#cad.net <- grain(cad.dag, data = cad1, smooth = 0.1)
sim.cad <- simulate.grain(cad.net.evidence, nsim = 5000)
head(sim.cad)
#joint distribution of "Smoker" and "CAD" given the new evidence
xtabs(~CAD + Smoker, data=sim.cad) / nrow(sim.cad) 
querygrain(cad.net.evidence, nodes=c("Smoker", "CAD"), type="joint", result = "data.frame")
#similar answers from simulation

querygrain(cad.net, nodes=c("Smoker", "CAD"), type="joint", result = "data.frame")

