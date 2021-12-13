

# Set working directory

#Load packages
library(RSiena)
library(dplyr)

# Load functions

siena07RunToConvergence <- function(alg, dat, eff, ans0, modelName, ...){
  numr <- 0
  ans <- siena07(alg, data = dat, effects = eff, prevAns = ans0, returnDeps = TRUE, ...)
  repeat{
    numr <- numr + 1
    tconv.max <- ans$tconv.max
    tratio.max <- max( abs( ans$tstat[eff$type[eff$include] != "rate"] ) )
    if (tconv.max > 100) {
      print(ans)
      cat("WARNING: Extreme divergence. Terminating run.\n")
      return("WARNING: Extreme divergence. Terminating run")
    }
    else if (tconv.max < 0.19 & tratio.max < 0.10) {
      print(ans)
      cat(paste0("Maximum Absolute Value Amongst Convergence t-Ratios: ", tratio.max, "\n"))
      cat(paste0("Model Has Converged After ", numr, " iterations. \n"))
      return(ans)
    }
    else {
      print(ans)
      cat("WARNING: Convergence Inadequate.\n")
      cat(paste0("Overall maximum convergence ratio: ", tconv.max, "\n"))
      cat(paste0("Iteration Number: ", numr), "\n")
      ans <- siena07(alg, data = dat, effects = eff, prevAns = ans, returnDeps = TRUE, ...)
    }
  }
}

#######################################
#         LOAD & SPECIFY DATA         #
#######################################

att <- read.csv("./1-data/covariates.csv", stringsAsFactors = FALSE, header = TRUE)
comp_list <- read.csv("./1-data/join_and_leave.csv", header = TRUE)

m1 <- as.matrix(read.table("./1-data/2005_matrix.csv", sep = ","))
m2 <- as.matrix(read.table("./1-data/2006_matrix.csv", sep = ","))
m3 <- as.matrix(read.table("./1-data/2007_matrix.csv", sep = ","))
m4 <- as.matrix(read.table("./1-data/2008_matrix.csv", sep = ","))

# Add placeholder numerical ID
att$m_id <- 1:nrow(att)
comp <- select(comp_list, comp = sessions)
# Remove punctuation & respecify comp list to be correct format
comp$comp <- gsub("[^[:alnum:].\\s]", " ", comp$comp)
# Write out new comp list for reference
write.table(comp, "./1-data/composition.txt", sep=";", col.names=FALSE, quote=FALSE, row.names=FALSE)

#Replace character ID with numeric
# Make list of matrices
M <- list(m1, m2, m3, m4)

for(i in seq_along(M)) {
  M[[i]][1,] <- att$m_id[match(M[[i]][1,], att$leg_name)]
  M[[i]][,1] <- att$m_id[match(M[[i]][,1], att$leg_name)]
  class(M[[i]]) <- "numeric"
  M[[i]][is.na(M[[i]])] <- 10
  round(M[[i]],1)
  }


# Change class to numeric
for (i in seq_along(M)) {
  class(M[[i]]) <- "numeric"
}

# Create the Siena data object
data <- sienaDataCreate(
                sponsorships =  sienaDependent(array(c(
								    M[[1]][-1, -1],
							      M[[2]][-1, -1],
                    M[[3]][-1, -1],
                    M[[4]][-1, -1]),
							    	  dim = c(133, 133, 4))),
                female = coCovar(att$female,centered = FALSE),
                democrat = coCovar(att$democrat, centered = FALSE),
                prior_years = coCovar(att$prior_years, centered = FALSE),
                senator = varCovar(as.matrix(select(att, senator1, senator2, senator3, senator4)), centered = FALSE),
                leader = varCovar(as.matrix(select(att, leader1, leader2, leader3, leader4)), centered = FALSE),
                composition = sienaCompositionChangeFromFile("./1-data/composition.txt")
							   )

# Create algorithm
alg1 <- sienaAlgorithmCreate(projname = './2-analyses/Utah-legislators',
                             n3 = 1000,
                             firstg = 0.15,
                             seed = 29)

#Increase n3 to 3000 iterations for final model
alg2 <- sienaAlgorithmCreate(projname = './2-analyses/Utah-legislators',
                             n3 = 10000,
                             firstg = 0.15,
                             seed = 29)

# Print initial report to see how the data look
print01Report(data, "./2-analyses/Utah-legislators" )

#######################################
#         SPECIFY BASE MODEL          #
#######################################

# Create effects object, this automatically includes outdegree & reciprocity
eff1 <- getEffects(data)
# Add individual-level effects
eff1 <- includeEffects(eff1, inPopSqrt, outActSqrt, outPopSqrt)
# Add higher-order effects (i.e., above dyadic).
# Note that transitive closure is impossible given the structure of the data.
# There don't seem to be any notable higher-order effects


#Run base model
model1 <- siena07RunToConvergence(alg1,
                        dat = data,
                        eff = eff1,
                        initC = TRUE,
                        useCluster = TRUE,
                        nbrNodes = 20,
                        batch = TRUE,
						            silent = FALSE,
                        ans0 = NULL,
                 		    modelName = "model.ans"
						            )

model1

# Check time heterogeneity.
tt_1 <- sienaTimeTest(model1)
summary(tt_1)

# Re-run with higher n3
model1 <- siena07(alg2, data = data,
                        effects = eff1,
                        initC = TRUE,
                        useCluster = TRUE,
                        nbrNodes = 20,
                        batch = TRUE,
                        prevAns = model1
                        )
model1

#######################################
#      SPECIFY EXTENDED MODEL         #
#######################################

# Add exogenous effects for whether senator, leader and prior years
eff2 <- includeEffects(eff1, egoX, interaction1 = "senator")
eff2 <- includeEffects(eff2, egoX, altX, sameX, interaction1 = "leader")
eff2 <- includeEffects(eff2, egoX, altX, simX, interaction1 = "prior_years")

# Run model until convergence
model2 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff2,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model1,
                  modelName = "model.ans"
                  )

model2

# Check for time heterogeneity
tt_2 <- sienaTimeTest(model2)
summary(tt_2)

# Add ego effect time dummy for senator 
eff2 <- includeTimeDummy(eff2, egoX, interaction1 = "senator")

# Run the model
model2 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff2,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model2,
                  modelName = "model.ans"
                  )

model2

# Check all looks ok
tt_2 <- sienaTimeTest(model2)
summary(tt_2)

# Re-run model with higher n3
model2 <- siena07(alg2, data = data,
                        effects = eff2,
                        initC = TRUE,
                        useCluster = TRUE,
                        nbrNodes = 20,
                        batch = TRUE,
                        prevAns = model2
                        )
model2

#######################################
#         SPECIFY FULL MODEL          #
#######################################

# Add effects ego, alter, same covariate (e.g., homophily), and reciprocity based on same covariate for gender and party
eff3 <- includeEffects(eff2, egoX, altX, sameX, sameXRecip, interaction1 = "female")
eff3 <- includeEffects(eff3, egoX, altX, sameX, sameXRecip, interaction1 = "democrat")

# Run full model with effects of gender and party
model3 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff3,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model2,
                  modelName = "model.ans"
                  )

model3

tt_3 <- sienaTimeTest(model3)
summary(tt_3)

# Add time dummies for democrat ego and female alter (while preference is for time dummies on ego, it was really alt that was the problem)
eff3 <- includeTimeDummy(eff3, egoX, interaction1 = "female")

model3 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff3,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model2,
                  modelName = "model.ans"
)

model3

tt_3 <- sienaTimeTest(model3)
summary(tt_3)

eff3 <- includeTimeDummy(eff3, altX, interaction1 = "female")

model3 <- siena07RunToConvergence(alg1,
                  dat = data,
                  eff = eff3,
                  initC = TRUE,
                  useCluster = TRUE,
                  nbrNodes = 20,
                  batch = TRUE,
                  verbose = FALSE,
                  silent = FALSE,
                  ans0 = model2,
                  modelName = "model.ans"
)

model3


# Re-run the model with higher n3
model3 <- siena07(alg2, data = data,
                        effects = eff3,
                        initC = TRUE,
                        useCluster = TRUE,
                        nbrNodes = 20,
                        batch = TRUE,
                        prevAns = model3
                        )
model3

# Save everything
save.image("./2-analyses/analyses.RData")

# END OF SCRIPT
