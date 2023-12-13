# simulation studies
library(dplyr)
library(ggplot2)
algorithms = c(
  "PSO",
  "ALO",
  "GWO",
  "DA",
  "FFA",
  "GA",
  "GOA",
  "HS",
  "MFO",
  "SCA",
  "WOA",
  "CLONALG",
  "DE",
  "SFL",
  "CSO",
  "ABC",
  "KH",
  "CS",
  "BA",
  "GBS",
  "BHO"
)

# simulation study 1:
# run algorithms at default settings to determine best algorithms
# try for all example data
# n=10 samples
# median ll

# investigate example data
data("carcinoma") # binary ratings of 7 pathologists for 118 slides
data("cheating") # binary student answers to questions about cheating for n=319 +GPA
data("election") # Opinions on presidential candidates + covariates
data("gss82") # attitudes towards survey taking
data("values") # tendencies towards universalistic or particularistic values

head(carcinoma)
head(cheating)
head(election)
head(gss82)
head(values)

# test algorithms on carcinoma data
# nsim: number of samples per algorithm
# default is pop=50, maxIter=500
sim1_carcinoma = function(nsim, algorithms, seed) {
  
  # based on ?carcinoma example
  # suggests 4 latent classes
  f <- cbind(A,B,C,D,E,F,G)~1
  results = matrix(data = NA, nrow = nsim, ncol = length(algorithms))
  
  for (alg in 1:length(algorithms)) {
    cat(paste0("Running ", algorithms[alg], '\n'))
    for (sim in 1:nsim) {
      
      try(
      results[sim, alg] <- poLCA(f,carcinoma,nclass=4,meta_control = list(
        method = 'metaheuristic',
        algorithm = algorithms[alg],
        seed = seed+sim # starting places are the same across algorithms
      ))$llik, TRUE)
      
      
    }
  }
  results = as.data.frame(results)
  colnames(results) = algorithms
  results
}

sim1_carcinoma_result = sim1_carcinoma(10, algorithms, 1234)
sim1_carcinoma_result
apply(sim1_carcinoma_result, 2, median, na.rm = T)

# test algorithms on cheating data
# test algorithms on election data
# test algorithms on gss82 data
# test algorithms on values data

# table of results
# plot of raw values
# take top 3 algorithms to next stage


# simulation 2:
# determine optimal values for population and iterations
# factorial experiment + fit regression model
# for top 3 algorithms only

# simulation 3:
# compare top 3 metaheuristics, EM algorithm, hybrid EM with best metaheuristic


