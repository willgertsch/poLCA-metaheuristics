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
# default is pop=50, maxIter=500

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
apply(sim1_carcinoma_result, 2, median, na.rm = T) %>% sort() # DE, GWO, HS
which.max(apply(sim1_carcinoma_result, 2, median, na.rm = T))

# test algorithms on cheating data
sim1_cheating = function(nsim, algorithms, seed) {
  
  f <- cbind(LIEEXAM,LIEPAPER,FRAUD,COPYEXAM)~1
  results = matrix(data = NA, nrow = nsim, ncol = length(algorithms))
  
  for (alg in 1:length(algorithms)) {
    cat(paste0("Running ", algorithms[alg], '\n'))
    for (sim in 1:nsim) {
      
      try(
        results[sim, alg] <- poLCA(f,cheating,nclass=2,meta_control = list(
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

sim1_cheating_result = sim1_cheating(10, algorithms, 1234)
sim1_cheating_result
apply(sim1_cheating_result, 2, median, na.rm = T)  %>% sort() # PSO, MFO, WOA
which.max(apply(sim1_cheating_result, 2, median, na.rm = T))

# test algorithms on election data
sim1_election = function(nsim, algorithms, seed) {
  
  f <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
             MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~1
  results = matrix(data = NA, nrow = nsim, ncol = length(algorithms))
  
  for (alg in 1:length(algorithms)) {
    cat(paste0("Running ", algorithms[alg], '\n'))
    for (sim in 1:nsim) {
      
      try(
        results[sim, alg] <- poLCA(f,election,nclass=3,meta_control = list(
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

sim1_election_result = sim1_election(10, algorithms, 1234)
sim1_election_result
apply(sim1_election_result, 2, median, na.rm = T) %>% sort() # WOA, GWO, HS
which.max(apply(sim1_election_result, 2, median, na.rm = T))

# test algorithms on gss82 data
sim1_gss82 = function(nsim, algorithms, seed) {
  
  f <- cbind(PURPOSE,ACCURACY,UNDERSTA,COOPERAT)~1
  results = matrix(data = NA, nrow = nsim, ncol = length(algorithms))
  
  for (alg in 1:length(algorithms)) {
    cat(paste0("Running ", algorithms[alg], '\n'))
    for (sim in 1:nsim) {
      
      try(
        results[sim, alg] <- poLCA(f,gss82,nclass=4,meta_control = list(
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

sim1_gss82_result = sim1_gss82(10, algorithms, 1234)
sim1_gss82_result
apply(sim1_gss82_result, 2, median, na.rm = T) %>% sort() # DE, GWO, HS
which.max(apply(sim1_gss82_result, 2, median, na.rm = T))

# test algorithms on values data
sim1_values = function(nsim, algorithms, seed) {
  
  f <- cbind(A,B,C,D)~1
  results = matrix(data = NA, nrow = nsim, ncol = length(algorithms))
  
  for (alg in 1:length(algorithms)) {
    cat(paste0("Running ", algorithms[alg], '\n'))
    for (sim in 1:nsim) {
      
      try(
        results[sim, alg] <- poLCA(f,values,nclass=3,meta_control = list(
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

sim1_values_result = sim1_values(10, algorithms, 1234)
sim1_values_result
apply(sim1_values_result, 2, median, na.rm = T) %>% sort() # WOA, DE, ALO
which.max(apply(sim1_values_result, 2, median, na.rm = T))

# table of results
library(dplyr)
sim1_combined_results = rbind(sim1_carcinoma_result, sim1_cheating_result,
      sim1_election_result, sim1_gss82_result, sim1_values_result) %>%
  as.data.frame() %>%
  mutate(dataset = rep(c("carcinoma", 'cheating', 'election', 'gss82',
                         'values'), each = 10))

sim1_combined_results
write.csv(sim1_combined_results, 'sim1_combined_results.csv')

sim1_combined_results_summarised = rbind(
  apply(sim1_carcinoma_result, 2, median, na.rm = T),
  apply(sim1_cheating_result, 2, median, na.rm = T),
  apply(sim1_election_result, 2, median, na.rm = T),
  apply(sim1_gss82_result, 2, median, na.rm = T),
  apply(sim1_values_result, 2, median, na.rm = T)
) %>%
  as.data.frame() %>%
  mutate(dataset = rep(c("carcinoma", 'cheating', 'election', 'gss82',
                         'values'), each = 1))
sim1_combined_results_summarised
write.csv(sim1_combined_results_summarised, 'sim1_combined_results_summarised.csv')

# which have the highest medians on each dataset?

# plot of raw values
library(tidyr)
sim1_combined_results %>%
  pivot_longer(cols = PSO:BHO, names_to = 'algorithm', values_to = 'loglikelihood') %>%
  filter(!is.na(loglikelihood)) %>%
  ggplot(aes(x =algorithm, y = loglikelihood)) +
  geom_boxplot() +
  facet_wrap(~dataset, scales = 'free')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# filter to better algorithms
# all algorithms were top 3 at some problem
sim1_combined_results %>%
  pivot_longer(cols = PSO:BHO, names_to = 'algorithm', values_to = 'loglikelihood') %>%
  filter(!is.na(loglikelihood)) %>%
  filter(algorithm %in% c('DE', 'GWO', 'HS', 'ALO', 'PSO', 'MFO', 'WOA')) %>%
  ggplot(aes(x =algorithm, y = loglikelihood)) +
  geom_point() +
  facet_wrap(~dataset, scales = 'free')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# take top algorithms to next stage
top_algorithms = c('DE', 'GWO', 'HS', 'WOA')
sim1_combined_results %>%
  pivot_longer(cols = PSO:BHO, names_to = 'algorithm', values_to = 'loglikelihood') %>%
  filter(!is.na(loglikelihood)) %>%
  filter(algorithm %in% top_algorithms) %>%
  ggplot(aes(x =algorithm, y = loglikelihood)) +
  geom_point() +
  facet_wrap(~dataset, scales = 'free')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# simulation 2:
# determine optimal values for population and iterations
# factorial experiment + fit regression model
# for top 4 algorithms only
iter = c(100, 250, 500, 750, 1000, 2000)
swarm = c(10, 20, 50, 75, 100, 200)
algorithms = c('DE', 'GWO', 'HS', 'WOA')

# nsim: number of simulations
# algorithms: selected algorithms
# iter: number of iterations
# swarm: size the swarm, always smaller than iter

sim2_carcinoma = function(nsim, algorithms, iter, swarm, seed) {
  
  # set up model
  f <- cbind(A,B,C,D,E,F,G)~1
  
  # set up results data structure
  results = expand.grid(iter, swarm, algorithms)
  results = results[rep(seq_len(nrow(results)), each = nsim), ]
  results$loglikelihood = rep(NA, nrow(results))
  colnames(results) = c('iter', 'swarm', 'algorithm', 'loglikelihood')
  
  # iterate through algorithms, iter, swarm
  for (i in 1:nrow(results)) {
    cat('Progress: ', i, '/', nrow(results), '\n')
    cat('Alg: ', as.character(results[i, 3]), ' iter: ', results[i, 1], ' swarm: ', results[i,2], '\n')
    results[i, 4] <- poLCA(f,carcinoma,nclass=4,meta_control = list(
      method = 'metaheuristic',
      algorithm = results[i, 3],
      iter = results[i, 1],
      swarm = results[i, 2],
      seed = seed+i # starting places are the same across algorithms
    ),verbose=F)$llik
  }
  
  return(results)
  
}

sim2_carcinoma_results = sim2_carcinoma(10, algorithms, iter, swarm,1234)
sim2_carcinoma_results
write.csv(sim2_carcinoma_results, 'sim2_carcinoma_results.csv')

# test plot
sim2_carcinoma_results %>%
  ggplot(aes(x = iter, y = loglikelihood, color = as.factor(swarm))) +
  geom_point()+geom_smooth(se=F)+
  facet_wrap(~algorithm) +
  labs(title = 'Carcinoma data')

# seems to suggest 500-750 iterations
# 100 swarm size seems to be good => benefits of high diversity

sim2_cheating = function(nsim, algorithms, iter, swarm, seed) {
  # set up model
  f <- cbind(LIEEXAM,LIEPAPER,FRAUD,COPYEXAM)~1
  
  # set up results data structure
  results = expand.grid(iter, swarm, algorithms)
  results$loglikelihood = rep(NA, nrow(results))
  results = results[rep(seq_len(nrow(results)), each = nsim), ]
  colnames(results) = c('iter', 'swarm', 'algorithm', 'loglikelihood')
  
  # iterate through algorithms, iter, swarm
  for (i in 1:nrow(results)) {
    cat('Progress: ', i, '/', nrow(results), '\n')
    cat('Alg: ', as.character(results[i, 3]), ' iter: ', results[i, 1], ' swarm: ', results[i,2], '\n')
    results[i, 4] <- poLCA(f,cheating,nclass=2,meta_control = list(
      method = 'metaheuristic',
      algorithm = results[i, 3],
      iter = results[i, 1],
      swarm = results[i, 2],
      seed = seed+i # starting places are the same across algorithms
    ))$llik
  }
  
  return(results)
}

sim2_cheating_results = sim2_cheating(10, algorithms, iter, swarm, 1234)
sim2_cheating_results
write.csv(sim2_cheating_results, 'sim2_cheating_results.csv')

sim2_cheating_results %>%
  ggplot(aes(x = iter, y = loglikelihood, color = as.factor(swarm))) +
  geom_point()+geom_smooth(se=F)+
  facet_wrap(~algorithm) +
  labs(title = 'Cheating data')

# fewer iterations needed:500 is done
# dimishing returns after 50 swarm size

sim2_election = function(nsim, algorithms, iter, swarm, seed) {
  # set up model
  f <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
             MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~1
  
  # set up results data structure
  results = expand.grid(iter, swarm, algorithms)
  results$loglikelihood = rep(NA, nrow(results))
  results = results[rep(seq_len(nrow(results)), each = nsim), ]
  colnames(results) = c('iter', 'swarm', 'algorithm', 'loglikelihood')
  
  # iterate through algorithms, iter, swarm
  for (i in 1:nrow(results)) {
    cat('Progress: ', i, '/', nrow(results), '\n')
    cat('Alg: ', as.character(results[i, 3]), ' iter: ', results[i, 1], ' swarm: ', results[i,2], '\n')
    results[i, 4] <- poLCA(f,election,nclass=3,meta_control = list(
      method = 'metaheuristic',
      algorithm = results[i, 3],
      iter = results[i, 1],
      swarm = results[i, 2],
      seed = seed+i # starting places are the same across algorithms
    ),verbose=F)$llik
  }
  
  return(results)
}

sim2_election_results = sim2_election(10, algorithms, iter, swarm, 1234)
sim2_election_results
write.csv(sim2_election_results, 'sim2_election_results.csv')

sim2_election_results %>%
  ggplot(aes(x = iter, y = loglikelihood, color = as.factor(swarm))) +
  geom_point()+geom_smooth(se=F)+
  facet_wrap(~algorithm) +
  labs(title = 'Election data')

# for DE need lots of iterations, low swarm size worked best?

sim2_gss82 = function(nsim, algorithms, iter, swarm, seed) {
  # set up model
  f <- cbind(PURPOSE,ACCURACY,UNDERSTA,COOPERAT)~1
  
  # set up results data structure
  results = expand.grid(iter, swarm, algorithms)
  results$loglikelihood = rep(NA, nrow(results))
  results = results[rep(seq_len(nrow(results)), each = nsim), ]
  colnames(results) = c('iter', 'swarm', 'algorithm', 'loglikelihood')
  
  # iterate through algorithms, iter, swarm
  for (i in 1:nrow(results)) {
    cat('Progress: ', i, '/', nrow(results), '\n')
    cat('Alg: ', as.character(results[i, 3]), ' iter: ', results[i, 1], ' swarm: ', results[i,2], '\n')
    results[i, 4] <- poLCA(f,gss82,nclass=4,meta_control = list(
      method = 'metaheuristic',
      algorithm = results[i, 3],
      iter = results[i, 1],
      swarm = results[i, 2],
      seed = seed+i # starting places are the same across algorithms
    ), verbose=F)$llik
  }
  
  return(results)
}

sim2_gss82_results = sim2_gss82(10, algorithms, iter, swarm, 1234)
sim2_gss82_results
write.csv(sim2_gss82_results, 'sim2_gss82_results.csv')

sim2_gss82_results %>%
  ggplot(aes(x = iter, y = loglikelihood, color = as.factor(swarm))) +
  geom_point()+geom_smooth(se=F)+
  facet_wrap(~algorithm) +
  labs(title = 'GSS82 data')

sim2_values = function(nsim, algorithms, iter, swarm, seed) {
  # set up model
  f <- cbind(A,B,C,D)~1
  
  # set up results data structure
  results = expand.grid(iter, swarm, algorithms)
  results$loglikelihood = rep(NA, nrow(results))
  results = results[rep(seq_len(nrow(results)), each = nsim), ]
  colnames(results) = c('iter', 'swarm', 'algorithm', 'loglikelihood')
  
  # iterate through algorithms, iter, swarm
  for (i in 1:nrow(results)) {
    cat('Progress: ', i, '/', nrow(results), '\n')
    cat('Alg: ', as.character(results[i, 3]), ' iter: ', results[i, 1], ' swarm: ', results[i,2], '\n')
    results[i, 4] <- poLCA(f,values,nclass=3,meta_control = list(
      method = 'metaheuristic',
      algorithm = results[i, 3],
      iter = results[i, 1],
      swarm = results[i, 2],
      seed = seed+i # starting places are the same across algorithms
    ), verbose = F)$llik
  }
  
  return(results)
}

sim2_values_results = sim2_values(10, algorithms, iter, swarm, 1234)
sim2_values_results
write.csv(sim2_values_results, 'sim2_values_results.csv')

sim2_values_results %>%
  ggplot(aes(x = iter, y = loglikelihood, color = as.factor(swarm))) +
  geom_point()+geom_smooth(se=F)+
  facet_wrap(~algorithm) +
  labs(title = 'GSS82 data')

# simulation 3:
# compare top 3 metaheuristics, EM algorithm, hybrid EM with best metaheuristic



