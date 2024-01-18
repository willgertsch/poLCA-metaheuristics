# simulation 3
# compare algorithms on simulated data

# test out data simulation function

dat = poLCA.simdata(
  N = 5000, # sample size
  probs = NULL, # class conditional probabilities, list of matrices form, NULL=random
  nclass = 2, # number of latent classes
  ndv = 4, # number of manifest variables
  nresp = NULL, # number of responses for each manifest variable, NULL=random
  x = NULL, # covariate data
  niv = 0, # number of covariates
  b = NULL, # covariate regression parameters
  P = NULL, # proportions of latent classes
  missval = FALSE, # add missing values?
  pctmiss = NULL # % values to be randomly dropped, NULL=random between 5 and 40
  )
dat$dat # raw data, column names Y1, Y2, ...
dat$trueclass # true latent class membership
dat$probs # class conditional response probabilities
dat$nresp # number of possible outcomes for each variable
dat$P # proportions of each class

# possible research questions:
# 1. performance at lower sample sizes
# 2. metaheuristics help when the number of manifest variables is high
# 3. identification of small latent classes
# 4. Dealing with missing data
# focus on correct latent class membership assignment
# can also provide attained log-likelihood
# main questions are 1 and 2, number of latent classes should be considered
# another thing to consider is misspecification of the number of latent classes

#data_sizes = c(118, 319, 1785, 1202, 216)
sample_sizes = seq(50, 2000, by = 50)
#data_manifest_vars = c(7, 5, 17, 4, 4)
num_vars = seq(4, 20)
#data_classes = c(4, 2, 3, 4, 3)
classes = c(2, 3, 4)

Nsim = 1
length(sample_sizes)*length(num_vars)*length(classes)*Nsim

sim.results = as.data.frame(expand.grid(sample_sizes, num_vars, classes, 1:Nsim))
colnames(sim.results) = c('sample_size', 'num_vars', 'latent_classes', 'Nsim')
sim.results$em_ll = NA
sim.results$em_gwo_ll = NA
sim.results$em_hs_ll = NA
sim.results$em_de_ll = NA
sim.results$hs_ll = NA
sim.results

library(foreach)
ncores = parallel::detectCores()-2
my.cluster = parallel::makeCluster(
  ncores,
  type = "FORK"
)
doParallel::registerDoParallel(cl = my.cluster)

x = foreach(
  i = 1:nrow(sim.results),
  .combine = 'rbind'
) %dopar% {
  
  cat("Running ", i, '/', nrow(sim.results), '\n')
  
  # generate data
  set.seed(i)
  dat = poLCA.simdata(
    N = sim.results[i, 1], # sample size
    probs = NULL, # class conditional probabilities, list of matrices form, NULL=random
    nclass = sim.results[i, 3], # number of latent classes
    ndv = sim.results[i, 2], # number of manifest variables
    nresp = NULL, # number of responses for each manifest variable, NULL=random
    x = NULL, # covariate data
    niv = 0, # number of covariates
    b = NULL, # covariate regression parameters
    P = NULL, # proportions of latent classes
    missval = FALSE, # add missing values?
    pctmiss = NULL # % values to be randomly dropped, NULL=random between 5 and 40
  )
  dat = dat$dat
  
  # fit all algorithms to generated data
  # use same seed for each algorithm
  # formula for generated data
  f = as.formula(paste0('cbind(', paste(colnames(dat), collapse=','), ')~1'))
  
  sim.results$em_ll[i] = poLCA(
    formula = f,
    dat,
    nclass = sim.results[i, 3],
    meta_control = list(
      method = 'EM',
      swarm = 500,
      iter = 100,
      algorithm = 'DE',
      seed = i
    ),
    verbose = F
  )$llik
  
  sim.results$em_gwo_ll[i] = poLCA(
    formula = f,
    dat,
    nclass = sim.results[i, 3],
    meta_control = list(
      method = 'hybrid',
      swarm = 500,
      iter = 100,
      algorithm = 'GWO',
      seed = i
    ),
    verbose = F
  )$llik
  
  sim.results$em_hs_ll[i] = poLCA(
    formula = f,
    dat,
    nclass = sim.results[i, 3],
    meta_control = list(
      method = 'hybrid',
      swarm = 500,
      iter = 100,
      algorithm = 'HS',
      seed = i
    ),
    verbose = F
  )$llik
  
  sim.results$em_de_ll[i] = poLCA(
    formula = f,
    dat,
    nclass = sim.results[i, 3],
    meta_control = list(
      method = 'hybrid',
      swarm = 500,
      iter = 100,
      algorithm = 'DE',
      seed = i
    ),
    verbose = F
  )$llik
  
  sim.results$hs_ll = poLCA(
    formula = f,
    dat,
    nclass = sim.results[i, 3],
    meta_control = list(
      method = 'metaheuristic',
      swarm = 500,
      iter = 100,
      algorithm = 'HS',
      seed = i
    ),
    verbose = F
  )$llik
  
  # return
  sim.results[i, ]
}
