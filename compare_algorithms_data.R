# comparison of different algorithms on data
# swarm size and max iter are determined from simulation 2
test_on_data = function(method, algorithm='DE', swarm=100, iter=100, seed=1234) {
  
  mod.carcinoma = poLCA(
    formula = cbind(A,B,C,D,E,F,G)~1,
    carcinoma,
    nclass = 4,
    meta_control = list(
      method = method,
      swarm = swarm,
      iter = iter,
      algorithm = algorithm,
      seed = seed
    ),
    verbose = F
  )
  
  mod.cheating = poLCA(
    formula = cbind(LIEEXAM,LIEPAPER,FRAUD,COPYEXAM)~1,
    cheating,
    nclass = 2,
    meta_control = list(
      method = method,
      swarm = swarm,
      iter = iter,
      algorithm = algorithm,
      seed = seed
    ),
    verbose = F
  )
  
  mod.election = poLCA(
    formula = cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
               MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~1,
    election,
    nclass = 3,
    meta_control = list(
      method = method,
      swarm = swarm,
      iter = iter,
      algorithm = algorithm,
      seed = seed
    ),
    verbose = F
  )
  
  mod.gss82 = poLCA(
    formula = cbind(PURPOSE,ACCURACY,UNDERSTA,COOPERAT)~1,
    gss82,
    nclass = 4,
    meta_control = list(
      method = method,
      swarm = swarm,
      iter = iter,
      algorithm = algorithm,
      seed = seed
    ),
    verbose = F
  )
  
  mod.values = poLCA(
    formula = cbind(A,B,C,D)~1,
    values,
    nclass = 3,
    meta_control = list(
      method = method,
      swarm = swarm,
      iter = iter,
      algorithm = algorithm,
      seed = seed
    ),
    verbose = F
  )
  
  return(c(
    mod.carcinoma$llik,
    mod.cheating$llik,
    mod.election$llik,
    mod.gss82$llik,
    mod.values$llik
  ))
}

# DE: swarm=100, iter=500, what is up with the election sim 2?
# these numbers should be good in general
results.em = test_on_data('EM', seed = 1132)

results.de = test_on_data('metaheuristic', 'DE', 100, 500, 1132)
results.hs = test_on_data('metaheuristic', 'HS', 100, 500, 1132)
results.woa = test_on_data('metaheuristic', 'WOA', 100, 500, 1132)
results.gwo = test_on_data('metaheuristic', 'GWO', 100, 500, 1132)

results.de.em = test_on_data('hybrid', 'DE', 100, 500, 1132)
results.hs.em = test_on_data('hybrid', 'HS', 100, 500, 1132)
results.woa.em = test_on_data('hybrid', 'WOA', 100, 500, 1132)
results.gwo.em = test_on_data('hybrid', 'GWO', 100, 500, 1132)

library(dplyr)
library(ggplot2)
library(tidyr)
results = rbind(
  results.em,
  results.de,
  results.woa,
  results.gwo,
  results.hs.em,
  results.de.em,
  results.hs.em,
  results.woa.em,
  results.gwo.em
) %>%
  as.data.frame()
colnames(results) = c('carcinoma', 'cheating', 'election', 'gss82', 'values')
results$algorithm = c('EM', 'DE', 'WOA', 'GWO', 'HS','DE+EM', 'HS+EM', 'WOA+EM', 'GWO+EM')
write.csv(results, 'compare_on_data.csv')

results %>%
  pivot_longer(
    cols = c('carcinoma', 'cheating', 'election', 'gss82', 'values'),
    names_to = 'data',
    values_to = 'log-likelihood'
  ) %>%
  filter(!(algorithm %in% c('DE', 'WOA', 'GWO', 'WOA+EM'))) %>%
  ggplot(aes(x = algorithm, y = `log-likelihood`, color = algorithm)) +
  geom_point() +
  facet_wrap(~data, scales = 'free_y') +
  theme(axis.text.x = element_text(angle=90))

# plot shows that hybrid algorithms perform best
# but EM does well and sometimes better than the hybrid
