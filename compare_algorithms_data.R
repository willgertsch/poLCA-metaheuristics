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
test_on_data('EM', seed = 1132)
test_on_data('metaheuristic', 'DE', 100, 500, 1132)
