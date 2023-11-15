# test examples

# example from JOSS paper
data('carcinoma')
f = cbind(A, B, C, D, E, F, G) ~ 1
lc2 <- poLCA(f, carcinoma, nclass = 2)
lc3 <- poLCA(f, carcinoma, nclass = 3, graphs = TRUE)
lc4 <- poLCA(f, carcinoma, nclass = 4, maxiter = 5000)

# try with metaheuristics
lca = poLCA(f, carcinoma, nclass = 2, meta_control = list(
  method = 'metaheuristic',
  swarm = 100,
  iter = 1000,
  algorithm = 'DE',
  seed = 1234
))
