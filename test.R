# test examples

# examples from JOSS paper
# carcinoma
data('carcinoma')
f = cbind(A, B, C, D, E, F, G) ~ 1
lc2 <- poLCA(f, carcinoma, nclass = 2)
lc3 <- poLCA(f, carcinoma, nclass = 3, graphs = TRUE)
lc4 <- poLCA(f, carcinoma, nclass = 4, maxiter = 5000)
lc3

# testing objective function
# define objective function
# vars is vectorized probabilities
J = 7
R = 3
K.j = matrix(rep(2, 7), nrow = 1)
# unique parameters: R * sum(K.j - 1) + (R - 1)
# => should probably work with this number of parameters instead
vars = runif(sum(K.j*R), 0, 1)

# construct vp object and enforce row sum constraints
# sum of probabilities of each response within a class should be 1
# enforce by rescaling
probs = list()
start = 1
for (j in 1:J) {
  # loop through each outcome variable
  end = start + R * K.j[j] - 1
  probs[[j]] = matrix(vars[start:end], nrow = R, ncol = K.j[j]) # put in list of matrices
  probs[[j]] <- probs[[j]]/rowSums(probs[[j]])  # normalize rows
  start = end + 1
}

# create vectorized object
vp <- poLCA.vectorize(probs)

llik = sum(log(rowSums(prior * poLCA.ylik.C(vp, y))) - log(.Machine$double.xmax))
llik


# try with metaheuristics
lca = poLCA(f, carcinoma, nclass = 3, meta_control = list(
  method = 'metaheuristic',
  swarm = 100,
  iter = 1000,
  algorithm = 'PSO',
  seed = 1234
))

lca$llik
lc3$llik

# local maxima example
data('gss82')
f <- cbind(PURPOSE, ACCURACY, UNDERSTA, COOPERAT) ~ 1
gss.lc <- poLCA(f, gss82, nclass = 3, maxiter = 3000, nrep = 10, verbose = T)
gss.lc.meta = poLCA(f, gss82, nclass = 3, maxiter = 3000, meta_control = list(
  method = 'metaheuristic',
  swarm = 100,
  iter = 1000,
  algorithm = 'DE',
  seed = 1234
))
gss.lc$llik
gss.lc.meta$llik


# test hybrid
data('carcinoma')
f = cbind(A, B, C, D, E, F, G) ~ 1
lca.hybrid = poLCA(f, carcinoma, nclass = 3, meta_control = list(
  method = 'hybrid',
  swarm = 100,
  iter = 200,
  algorithm = 'HS',
  seed = 1234
))
lca.hybrid$llik

# check
lca.em = poLCA(f, carcinoma, nclass = 3, meta_control = list(
  method = 'EM',
  swarm = 100,
  iter = 200,
  algorithm = 'HS',
  seed = 1234
))

lca.em$llik
