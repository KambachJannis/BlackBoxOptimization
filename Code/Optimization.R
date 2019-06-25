###########################################################
source("Base.R")

###########################################################
##### Optimization #####
library(ecr)
library(MOEADr)
library(smoof)

### just for testing how this works
testfunc_help <- function(x){
  y1 <- 2*x[1]
  y2 <- -2*x[1]+3*x[2]+x[3]
  return(c(y1,y2))
}

testfunc <- function(X, ...){
  t(apply(X, MARGIN = 1, FUN = testfunc_help))
}

### NSGA-II
results <- nsga2(fitness.fun=testfunc_help, n.objectives=2, n.dim=3, lower=rep(0,3), upper=rep(1,3), mu=100L, terminators=list(stopOnIters(100L)))
plot(results$pareto.front)

### SMS-EMOA
results2 <- smsemoa(fitness.fun=testfunc_help, n.objectives=2, n.dim=3, lower=rep(0,3), upper=rep(1,3), mu=100L, terminators=list(stopOnIters(100L)))
plot(results2$pareto.front)

### MOEA/D
## 1: prepare test problem
ZDT1 <- make_vectorized_smoof(prob.name  = "ZDT1",
                              dimensions = 30)

## 2: set input parameters
problem   <- list(name       = "testfunc",
                  xmin       = rep(0, 30),
                  xmax       = rep(1, 30),
                  m          = 2)
decomp    <- list(name       = "sld", H = 99)
neighbors <- list(name       = "lambda",
                  T          = 20,
                  delta.p    = 1)
aggfun    <- list(name       = "wt")
variation <- list(list(name  = "sbx",
                       etax  = 20, pc = 1),
                  list(name  = "polymut",
                       etam  = 20, pm = 0.1),
                  list(name  = "truncate"))
update    <- list(name       = "standard", 
                  UseArchive = FALSE)
scaling   <- list(name       = "none")
constraint<- list(name       = "none")
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = 200))
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- NULL

## 3: run MOEA/D
out1 <- moead(problem = problem, 
              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation, 
              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
              showpars = showpars, seed = seed)

## 4: plot results
plot(out1$Y[,1], out1$Y[,2])


# Test calls

samples = as.data.frame(expand.grid(seq(0,20,by=1),seq(0,20,by=1)))
colnames(samples)=c("x","y")
response = batch_apirequest(samples, 2, "api-test2D")
samples$f = response #the function value

# Plotting example (3D plot)
library(plotly)

plot_ly(samples, intensity = ~f,
        colors = colorRamp(c("blue","green", "red"))) %>%
  add_trace(x = ~x, y = ~y, z = ~f, type = 'mesh3d') %>%
  layout(title="Function Landscape Approximation",
         scene = list(xaxis = list(title="X"),
                      yaxis = list(title="Y"),
                      zaxis = list(title="Function Value")))

