###########################################################
source("Base.R")

###########################################################
##### Optimization #####
library(ecr)
library(nsga2R)
library(MOEADr)

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
results.nsga2 <- ecr::nsga2(fitness.fun = testfunc_help, n.objectives = 2, n.dim = 3, lower = rep(0,3), upper = rep(1,3), mu = 100L, terminators = list(stopOnIters(1000L)))
plot(results.nsga2$pareto.front)

### SMS-EMOA
results.smsemoa <- ecr::smsemoa(fitness.fun = testfunc_help, n.objectives = 2, n.dim = 3, lower = rep(0,3), upper = rep(1,3), mu = 100L, terminators = list(stopOnIters(10000L)))
plot(results.smsemoa$pareto.front)

### MOEA/D
## 1: Prepare test problem
ZDT1 <- make_vectorized_smoof(prob.name  = "ZDT1",
                              dimensions = 30)

## 2: Set input parameters
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
                       maxiter  = 1000))
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- NULL

## 3: Run MOEA/D
results.moead <- moead(problem = problem, 
              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation, 
              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
              showpars = showpars, seed = seed)

## 4: Plot results
plot(results.moead$Y[,1], results.moead$Y[,2])


#### Test calls ####
### 2D functions
samples2D_f1 = as.data.frame(expand.grid(seq(0,20,by=1),seq(0,20,by=1)))
colnames(samples2D_f1)=c("x","y")
response2D_f1 = batch_apirequest(samples2D_f1, 1, "api-test2D")
samples2D_f1$f = response2D_f1

samples2D_f2 = as.data.frame(expand.grid(seq(0,20,by=1),seq(0,20,by=1)))
colnames(samples2D_f2)=c("x","y")
response2D_f2 = batch_apirequest(samples2D_f2, 2, "api-test2D")
samples2D_f2$f = response2D_f2

## Plotting example (3D plot)
library(plotly)

plot_ly(samples2D_f1, intensity = ~f,
        colors = colorRamp(c("blue","green", "red"))) %>%
  add_trace(x = ~x, y = ~y, z = ~f, type = 'mesh3d') %>%
  layout(title="Function Landscape Approximation",
         scene = list(xaxis = list(title="X"),
                      yaxis = list(title="Y"),
                      zaxis = list(title="Function Value")))

plot_ly(samples2D_f2, intensity = ~f,
        colors = colorRamp(c("blue","green", "red"))) %>%
  add_trace(x = ~x, y = ~y, z = ~f, type = 'mesh3d') %>%
  layout(title="Function Landscape Approximation",
         scene = list(xaxis = list(title="X"),
                      yaxis = list(title="Y"),
                      zaxis = list(title="Function Value")))

### 3D functions
samples3D_f1 = as.data.frame(expand.grid(seq(0,20,by=2),seq(0,20,by=2),seq(0,20,by=2)))
colnames(samples3D_f1)=c("x","y","z")
response3D_f1 = batch_apirequest(samples3D_f1, 1, "api-test3D")
samples3D_f1$f = response3D_f1

samples3D_f2 = as.data.frame(expand.grid(seq(0,20,by=2),seq(0,20,by=2),seq(0,20,by=2)))
colnames(samples3D_f2)=c("x","y","z")
response3D_f2 = batch_apirequest(samples3D_f2, 2, "api-test3D")
samples3D_f2$f = response3D_f2

### Models for the 2D-functions
library(mlr)
library(e1071)
## Create the task
task.2D_f1 <- makeRegrTask(data = samples2D_f1, target = "f")

## Create learner and train model
lrn.2D_f1 <- makeLearner("regr.svm")
mdl.svm.2D_f1 <- train(learner = lrn.2D_f1, task = task.2D_f1)

## Make predictions
new_samples2D_f1 = data.frame(1.5,1.5)
colnames(new_samples2D_f1)=c("x","y")
new_samples2D_f1$f = 0

pred.svm.2D_f1 <- predict(object = mdl.svm.2D_f1, newdata = new_samples2D_f1)
pred.svm.2D_f1$data$response
