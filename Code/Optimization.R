###########################################################
source("Base.R")

###########################################################
##### Optimization #####
library(ecr)
library(nsga2R)
library(MOEADr)
library(mlr)
library(e1071)
library(plotly)

#### Testing how this works ####
testfunc_help <- function(x){
  y1 <- 2*x[1]
  y2 <- -2*x[1]+3*x[2]+x[3]
  return(c(y1,y2))
}

testfunc <- function(X, ...){
  t(apply(X, MARGIN = 1, FUN = testfunc_help))
}

#### Optimization of the 2D test functions ####
opti_2D_functions <- function(x){
  new_samples2D = data.frame(x[1],x[2],0)
  colnames(new_samples2D)=c("x","y","f")
  pred.svm.2D_f1 <- predict(object = mdl.svm.2D_f1, newdata = new_samples2D)
  pred.svm.2D_f2 <- predict(object = mdl.svm.2D_f2, newdata = new_samples2D)
  
  y1 <- pred.svm.2D_f1$data$response
  y2 <- pred.svm.2D_f2$data$response
  return(c(y1,y2))
}

opti_2D_functions_moead <- function(X, ...){
  t(apply(X, MARGIN = 1, FUN = opti_2D_functions))
}

### NSGA-II
results_2D.nsga2 <- ecr::nsga2(fitness.fun = opti_2D_functions, n.objectives = 2, n.dim = 2, lower = rep(0,2), upper = rep(20,2), mu = 100L, terminators = list(stopOnIters(1000L)))
plot(results_2D.nsga2$pareto.front)

### SMS-EMOA
results_2D.smsemoa <- ecr::smsemoa(fitness.fun = opti_2D_functions, n.objectives = 2, n.dim = 2, lower = rep(0,2), upper = rep(20,2), mu = 100L, terminators = list(stopOnIters(10000L)))
plot(results_2D.smsemoa$pareto.front)

### MOEA/D
problem   <- list(name       = "opti_2D_functions_moead",
                  xmin       = rep(0, 2),
                  xmax       = rep(20, 2),
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

results_2D.moead <- moead(problem = problem, 
                          decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation, 
                          update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
                          showpars = showpars, seed = seed)
plot(results_2D.moead$Y[,1], results_2D.moead$Y[,2])

#### Optimization of the 3D test functions #### 
opti_3D_functions <- function(x){
  new_samples3D = data.frame(x[1],x[2],x[3],0)
  colnames(new_samples3D)=c("x","y","z","f")
  pred.svm.3D_f1 <- predict(object = mdl.svm.3D_f1, newdata = new_samples3D)
  pred.svm.3D_f2 <- predict(object = mdl.svm.3D_f2, newdata = new_samples3D)
  
  y1 <- pred.svm.3D_f1$data$response
  y2 <- pred.svm.3D_f2$data$response
  return(c(y1,y2))
}

opti_3D_functions_moead <- function(X, ...){
  t(apply(X, MARGIN = 1, FUN = opti_3D_functions))
}

### NSGA-II
results_3D.nsga2 <- ecr::nsga2(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(0,3), upper = rep(20,3), mu = 100L, terminators = list(stopOnIters(1000L)))
plot(results_3D.nsga2$pareto.front)

### SMS-EMOA
results_3D.smsemoa <- ecr::smsemoa(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(0,3), upper = rep(20,3), mu = 100L, terminators = list(stopOnIters(10000L)))
plot(results_3D.smsemoa$pareto.front)

### MOEA/D
problem   <- list(name       = "opti_3D_functions_moead",
                  xmin       = rep(0, 3),
                  xmax       = rep(20, 3),
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

results_3D.moead <- moead(problem = problem, 
              decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation, 
              update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
              showpars = showpars, seed = seed)
plot(results_3D.moead$Y[,1], results_3D.moead$Y[,2])

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

samples_2D_both <- as.data.frame(expand.grid(seq(0,20,by=1),seq(0,20,by=1)))
colnames(samples_2D_both)=c("x","y")
samples_2D_both$f1 <- response2D_f1
samples_2D_both$f2 <- response2D_f2

f1f2plot(samples_2D_both,scaleit=T)

### 3D functions
samples3D_f1 = as.data.frame(expand.grid(seq(0,20,by=2),seq(0,20,by=2),seq(0,20,by=2)))
colnames(samples3D_f1)=c("x","y","z")
response3D_f1 = batch_apirequest(samples3D_f1, 1, "api-test3D")
samples3D_f1$f = response3D_f1

samples3D_f2 = as.data.frame(expand.grid(seq(0,20,by=2),seq(0,20,by=2),seq(0,20,by=2)))
colnames(samples3D_f2)=c("x","y","z")
response3D_f2 = batch_apirequest(samples3D_f2, 2, "api-test3D")
samples3D_f2$f = response3D_f2

#### Models ####
### Models for the 2D test functions
## Create the tasks
task.2D_f1 <- makeRegrTask(data = samples2D_f1, target = "f")
task.2D_f2 <- makeRegrTask(data = samples2D_f2, target = "f")

## Create learners and train models
lrn.2D_f1 <- makeLearner("regr.svm")
mdl.svm.2D_f1 <- train(learner = lrn.2D_f1, task = task.2D_f1)
lrn.2D_f2 <- makeLearner("regr.svm")
mdl.svm.2D_f2 <- train(learner = lrn.2D_f2, task = task.2D_f2)

## Make predictions
new_samples2D = data.frame(0,0,0)
colnames(new_samples2D)=c("x","y","f")

pred.svm.2D_f1 <- predict(object = mdl.svm.2D_f1, newdata = new_samples2D)
pred.svm.2D_f1$data$response
pred.svm.2D_f2 <- predict(object = mdl.svm.2D_f2, newdata = new_samples2D)
pred.svm.2D_f2$data$response

### Models for the 3D test functions
## Create the tasks
task.3D_f1 <- makeRegrTask(data = samples3D_f1, target = "f")
task.3D_f2 <- makeRegrTask(data = samples3D_f2, target = "f")

## Create learners and train models
lrn.3D_f1 <- makeLearner("regr.svm")
mdl.svm.3D_f1 <- train(learner = lrn.3D_f1, task = task.3D_f1)
lrn.3D_f2 <- makeLearner("regr.svm")
mdl.svm.3D_f2 <- train(learner = lrn.3D_f2, task = task.3D_f2)

## Make predictions
new_samples3D = data.frame(0,0,0,0)
colnames(new_samples3D)=c("x","y","z","f")

pred.svm.3D_f1 <- predict(object = mdl.svm.3D_f1, newdata = new_samples3D)
pred.svm.3D_f1$data$response
pred.svm.3D_f2 <- predict(object = mdl.svm.3D_f2, newdata = new_samples3D)
pred.svm.3D_f2$data$response
