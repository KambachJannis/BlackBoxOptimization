###########################################################
source("Base.R")

###########################################################
##### Optimization #####
library(ecr)
library(mlr)
library(MOEADr)
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

### NSGA-II ####
nsga2.results <- ecr::nsga2(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                               mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                               recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                               terminators = list(stopOnIters(100L)))
plot(nsga2.results$pareto.front, xlim = c(-150,350), ylim = c(45,70))

plot(nsga2.results_02_07_standard$pareto.front, xlim = c(-150,350), ylim = c(45,70))
points(nsga2.results_01_09$pareto.front, xlim = c(-150,350), ylim = c(45,70), col = "gold")
points(nsga2.results_05_09$pareto.front, xlim = c(-150,350), ylim = c(45,70), col = "green")
points(nsga2.results_05_06$pareto.front, xlim = c(-150,350), ylim = c(45,70), col = "red")
points(nsga2.results_07_06$pareto.front, xlim = c(-150,350), ylim = c(45,70), col = "blue")

## Vary iterations size and plot hypervolume
iterations <- seq(10,100,by=10)
nsga2.hypervolumes_iterations_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_iterations_overview) <- c("10 iterations","20","30","40","50","60","70","80","90","100")
i <- 1
while(length(iterations)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(iterations[1])))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(350,70)))
  }
  nsga2.hypervolumes_iterations_overview[,i] <- hypervolumes
  
  iterations <- iterations[-1]
  i <- i+1
}
iterations_plot <- c(rep(10,10),rep(20,10),rep(30,10),rep(40,10),rep(50,10),rep(60,10),rep(70,10),rep(80,10),rep(90,10),rep(100,10))
nsga2.hypervolumes_iterations_plot <- c(nsga2.hypervolumes_iterations_overview[,1],nsga2.hypervolumes_iterations_overview[,2],nsga2.hypervolumes_iterations_overview[,3],nsga2.hypervolumes_iterations_overview[,4],nsga2.hypervolumes_iterations_overview[,5],nsga2.hypervolumes_iterations_overview[,6],nsga2.hypervolumes_iterations_overview[,7],nsga2.hypervolumes_iterations_overview[,8],nsga2.hypervolumes_iterations_overview[,9],nsga2.hypervolumes_iterations_overview[,10])
plot(iterations_plot,nsga2.hypervolumes_iterations_plot)

## Vary population size and plot hypervolume
population <- seq(20,200,by=20)
nsga2.hypervolumes_population_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_population_overview) <- c("20 population","40","60","80","100","120","140","160","180","200")
i <- 1
while(length(population)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = population[1], lambda = population[1], mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(350,70)))
  }
  nsga2.hypervolumes_population_overview[,i] <- hypervolumes
  
  population <- population[-1]
  i <- i+1
}
population_plot <- c(rep(20,10),rep(40,10),rep(60,10),rep(80,10),rep(100,10),rep(120,10),rep(140,10),rep(160,10),rep(180,10),rep(200,10))
nsga2.hypervolumes_population_plot <- c(nsga2.hypervolumes_population_overview[,1],nsga2.hypervolumes_population_overview[,2],nsga2.hypervolumes_population_overview[,3],nsga2.hypervolumes_population_overview[,4],nsga2.hypervolumes_population_overview[,5],nsga2.hypervolumes_population_overview[,6],nsga2.hypervolumes_population_overview[,7],nsga2.hypervolumes_population_overview[,8],nsga2.hypervolumes_population_overview[,9],nsga2.hypervolumes_population_overview[,10])
plot(population_plot,nsga2.hypervolumes_population_plot)

## Vary mutation probability and plot hypervolume
mutationprob <- seq(0.1,1,by=0.1)
nsga2.hypervolumes_mutationprob_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_mutationprob_overview) <- c("0.1 mutation probability","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
i <- 1
while(length(mutationprob)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = mutationprob[1], lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(350,70)))
  }
  nsga2.hypervolumes_mutationprob_overview[,i] <- hypervolumes
  
  mutationprob <- mutationprob[-1]
  i <- i+1
}
mutationprob_plot <- c(rep(0.1,10),rep(0.2,10),rep(0.3,10),rep(0.4,10),rep(0.5,10),rep(0.6,10),rep(0.7,10),rep(0.8,10),rep(0.9,10),rep(1.0,10))
nsga2.hypervolumes_mutationprob_plot <- c(nsga2.hypervolumes_mutationprob_overview[,1],nsga2.hypervolumes_mutationprob_overview[,2],nsga2.hypervolumes_mutationprob_overview[,3],nsga2.hypervolumes_mutationprob_overview[,4],nsga2.hypervolumes_mutationprob_overview[,5],nsga2.hypervolumes_mutationprob_overview[,6],nsga2.hypervolumes_mutationprob_overview[,7],nsga2.hypervolumes_mutationprob_overview[,8],nsga2.hypervolumes_mutationprob_overview[,9],nsga2.hypervolumes_mutationprob_overview[,10])
plot(mutationprob_plot,nsga2.hypervolumes_mutationprob_plot)

## Vary crossover rate and plot hypervolume
crossoverrate <- seq(0.1,1,by=0.1)
nsga2.hypervolumes_crossoverrate_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_crossoverrate_overview) <- c("0.1 mutation probability","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
i <- 1
while(length(crossoverrate)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = crossoverrate[1], lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(350,70)))
  }
  nsga2.hypervolumes_crossoverrate_overview[,i] <- hypervolumes
  
  crossoverrate <- crossoverrate[-1]
  i <- i+1
}
crossoverrate_plot <- c(rep(0.1,10),rep(0.2,10),rep(0.3,10),rep(0.4,10),rep(0.5,10),rep(0.6,10),rep(0.7,10),rep(0.8,10),rep(0.9,10),rep(1.0,10))
nsga2.hypervolumes_crossoverrate_plot <- c(nsga2.hypervolumes_crossoverrate_overview[,1],nsga2.hypervolumes_crossoverrate_overview[,2],nsga2.hypervolumes_crossoverrate_overview[,3],nsga2.hypervolumes_crossoverrate_overview[,4],nsga2.hypervolumes_crossoverrate_overview[,5],nsga2.hypervolumes_crossoverrate_overview[,6],nsga2.hypervolumes_crossoverrate_overview[,7],nsga2.hypervolumes_crossoverrate_overview[,8],nsga2.hypervolumes_crossoverrate_overview[,9],nsga2.hypervolumes_crossoverrate_overview[,10])
plot(crossoverrate_plot,nsga2.hypervolumes_crossoverrate_plot)

### SMS-EMOA ####
smsemoa.results <- ecr::smsemoa(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(5000L)))
points(smsemoa.results$pareto.front, xlim = c(-150,350), ylim = c(45,70))

computeHV(rbind(smsemoa.results$pareto.front$y1,smsemoa.results$pareto.front$y2), c(350,70))

## Vary iterations size and plot hypervolume
iterations <- seq(10,100,by=10)
smsemoa.hypervolumes_iterations_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(smsemoa.hypervolumes_iterations_overview) <- c("10 iterations","20","30","40","50","60","70","80","90","100")
i <- 1
while(length(iterations)>0){
  hypervolumes <- c()
  for(j in 1:10){
    smsemoa.results <- ecr::smsemoa(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                    mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                    terminators = list(stopOnIters(iterations[1])))
    hypervolumes <- c(hypervolumes,computeHV(rbind(smsemoa.results$pareto.front$y1,smsemoa.results$pareto.front$y2), c(350,70)))
  }
  smsemoa.hypervolumes_iterations_overview[,i] <- hypervolumes
  
  iterations <- iterations[-1]
  i <- i+1
}
iterations_plot <- c(rep(10,10),rep(20,10),rep(30,10),rep(40,10),rep(50,10),rep(60,10),rep(70,10),rep(80,10),rep(90,10),rep(100,10))
smsemoa.hypervolumes_iterations_plot <- c(smsemoa.hypervolumes_iterations_overview[,1],smsemoa.hypervolumes_iterations_overview[,2],smsemoa.hypervolumes_iterations_overview[,3],smsemoa.hypervolumes_iterations_overview[,4],smsemoa.hypervolumes_iterations_overview[,5],smsemoa.hypervolumes_iterations_overview[,6],smsemoa.hypervolumes_iterations_overview[,7],smsemoa.hypervolumes_iterations_overview[,8],smsemoa.hypervolumes_iterations_overview[,9],smsemoa.hypervolumes_iterations_overview[,10])
plot(iterations_plot,smsemoa.hypervolumes_iterations_plot)

## Vary population size and plot hypervolume
population <- seq(20,200,by=20)
smsemoa.hypervolumes_population_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(smsemoa.hypervolumes_population_overview) <- c("20 population","40","60","80","100","120","140","160","180","200")
i <- 1
while(length(population)>0){
  hypervolumes <- c()
  for(j in 1:10){
    smsemoa.results <- ecr::smsemoa(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                    mu = population[1], lambda = population[1], mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                    terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(smsemoa.results$pareto.front$y1,smsemoa.results$pareto.front$y2), c(350,70)))
  }
  smsemoa.hypervolumes_population_overview[,i] <- hypervolumes
  
  population <- population[-1]
  i <- i+1
}
population_plot <- c(rep(20,10),rep(40,10),rep(60,10),rep(80,10),rep(100,10),rep(120,10),rep(140,10),rep(160,10),rep(180,10),rep(200,10))
smsemoa.hypervolumes_population_plot <- c(smsemoa.hypervolumes_population_overview[,1],smsemoa.hypervolumes_population_overview[,2],smsemoa.hypervolumes_population_overview[,3],smsemoa.hypervolumes_population_overview[,4],smsemoa.hypervolumes_population_overview[,5],smsemoa.hypervolumes_population_overview[,6],smsemoa.hypervolumes_population_overview[,7],smsemoa.hypervolumes_population_overview[,8],smsemoa.hypervolumes_population_overview[,9],smsemoa.hypervolumes_population_overview[,10])
plot(population_plot,smsemoa.hypervolumes_population_plot)

## Vary mutation probability and plot hypervolume
mutationprob <- seq(0.1,1,by=0.1)
smsemoa.hypervolumes_mutationprob_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(smsemoa.hypervolumes_mutationprob_overview) <- c("0.1 mutation probability","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
i <- 1
while(length(mutationprob)>0){
  hypervolumes <- c()
  for(j in 1:10){
    smsemoa.results <- ecr::smsemoa(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                    mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = mutationprob[1], lower = rep(-5,3), upper = rep(5,3)),
                                    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                    terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(smsemoa.results$pareto.front$y1,smsemoa.results$pareto.front$y2), c(350,70)))
  }
  smsemoa.hypervolumes_mutationprob_overview[,i] <- hypervolumes
  
  mutationprob <- mutationprob[-1]
  i <- i+1
}
mutationprob_plot <- c(rep(0.1,10),rep(0.2,10),rep(0.3,10),rep(0.4,10),rep(0.5,10),rep(0.6,10),rep(0.7,10),rep(0.8,10),rep(0.9,10),rep(1.0,10))
smsemoa.hypervolumes_mutationprob_plot <- c(smsemoa.hypervolumes_mutationprob_overview[,1],smsemoa.hypervolumes_mutationprob_overview[,2],smsemoa.hypervolumes_mutationprob_overview[,3],smsemoa.hypervolumes_mutationprob_overview[,4],smsemoa.hypervolumes_mutationprob_overview[,5],smsemoa.hypervolumes_mutationprob_overview[,6],smsemoa.hypervolumes_mutationprob_overview[,7],smsemoa.hypervolumes_mutationprob_overview[,8],smsemoa.hypervolumes_mutationprob_overview[,9],smsemoa.hypervolumes_mutationprob_overview[,10])
plot(mutationprob_plot,smsemoa.hypervolumes_mutationprob_plot)

## Vary crossover rate and plot hypervolume
crossoverrate <- seq(0.1,1,by=0.1)
smsemoa.hypervolumes_crossoverrate_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(smsemoa.hypervolumes_crossoverrate_overview) <- c("0.1 mutation probability","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
i <- 1
while(length(crossoverrate)>0){
  hypervolumes <- c()
  for(j in 1:10){
    smsemoa.results <- ecr::smsemoa(fitness.fun = opti_3D_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                    mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                    recombinator = setup(recSBX, eta = 15, p = crossoverrate[1], lower = rep(-5,3), upper = rep(5,3)),
                                    terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(smsemoa.results$pareto.front$y1,smsemoa.results$pareto.front$y2), c(350,70)))
  }
  smsemoa.hypervolumes_crossoverrate_overview[,i] <- hypervolumes
  
  crossoverrate <- crossoverrate[-1]
  i <- i+1
}
crossoverrate_plot <- c(rep(0.1,10),rep(0.2,10),rep(0.3,10),rep(0.4,10),rep(0.5,10),rep(0.6,10),rep(0.7,10),rep(0.8,10),rep(0.9,10),rep(1.0,10))
smsemoa.hypervolumes_crossoverrate_plot <- c(smsemoa.hypervolumes_crossoverrate_overview[,1],smsemoa.hypervolumes_crossoverrate_overview[,2],smsemoa.hypervolumes_crossoverrate_overview[,3],smsemoa.hypervolumes_crossoverrate_overview[,4],smsemoa.hypervolumes_crossoverrate_overview[,5],smsemoa.hypervolumes_crossoverrate_overview[,6],smsemoa.hypervolumes_crossoverrate_overview[,7],smsemoa.hypervolumes_crossoverrate_overview[,8],smsemoa.hypervolumes_crossoverrate_overview[,9],smsemoa.hypervolumes_crossoverrate_overview[,10])
plot(crossoverrate_plot,smsemoa.hypervolumes_crossoverrate_plot)

### MOEA/D ####
problem   <- list(name       = "opti_3D_functions_moead",
                  xmin       = rep(-5, 3),
                  xmax       = rep(5, 3),
                  m          = 2)
decomp    <- list(name       = "sld", H = 99)
aggfun    <- list(name       = "awt")
neighbors <- list(name       = "lambda",
                  T          = 20,
                  delta.p    = 1)
variation <- list(list(name  = "sbx",
                       etax  = 20, pc = 1),
                  list(name  = "polymut",
                       etam  = 20, pm = 0.1),
                  list(name  = "truncate"))
update    <- list(name       = "standard", 
                  UseArchive = FALSE)
constraint<- list(name       = "none")
scaling   <- list(name       = "none")
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = 100L))
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- NULL

moead.results <- moead(problem = problem,
                       decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
                       update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
                       showpars = showpars, seed = seed)
plot(results_3D.moead$Y[,1], results_3D.moead$Y[,2], xlim = c(-150,350), ylim = c(45,70))

computeHV(rbind(results_3D.moead$Y[,1],results_3D.moead$Y[,2]), c(350,70))

#### Test calls ####
### 2D functions
samples2D_f1 = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(samples2D_f1)=c("x","y")
response2D_f1 = batch_apirequest(samples2D_f1, 1, "api-test2D")
samples2D_f1$f = response2D_f1

samples2D_f2 = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
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

samples_2D_both <- as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(samples_2D_both)=c("x","y")
samples_2D_both$f1 <- response2D_f1
samples_2D_both$f2 <- response2D_f2

f1f2plot(samples_2D_both,scaleit=T)

### 3D functions
samples3D_f1 = as.data.frame(expand.grid(seq(-5,5,by=2),seq(-5,5,by=2),seq(-5,5,by=2)))
colnames(samples3D_f1)=c("x","y","z")
response3D_f1 = batch_apirequest(samples3D_f1, 1, "api-test3D")
samples3D_f1$f = response3D_f1

samples3D_f2 = as.data.frame(expand.grid(seq(-5,5,by=2),seq(-5,5,by=2),seq(-5,5,by=2)))
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
