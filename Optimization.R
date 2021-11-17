###########################################################
library(R.filesets)
library(ecr)
library(mlr)
library(MOEADr)
library(kernlab)
library(ggplot2)
library(tidyr)
model_f1 <- loadRDS("f1_final.rds")
model_f2 <- loadRDS("f2_final.rds")

###########################################################
##### Optimization #####
opti_functions <- function(x){
  new_sample = data.frame(x[1],x[2],x[3],0)
  colnames(new_sample)=c("x","y","z","f")
  pred.f1 <- predict(object = model_f1, newdata = new_sample)
  pred.f2 <- predict(object = model_f2, newdata = new_sample)
  
  y1 <- pred.f1$data$response
  y2 <- pred.f2$data$response
  return(c(y1,y2))
}

opti_functions_moead <- function(X, ...){
  t(apply(X, MARGIN = 1, FUN = opti_functions))
}

### NSGA-II ####
# Standard settings
nsga2.results_standard_overview <- data.frame(c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100))
colnames(nsga2.results_standard_overview) <- c("r1_x1","r1_x2","r1_x3","r1_y1","r1_y2","r2_x1","r2_x2","r2_x3","r2_y1","r2_y2","r3_x1","r3_x2","r3_x3","r3_y1","r3_y2")
for(i in 1:3){
  nsga2.results <- ecr::nsga2(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                              mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                              recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                              terminators = list(stopOnIters(100L)))
  nsga2.results_standard_overview[,(i-1)*5+1] <- nsga2.results$pareto.set[[1]][1]
  nsga2.results_standard_overview[,(i-1)*5+2] <- nsga2.results$pareto.set[[1]][2]
  nsga2.results_standard_overview[,(i-1)*5+3] <- nsga2.results$pareto.set[[1]][3]
  nsga2.results_standard_overview[,(i-1)*5+4] <- nsga2.results$pareto.front$y1
  nsga2.results_standard_overview[,(i-1)*5+5] <- nsga2.results$pareto.front$y2
}

min_y1_standard <- min(min(nsga2.results_standard_overview[,4]),min(nsga2.results_standard_overview[,9]),min(nsga2.results_standard_overview[,14]))
max_y1_standard <- max(max(nsga2.results_standard_overview[,4]),max(nsga2.results_standard_overview[,9]),max(nsga2.results_standard_overview[,14]))
min_y2_standard <- min(min(nsga2.results_standard_overview[,5]),min(nsga2.results_standard_overview[,10]),min(nsga2.results_standard_overview[,15]))
max_y2_standard <- max(max(nsga2.results_standard_overview[,5]),max(nsga2.results_standard_overview[,10]),max(nsga2.results_standard_overview[,15]))

nsga2.results_standard <- ecr::nsga2(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                     mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                     recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                     terminators = list(stopOnIters(100L)))
plot(nsga2.results_standard$pareto.front, col = "red")
plot_ly(nsga2.results_standard$pareto.front,x=~y1,y=~y2,type="scatter",mode="markers",marker=list(colour = "red"))

computeHV(rbind(nsga2.results_standard$pareto.front$y1,nsga2.results_standard$pareto.front$y2), c(950000,100000))

# Best settings after parameter tuning
nsga2.results_best_overview <- data.frame(c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140),c(1:140))
colnames(nsga2.results_best_overview) <- c("r1_x1","r1_x2","r1_x3","r1_y1","r1_y2","r2_x1","r2_x2","r2_x3","r2_y1","r2_y2","r3_x1","r3_x2","r3_x3","r3_y1","r3_y2")
for(i in 1:3){
  nsga2.results <- ecr::nsga2(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                              mu = 140L, lambda = 140L, mutator = setup(mutPolynomial, eta = 25, p = 0.8, lower = rep(-5,3), upper = rep(5,3)),
                              recombinator = setup(recSBX, eta = 15, p = 0.8, lower = rep(-5,3), upper = rep(5,3)),
                              terminators = list(stopOnIters(100L)))
  for(j in 1:140){
    nsga2.results_best_overview[j,(i-1)*5+1] <- nsga2.results$pareto.set[[j]][1]
    nsga2.results_best_overview[j,(i-1)*5+2] <- nsga2.results$pareto.set[[j]][2]
    nsga2.results_best_overview[j,(i-1)*5+3] <- nsga2.results$pareto.set[[j]][3]
  }
  nsga2.results_best_overview[,(i-1)*5+4] <- nsga2.results$pareto.front$y1
  nsga2.results_best_overview[,(i-1)*5+5] <- nsga2.results$pareto.front$y2
}

min_y1_best_nsga2 <- min(min(nsga2.results_best_overview[,4]),min(nsga2.results_best_overview[,9]),min(nsga2.results_best_overview[,14]))
max_y1_best_nsga2 <- max(max(nsga2.results_best_overview[,4]),max(nsga2.results_best_overview[,9]),max(nsga2.results_best_overview[,14]))
min_y2_best_nsga2 <- min(min(nsga2.results_best_overview[,5]),min(nsga2.results_best_overview[,10]),min(nsga2.results_best_overview[,15]))
max_y2_best_nsga2 <- max(max(nsga2.results_best_overview[,5]),max(nsga2.results_best_overview[,10]),max(nsga2.results_best_overview[,15]))

# Plot both
#min_y1 <- min(min_y1_standard,min_y1_best)
#max_y1 <- max(max_y1_standard,max_y1_best)
#min_y2 <- min(min_y2_standard,min_y2_best)
#max_y2 <- max(max_y2_standard,max_y2_best)

#plot(nsga2.results_standard_overview[,1],nsga2.results_standard_overview[,2], xlim = c(min_y1,max_y1), ylim = c(min_y2,max_y2))
#points(nsga2.results_standard_overview[,3],nsga2.results_standard_overview[,4], xlim = c(min_y1,max_y1), ylim = c(min_y2,max_y2), col = "blue")
#points(nsga2.results_standard_overview[,5],nsga2.results_standard_overview[,6], xlim = c(min_y1,max_y1), ylim = c(min_y2,max_y2), col = "red")
#points(nsga2.results_standard_overview[,7],nsga2.results_standard_overview[,8], xlim = c(min_y1,max_y1), ylim = c(min_y2,max_y2), col = "green")
#points(nsga2.results_standard_overview[,9],nsga2.results_standard_overview[,10], xlim = c(min_y1,max_y1), ylim = c(min_y2,max_y2), col = "gold")

# Final solutions with NSGA-II!!!
plot(nsga2.results_best_overview[,4],nsga2.results_best_overview[,5], xlim = c(min_y1_best_nsga2,max_y1_best_nsga2), ylim = c(min_y2_best_nsga2,max_y2_best_nsga2))
points(nsga2.results_best_overview[,9],nsga2.results_best_overview[,10], xlim = c(min_y1_best_nsga2,max_y1_best_nsga2), ylim = c(min_y2_best_nsga2,max_y2_best_nsga2), col = "green")
points(nsga2.results_best_overview[,14],nsga2.results_best_overview[,15], xlim = c(min_y1_best_nsga2,max_y1_best_nsga2), ylim = c(min_y2_best_nsga2,max_y2_best_nsga2), col = "red")

## Parameter tuning round 1 ####
# Vary iterations size and plot hypervolume
iterations <- seq(20,200,by=20)
nsga2.hypervolumes_iterations_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_iterations_overview) <- c("20 iterations","40","60","80","100","120","140","160","180","200")
i <- 1
while(length(iterations)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(iterations[1])))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(950000,100000)))
  }
  nsga2.hypervolumes_iterations_overview[,i] <- hypervolumes
  
  iterations <- iterations[-1]
  i <- i+1
}
iterations_plot <- c(rep(20,10),rep(40,10),rep(60,10),rep(80,10),rep(100,10),rep(120,10),rep(140,10),rep(160,10),rep(180,10),rep(200,10))
nsga2.hypervolumes_iterations_plot <- c(nsga2.hypervolumes_iterations_overview[,1],nsga2.hypervolumes_iterations_overview[,2],nsga2.hypervolumes_iterations_overview[,3],nsga2.hypervolumes_iterations_overview[,4],nsga2.hypervolumes_iterations_overview[,5],nsga2.hypervolumes_iterations_overview[,6],nsga2.hypervolumes_iterations_overview[,7],nsga2.hypervolumes_iterations_overview[,8],nsga2.hypervolumes_iterations_overview[,9],nsga2.hypervolumes_iterations_overview[,10])
plot(iterations_plot,nsga2.hypervolumes_iterations_plot)

# Vary population size and plot hypervolume
population <- seq(20,200,by=20)
nsga2.hypervolumes_population_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_population_overview) <- c("20 population","40","60","80","100","120","140","160","180","200")
i <- 1
while(length(population)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = population[1], lambda = population[1], mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(950000,150000)))
  }
  nsga2.hypervolumes_population_overview[,i] <- hypervolumes
  
  population <- population[-1]
  i <- i+1
}
population_plot <- c(rep(20,10),rep(40,10),rep(60,10),rep(80,10),rep(100,10),rep(120,10),rep(140,10),rep(160,10),rep(180,10),rep(200,10))
nsga2.hypervolumes_population_plot <- c(nsga2.hypervolumes_population_overview[,1],nsga2.hypervolumes_population_overview[,2],nsga2.hypervolumes_population_overview[,3],nsga2.hypervolumes_population_overview[,4],nsga2.hypervolumes_population_overview[,5],nsga2.hypervolumes_population_overview[,6],nsga2.hypervolumes_population_overview[,7],nsga2.hypervolumes_population_overview[,8],nsga2.hypervolumes_population_overview[,9],nsga2.hypervolumes_population_overview[,10])
plot(population_plot,nsga2.hypervolumes_population_plot)

# Vary mutation probability and plot hypervolume
mutationprob <- seq(0.1,1,by=0.1)
nsga2.hypervolumes_mutationprob_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_mutationprob_overview) <- c("0.1 mutation probability","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
i <- 1
while(length(mutationprob)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = 140L, lambda = 140L, mutator = setup(mutPolynomial, eta = 25, p = mutationprob[1], lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(950000,150000)))
  }
  nsga2.hypervolumes_mutationprob_overview[,i] <- hypervolumes
  
  mutationprob <- mutationprob[-1]
  i <- i+1
}
mutationprob_plot <- c(rep(0.1,10),rep(0.2,10),rep(0.3,10),rep(0.4,10),rep(0.5,10),rep(0.6,10),rep(0.7,10),rep(0.8,10),rep(0.9,10),rep(1.0,10))
nsga2.hypervolumes_mutationprob_plot <- c(nsga2.hypervolumes_mutationprob_overview[,1],nsga2.hypervolumes_mutationprob_overview[,2],nsga2.hypervolumes_mutationprob_overview[,3],nsga2.hypervolumes_mutationprob_overview[,4],nsga2.hypervolumes_mutationprob_overview[,5],nsga2.hypervolumes_mutationprob_overview[,6],nsga2.hypervolumes_mutationprob_overview[,7],nsga2.hypervolumes_mutationprob_overview[,8],nsga2.hypervolumes_mutationprob_overview[,9],nsga2.hypervolumes_mutationprob_overview[,10])
plot(mutationprob_plot,nsga2.hypervolumes_mutationprob_plot)

# Vary crossover rate and plot hypervolume
crossoverrate <- seq(0.1,1,by=0.1)
nsga2.hypervolumes_crossoverrate_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_crossoverrate_overview) <- c("0.1 crossover rate","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
i <- 1
while(length(crossoverrate)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = 140L, lambda = 140L, mutator = setup(mutPolynomial, eta = 25, p = 0.8, lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = crossoverrate[1], lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(950000,150000)))
  }
  nsga2.hypervolumes_crossoverrate_overview[,i] <- hypervolumes
  
  crossoverrate <- crossoverrate[-1]
  i <- i+1
}
crossoverrate_plot <- c(rep(0.1,10),rep(0.2,10),rep(0.3,10),rep(0.4,10),rep(0.5,10),rep(0.6,10),rep(0.7,10),rep(0.8,10),rep(0.9,10),rep(1.0,10))
nsga2.hypervolumes_crossoverrate_plot <- c(nsga2.hypervolumes_crossoverrate_overview[,1],nsga2.hypervolumes_crossoverrate_overview[,2],nsga2.hypervolumes_crossoverrate_overview[,3],nsga2.hypervolumes_crossoverrate_overview[,4],nsga2.hypervolumes_crossoverrate_overview[,5],nsga2.hypervolumes_crossoverrate_overview[,6],nsga2.hypervolumes_crossoverrate_overview[,7],nsga2.hypervolumes_crossoverrate_overview[,8],nsga2.hypervolumes_crossoverrate_overview[,9],nsga2.hypervolumes_crossoverrate_overview[,10])
plot(crossoverrate_plot,nsga2.hypervolumes_crossoverrate_plot)

## Parameter tuning round 2 ####
# Population
population <- seq(20,200,by=20)
nsga2.hypervolumes_population_overview_round2 <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_population_overview_round2) <- c("20 population","40","60","80","100","120","140","160","180","200")
i <- 1
while(length(population)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = population[1], lambda = population[1], mutator = setup(mutPolynomial, eta = 25, p = 0.8, lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = 0.8, lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(950000,150000)))
  }
  nsga2.hypervolumes_population_overview_round2[,i] <- hypervolumes
  
  population <- population[-1]
  i <- i+1
}
population_plot_round2 <- c(rep(20,10),rep(40,10),rep(60,10),rep(80,10),rep(100,10),rep(120,10),rep(140,10),rep(160,10),rep(180,10),rep(200,10))
nsga2.hypervolumes_population_plot_round2 <- c(nsga2.hypervolumes_population_overview_round2[,1],nsga2.hypervolumes_population_overview_round2[,2],nsga2.hypervolumes_population_overview_round2[,3],nsga2.hypervolumes_population_overview_round2[,4],nsga2.hypervolumes_population_overview_round2[,5],nsga2.hypervolumes_population_overview_round2[,6],nsga2.hypervolumes_population_overview_round2[,7],nsga2.hypervolumes_population_overview_round2[,8],nsga2.hypervolumes_population_overview_round2[,9],nsga2.hypervolumes_population_overview_round2[,10])
plot(population_plot_round2,nsga2.hypervolumes_population_plot_round2)
# Still 140 as one of the best results, so no further changes

# Vary mutation probability and plot hypervolume
mutationprob <- seq(0.1,1,by=0.1)
nsga2.hypervolumes_mutationprob_overview_round2 <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_mutationprob_overview_round2) <- c("0.1 mutation probability","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
i <- 1
while(length(mutationprob)>0){
  hypervolumes <- c()
  for(j in 1:10){
    nsga2.results <- ecr::nsga2(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                mu = 140L, lambda = 140L, mutator = setup(mutPolynomial, eta = 25, p = mutationprob[1], lower = rep(-5,3), upper = rep(5,3)),
                                recombinator = setup(recSBX, eta = 15, p = 0.8, lower = rep(-5,3), upper = rep(5,3)),
                                terminators = list(stopOnIters(100L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(nsga2.results$pareto.front$y1,nsga2.results$pareto.front$y2), c(950000,150000)))
  }
  nsga2.hypervolumes_mutationprob_overview_round2[,i] <- hypervolumes
  
  mutationprob <- mutationprob[-1]
  i <- i+1
}
mutationprob_plot_round2 <- c(rep(0.1,10),rep(0.2,10),rep(0.3,10),rep(0.4,10),rep(0.5,10),rep(0.6,10),rep(0.7,10),rep(0.8,10),rep(0.9,10),rep(1.0,10))
nsga2.hypervolumes_mutationprob_plot_round2 <- c(nsga2.hypervolumes_mutationprob_overview_round2[,1],nsga2.hypervolumes_mutationprob_overview_round2[,2],nsga2.hypervolumes_mutationprob_overview_round2[,3],nsga2.hypervolumes_mutationprob_overview_round2[,4],nsga2.hypervolumes_mutationprob_overview_round2[,5],nsga2.hypervolumes_mutationprob_overview_round2[,6],nsga2.hypervolumes_mutationprob_overview_round2[,7],nsga2.hypervolumes_mutationprob_overview_round2[,8],nsga2.hypervolumes_mutationprob_overview_round2[,9],nsga2.hypervolumes_mutationprob_overview_round2[,10])
plot(mutationprob_plot_round2,nsga2.hypervolumes_mutationprob_plot_round2)
# Still among the best for 0.8, so no further changes

### SMS-EMOA ####
smsemoa.results_standard <- ecr::smsemoa(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                         mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                         recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                         terminators = list(stopOnIters(20000L)))
plot(smsemoa.results_standard$pareto.front, xlim=c(280000,850000), ylim=c(-700000,-280000))
computeHV(rbind(smsemoa.results_standard$pareto.front$y1,smsemoa.results_standard$pareto.front$y2), c(950000,100000))

smsemoa.results_current_best <- ecr::smsemoa(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                             mu = 160L, lambda = 160L, mutator = setup(mutPolynomial, eta = 25, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                             recombinator = setup(recSBX, eta = 15, p = 0.6, lower = rep(-5,3), upper = rep(5,3)),
                                             terminators = list(stopOnIters(12000L)))
points(smsemoa.results_current_best$pareto.front, xlim=c(280000,850000), ylim=c(-750000,-250000), col = "green")
computeHV(rbind(smsemoa.results_current_best$pareto.front$y1,smsemoa.results_current_best$pareto.front$y2), c(950000,100000))

## Parameter tuning ####
# Vary iterations size and plot hypervolume
iterations <- seq(12000,30000,by=2000)
smsemoa.hypervolumes_iterations_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(nsga2.hypervolumes_iterations_overview) <- c("12000 iterations","14000","16000","18000","20000","22000","24000","26000","28000","30000")
i <- 1
while(length(iterations)>0){
  hypervolumes <- c()
  for(j in 1:10){
    smsemoa.results <- ecr::smsemoa(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                    mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                    terminators = list(stopOnIters(iterations[1])))
    hypervolumes <- c(hypervolumes,computeHV(rbind(smsemoa.results$pareto.front$y1,smsemoa.results$pareto.front$y2), c(750000,150000)))
  }
  smsemoa.hypervolumes_iterations_overview[,i] <- hypervolumes
  
  iterations <- iterations[-1]
  i <- i+1
}
iterations_plot <- c(rep(12000,10),rep(14000,10),rep(16000,10),rep(18000,10),rep(20000,10),rep(22000,10),rep(24000,10),rep(26000,10),rep(28000,10),rep(30000,10))
smsemoa.hypervolumes_iterations_plot <- c(smsemoa.hypervolumes_iterations_overview[,1],smsemoa.hypervolumes_iterations_overview[,2],smsemoa.hypervolumes_iterations_overview[,3],smsemoa.hypervolumes_iterations_overview[,4],smsemoa.hypervolumes_iterations_overview[,5],smsemoa.hypervolumes_iterations_overview[,6],smsemoa.hypervolumes_iterations_overview[,7],smsemoa.hypervolumes_iterations_overview[,8],smsemoa.hypervolumes_iterations_overview[,9],smsemoa.hypervolumes_iterations_overview[,10])
plot(iterations_plot,smsemoa.hypervolumes_iterations_plot)

# Vary population size and plot hypervolume
population <- seq(20,200,by=20)
smsemoa.hypervolumes_population_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(smsemoa.hypervolumes_population_overview) <- c("20 population","40","60","80","100","120","140","160","180","200")
i <- 1
while(length(population)>0){
  hypervolumes <- c()
  for(j in 1:10){
    smsemoa.results <- ecr::smsemoa(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                    mu = population[1], lambda = population[1], mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                    terminators = list(stopOnIters(12000L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(smsemoa.results$pareto.front$y1,smsemoa.results$pareto.front$y2), c(750000,150000)))
  }
  smsemoa.hypervolumes_population_overview[,i] <- hypervolumes
  
  population <- population[-1]
  i <- i+1
}
population_plot <- c(rep(20,10),rep(40,10),rep(60,10),rep(80,10),rep(100,10),rep(120,10),rep(140,10),rep(160,10),rep(180,10),rep(200,10))
smsemoa.hypervolumes_population_plot <- c(smsemoa.hypervolumes_population_overview[,1],smsemoa.hypervolumes_population_overview[,2],smsemoa.hypervolumes_population_overview[,3],smsemoa.hypervolumes_population_overview[,4],smsemoa.hypervolumes_population_overview[,5],smsemoa.hypervolumes_population_overview[,6],smsemoa.hypervolumes_population_overview[,7],smsemoa.hypervolumes_population_overview[,8],smsemoa.hypervolumes_population_overview[,9],smsemoa.hypervolumes_population_overview[,10])
plot(population_plot,smsemoa.hypervolumes_population_plot)

# Vary mutation probability and plot hypervolume
mutationprob <- seq(0.1,1,by=0.1)
smsemoa.hypervolumes_mutationprob_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(smsemoa.hypervolumes_mutationprob_overview) <- c("0.1 mutation probability","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
i <- 1
while(length(mutationprob)>0){
  hypervolumes <- c()
  for(j in 1:10){
    smsemoa.results <- ecr::smsemoa(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                    mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = mutationprob[1], lower = rep(-5,3), upper = rep(5,3)),
                                    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = rep(-5,3), upper = rep(5,3)),
                                    terminators = list(stopOnIters(12000L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(smsemoa.results$pareto.front$y1,smsemoa.results$pareto.front$y2), c(750000,150000)))
  }
  smsemoa.hypervolumes_mutationprob_overview[,i] <- hypervolumes
  
  mutationprob <- mutationprob[-1]
  i <- i+1
}
mutationprob_plot <- c(rep(0.1,10),rep(0.2,10),rep(0.3,10),rep(0.4,10),rep(0.5,10),rep(0.6,10),rep(0.7,10),rep(0.8,10),rep(0.9,10),rep(1.0,10))
smsemoa.hypervolumes_mutationprob_plot <- c(smsemoa.hypervolumes_mutationprob_overview[,1],smsemoa.hypervolumes_mutationprob_overview[,2],smsemoa.hypervolumes_mutationprob_overview[,3],smsemoa.hypervolumes_mutationprob_overview[,4],smsemoa.hypervolumes_mutationprob_overview[,5],smsemoa.hypervolumes_mutationprob_overview[,6],smsemoa.hypervolumes_mutationprob_overview[,7],smsemoa.hypervolumes_mutationprob_overview[,8],smsemoa.hypervolumes_mutationprob_overview[,9],smsemoa.hypervolumes_mutationprob_overview[,10])
plot(mutationprob_plot,smsemoa.hypervolumes_mutationprob_plot)

# Vary crossover rate and plot hypervolume
crossoverrate <- seq(0.1,1,by=0.1)
smsemoa.hypervolumes_crossoverrate_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(smsemoa.hypervolumes_crossoverrate_overview) <- c("0.1 crossover rate","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0")
i <- 1
while(length(crossoverrate)>0){
  hypervolumes <- c()
  for(j in 1:10){
    smsemoa.results <- ecr::smsemoa(fitness.fun = opti_functions, n.objectives = 2, n.dim = 3, lower = rep(-5,3), upper = rep(5,3),
                                    mu = 100L, lambda = 100L, mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = rep(-5,3), upper = rep(5,3)),
                                    recombinator = setup(recSBX, eta = 15, p = crossoverrate[1], lower = rep(-5,3), upper = rep(5,3)),
                                    terminators = list(stopOnIters(12000L)))
    hypervolumes <- c(hypervolumes,computeHV(rbind(smsemoa.results$pareto.front$y1,smsemoa.results$pareto.front$y2), c(750000,150000)))
  }
  smsemoa.hypervolumes_crossoverrate_overview[,i] <- hypervolumes
  
  crossoverrate <- crossoverrate[-1]
  i <- i+1
}
crossoverrate_plot <- c(rep(0.1,10),rep(0.2,10),rep(0.3,10),rep(0.4,10),rep(0.5,10),rep(0.6,10),rep(0.7,10),rep(0.8,10),rep(0.9,10),rep(1.0,10))
smsemoa.hypervolumes_crossoverrate_plot <- c(smsemoa.hypervolumes_crossoverrate_overview[,1],smsemoa.hypervolumes_crossoverrate_overview[,2],smsemoa.hypervolumes_crossoverrate_overview[,3],smsemoa.hypervolumes_crossoverrate_overview[,4],smsemoa.hypervolumes_crossoverrate_overview[,5],smsemoa.hypervolumes_crossoverrate_overview[,6],smsemoa.hypervolumes_crossoverrate_overview[,7],smsemoa.hypervolumes_crossoverrate_overview[,8],smsemoa.hypervolumes_crossoverrate_overview[,9],smsemoa.hypervolumes_crossoverrate_overview[,10])
plot(crossoverrate_plot,smsemoa.hypervolumes_crossoverrate_plot)

### MOEA/D ####
# Standard settings
problem   <- list(name       = "opti_functions_moead",
                  xmin       = rep(-5, 3),
                  xmax       = rep(5, 3),
                  m          = 2)
decomp    <- list(name       = "sld", H = 99)
aggfun    <- list(name       = "wt")
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

moead.results_standard <- moead(problem = problem,
                                decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
                                update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
                                showpars = showpars, seed = seed)
points(moead.results_standard$Y[,1], moead.results_standard$Y[,2])

# Best settings after parameter tuning
problem   <- list(name       = "opti_functions_moead",
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

moead.results_best <- moead(problem = problem,
                            decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
                            update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
                            showpars = showpars, seed = seed)
plot(moead.results_best$Y[,1], moead.results_best$Y[,2])

moead.results_best_overview <- data.frame(c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100),c(1:100))
colnames(moead.results_best_overview) <- c("r1_x1","r1_x2","r1_x3","r1_y1","r1_y2","r2_x1","r2_x2","r2_x3","r2_y1","r2_y2","r3_x1","r3_x2","r3_x3","r3_y1","r3_y2")
for(i in 1:3){
  moead.results <- moead(problem = problem,
                         decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
                         update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
                         showpars = showpars, seed = seed)
  moead.results_best_overview[,(i-1)*5+1] <- moead.results$X[,1]
  moead.results_best_overview[,(i-1)*5+2] <- moead.results$X[,2]
  moead.results_best_overview[,(i-1)*5+3] <- moead.results$X[,3]
  moead.results_best_overview[,(i-1)*5+4] <- moead.results$Y[,1]
  moead.results_best_overview[,(i-1)*5+5] <- moead.results$Y[,2]
}

min_y1_best_both <- min(min_y1_best_nsga2,min(moead.results_best_overview[,4]),min(moead.results_best_overview[,9]),min(moead.results_best_overview[,14]))
max_y1_best_both <- max(max_y1_best_nsga2,max(moead.results_best_overview[,4]),max(moead.results_best_overview[,9]),max(moead.results_best_overview[,14]))
min_y2_best_both <- min(min_y2_best_nsga2,min(moead.results_best_overview[,5]),min(moead.results_best_overview[,10]),min(moead.results_best_overview[,15]))
max_y2_best_both <- max(max_y2_best_nsga2,max(moead.results_best_overview[,5]),max(moead.results_best_overview[,10]),max(moead.results_best_overview[,15]))

## Parameter tuning ####
## aggfun = wt
# Iterations
problem   <- list(name       = "opti_functions_moead",
                  xmin       = rep(-5, 3),
                  xmax       = rep(5, 3),
                  m          = 2)
decomp    <- list(name       = "sld", H = 99)
aggfun    <- list(name       = "wt")
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
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- NULL

iterations <- seq(20,200,by=20)
moead.hypervolumes_iterations_overview <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(moead.hypervolumes_iterations_overview) <- c("20 iterations","40","60","80","100","120","140","160","180","200")
i <- 1
while(length(iterations)>0){
  hypervolumes <- c()
  stopcrit  <- list(list(name  = "maxiter",
                         maxiter  = iterations[1]))
  for(j in 1:10){
    moead.results <- moead(problem = problem,
                           decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
                           update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
                           showpars = showpars, seed = seed)
    hypervolumes <- c(hypervolumes,computeHV(rbind(moead.results$Y[,1],moead.results$Y[,2]), c(950000,100000)))
  }
  moead.hypervolumes_iterations_overview[,i] <- hypervolumes
  
  iterations <- iterations[-1]
  i <- i+1
}
iterations_plot <- c(rep(20,10),rep(40,10),rep(60,10),rep(80,10),rep(100,10),rep(120,10),rep(140,10),rep(160,10),rep(180,10),rep(200,10))
moead.hypervolumes_iterations_plot <- c(moead.hypervolumes_iterations_overview[,1],moead.hypervolumes_iterations_overview[,2],moead.hypervolumes_iterations_overview[,3],moead.hypervolumes_iterations_overview[,4],moead.hypervolumes_iterations_overview[,5],moead.hypervolumes_iterations_overview[,6],moead.hypervolumes_iterations_overview[,7],moead.hypervolumes_iterations_overview[,8],moead.hypervolumes_iterations_overview[,9],moead.hypervolumes_iterations_overview[,10])
plot(iterations_plot,moead.hypervolumes_iterations_plot)

# delta.p
problem   <- list(name       = "opti_functions_moead",
                  xmin       = rep(-5, 3),
                  xmax       = rep(5, 3),
                  m          = 2)
decomp    <- list(name       = "sld", H = 99)
aggfun    <- list(name       = "wt")
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

delta.p <- c(0.9,0.95,1)
moead.hypervolumes_delta.p_overview <- data.frame(c(1:10),c(1:10),c(1:10))
colnames(moead.hypervolumes_delta.p_overview) <- c("0.9 delta.p","0.95","1")
i <- 1
while(length(delta.p)>0){
  hypervolumes <- c()
  neighbors <- list(name       = "lambda",
                    T          = 20,
                    delta.p    = delta.p[1])
  for(j in 1:10){
    moead.results <- moead(problem = problem,
                           decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
                           update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
                           showpars = showpars, seed = seed)
    hypervolumes <- c(hypervolumes,computeHV(rbind(moead.results$Y[,1],moead.results$Y[,2]), c(950000,100000)))
  }
  moead.hypervolumes_delta.p_overview[,i] <- hypervolumes
  
  delta.p <- delta.p[-1]
  i <- i+1
}
delta.p_plot <- c(rep(0.9,10),rep(0.95,10),rep(1,10))
moead.hypervolumes_delta.p_plot <- c(moead.hypervolumes_delta.p_overview[,1],moead.hypervolumes_delta.p_overview[,2],moead.hypervolumes_delta.p_overview[,3])
plot(delta.p_plot,moead.hypervolumes_delta.p_plot)

## aggfun = awt
# Iterations
problem   <- list(name       = "opti_functions_moead",
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
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- NULL

iterations_awt <- seq(20,200,by=20)
moead.hypervolumes_iterations_overview_awt <- data.frame(c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10),c(1:10))
colnames(moead.hypervolumes_iterations_overview_awt) <- c("20 iterations","40","60","80","100","120","140","160","180","200")
i <- 1
while(length(iterations_awt)>0){
  hypervolumes <- c()
  stopcrit  <- list(list(name  = "maxiter",
                         maxiter  = iterations_awt[1]))
  for(j in 1:10){
    moead.results <- moead(problem = problem,
                           decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
                           update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
                           showpars = showpars, seed = seed)
    hypervolumes <- c(hypervolumes,computeHV(rbind(moead.results$Y[,1],moead.results$Y[,2]), c(950000,100000)))
  }
  moead.hypervolumes_iterations_overview_awt[,i] <- hypervolumes
  
  iterations_awt <- iterations_awt[-1]
  i <- i+1
}
iterations_plot_awt <- c(rep(20,10),rep(40,10),rep(60,10),rep(80,10),rep(100,10),rep(120,10),rep(140,10),rep(160,10),rep(180,10),rep(200,10))
moead.hypervolumes_iterations_plot_awt <- c(moead.hypervolumes_iterations_overview_awt[,1],moead.hypervolumes_iterations_overview_awt[,2],moead.hypervolumes_iterations_overview_awt[,3],moead.hypervolumes_iterations_overview_awt[,4],moead.hypervolumes_iterations_overview_awt[,5],moead.hypervolumes_iterations_overview_awt[,6],moead.hypervolumes_iterations_overview_awt[,7],moead.hypervolumes_iterations_overview_awt[,8],moead.hypervolumes_iterations_overview_awt[,9],moead.hypervolumes_iterations_overview_awt[,10])
points(iterations_plot_awt,moead.hypervolumes_iterations_plot_awt, col = "red")

# delta.p
problem   <- list(name       = "opti_functions_moead",
                  xmin       = rep(-5, 3),
                  xmax       = rep(5, 3),
                  m          = 2)
decomp    <- list(name       = "sld", H = 99)
aggfun    <- list(name       = "awt")
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

delta.p_awt <- c(0.9,0.95,1)
moead.hypervolumes_delta.p_overview_awt <- data.frame(c(1:10),c(1:10),c(1:10))
colnames(moead.hypervolumes_delta.p_overview_awt) <- c("0.9 delta.p","0.95","1")
i <- 1
while(length(delta.p_awt)>0){
  hypervolumes <- c()
  neighbors <- list(name       = "lambda",
                    T          = 20,
                    delta.p    = delta.p_awt[1])
  for(j in 1:10){
    moead.results <- moead(problem = problem,
                           decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation,
                           update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
                           showpars = showpars, seed = seed)
    hypervolumes <- c(hypervolumes,computeHV(rbind(moead.results$Y[,1],moead.results$Y[,2]), c(950000,100000)))
  }
  moead.hypervolumes_delta.p_overview_awt[,i] <- hypervolumes
  
  delta.p_awt <- delta.p_awt[-1]
  i <- i+1
}
delta.p_plot_awt <- c(rep(0.9,10),rep(0.95,10),rep(1,10))
moead.hypervolumes_delta.p_plot_awt <- c(moead.hypervolumes_delta.p_overview_awt[,1],moead.hypervolumes_delta.p_overview_awt[,2],moead.hypervolumes_delta.p_overview_awt[,3])
points(delta.p_plot_awt,moead.hypervolumes_delta.p_plot_awt, col = "red")

### Get best points from solutions ####
getNonDominatedOfTwoSolutions <- function(solution1, solution2){
  sol1 <- solution1
  sol2 <- solution2
  colnames(sol1) <- c("x1","x2","x3","y1","y2")
  colnames(sol2) <- c("x1","x2","x3","y1","y2")
  index_delete_sol1 <- c()
  index_delete_sol2 <- c()
  for(i in 1:nrow(sol1)){
    for(j in 1:nrow(sol2)){
      if(sol1[i,4]>sol2[j,4] && sol1[i,5]>sol2[j,5]){
        index_delete_sol1 <- c(index_delete_sol1,i)
      }
    }
  }
  index_delete_sol1 <- unique(index_delete_sol1)
  
  for(k in 1:nrow(sol2)){
    for(l in 1:nrow(sol1)){
      if(sol2[k,4]>sol1[l,4] && sol2[k,5]>sol1[l,5]){
        index_delete_sol2 <- c(index_delete_sol2,k)
      }
    }
  }
  index_delete_sol2 <- unique(index_delete_sol2)
  
  sol1 <- sol1[-index_delete_sol1,]
  sol2 <- sol2[-index_delete_sol2,]
  
  return(rbind(sol1,sol2))
}

## Plot results from NSGA-II and MOEA/D
plot(nsga2.results_best_overview[,4],nsga2.results_best_overview[,5], xlim = c(min_y1_best_both,max_y1_best_both), ylim = c(min_y2_best_both,max_y2_best_both))
points(nsga2.results_best_overview[,9],nsga2.results_best_overview[,10], xlim = c(min_y1_best_both,max_y1_best_both), ylim = c(min_y2_best_both,max_y2_best_both), col = "green")
points(nsga2.results_best_overview[,14],nsga2.results_best_overview[,15], xlim = c(min_y1_best_both,max_y1_best_both), ylim = c(min_y2_best_both,max_y2_best_both), col = "red")

plotdata = cbind(gather(nsga2.results_best_overview[,c(4,9,14)],value="y1",key="Run") %>%
                        mutate(Run=sub("_y1","",Run)),(gather(nsga2.results_best_overview[,c(5,10,15)],value="y2") %>% mutate(key=sub("_y2","",key)) %>% select(y2)))
p1 = ggplot(plotdata) + geom_point(aes(x=y1,y=y2,col=Run),alpha=0.5) +
  theme_minimal() + labs(title="Approximated Pareto Front by NSGA-II",x="Function 1",y="Function 2") + xlim(min_y1_best_both,max_y1_best_both) + ylim(min_y2_best_both,max_y2_best_both)
p1

plot(moead.results_best_overview[,4],moead.results_best_overview[,5], xlim = c(min_y1_best_both,max_y1_best_both), ylim = c(min_y2_best_both,max_y2_best_both))
points(moead.results_best_overview[,9],moead.results_best_overview[,10], xlim = c(min_y1_best_both,max_y1_best_both), ylim = c(min_y2_best_both,max_y2_best_both), col = "green")
points(moead.results_best_overview[,14],moead.results_best_overview[,15], xlim = c(min_y1_best_both,max_y1_best_both), ylim = c(min_y2_best_both,max_y2_best_both), col = "red")

plotdata = cbind(gather(moead.results_best_overview[,c(4,9,14)],value="y1",key="Run") %>%
                   mutate(Run=sub("_y1","",Run)),(gather(moead.results_best_overview[,c(5,10,15)],value="y2") %>% mutate(key=sub("_y2","",key)) %>% select(y2)))
p2 = ggplot(plotdata) + geom_point(aes(x=y1,y=y2,col=Run),alpha=0.5) +
  theme_minimal() + labs(title="Approximated Pareto Front by MOEA/D",x="Function 1",y="Function 2") + xlim(min_y1_best_both,max_y1_best_both) + ylim(min_y2_best_both,max_y2_best_both)
p2

## Determine final Pareto front
interim_pareto <- getNonDominatedOfTwoSolutions(nsga2.results_best_overview[1:5],nsga2.results_best_overview[6:10])
interim_pareto2 <- getNonDominatedOfTwoSolutions(interim_pareto,nsga2.results_best_overview[11:15])
interim_pareto3 <- getNonDominatedOfTwoSolutions(interim_pareto2,moead.results_best_overview[1:5])
interim_pareto4 <- getNonDominatedOfTwoSolutions(interim_pareto3,moead.results_best_overview[6:10])
final_pareto <- getNonDominatedOfTwoSolutions(interim_pareto4,moead.results_best_overview[11:15])

## Select final set of 20 points for submission
final_pareto_ordered_by_y1 <- final_pareto[order(final_pareto$y1),]
rownames(final_pareto_ordered_by_y1) <- c(1:nrow(final_pareto_ordered_by_y1))
# Selection based on good distribution across the Pareto front, both for y1 and y2
final_setOf20 <- final_pareto_ordered_by_y1[c(1,16,45,74,103,136,169,202,233,264,280,316,317,341,365,389,413,437,461,470),]

plot(final_pareto[,4],final_pareto[,5], xlim = c(min(final_pareto[,4]),max(final_pareto[,4])), ylim = c(min(final_pareto[,5]),max(final_pareto[,5])))
points(final_setOf20$y1,final_setOf20$y2, col = "red", type = "p")

ggplot(final_pareto_ordered_by_y1) +
  geom_point(aes(x=y1,y=y2,color=y2)) + scale_colour_continuous() + guides(color="none") +
  theme_minimal() + labs(title="Final Approximated Pareto Front",x="Function 1",y="Function 2")
ggsave("Final_Pareto_Front.png")
